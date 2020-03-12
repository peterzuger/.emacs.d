;;; ibuffer.el --- ibuffer configuration
;;; Commentary:
;;; Code:

(require 'ibuffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)                    ;; delete unmodified buffers without asking
(setq ibuffer-show-empty-filter-groups nil);; dont show empty groups

(defun git-root-dir(buf)
  "Return the git root directory of BUF, or nil."
  (when (buffer-file-name buf)
    (locate-dominating-file (buffer-file-name buf) ".git")))

(defun group-buffer-list()
  "Return an ibuffer group for all current buffers."
  (ibuffer-remove-duplicates
   (delq 'nil
         (mapcar
          (lambda(buf)
            (when (git-root-dir buf)
              (let ((name (git-root-dir buf)))
                `(,name (filename . ,(expand-file-name name))))))
          (buffer-list)))))

(add-hook 'ibuffer-hook
          (lambda()
            (ibuffer-switch-to-saved-filter-groups "default")
            (setq ibuffer-filter-groups (append ibuffer-filter-groups (group-buffer-list)))
            (let ((ibuf (get-buffer "*Ibuffer*")))
              (when ibuf
                (with-current-buffer ibuf
                  (pop-to-buffer ibuf)
                  (ibuffer-update nil t))))))

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("mu4e" (or
                        (mode . message-mode)
                        (mode . mail-mode)))))))

;;; ibuffer.el ends here
