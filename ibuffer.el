;;; ibuffer.el --- ibuffer configuration
;;;
;;; The MIT License (MIT)
;;;
;;; Copyright (c) 2020 Peter Züger
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.
;;;
;;; Commentary:
;;; Code:

(require 'ibuffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)                    ;; delete unmodified buffers without asking
(setq ibuffer-show-empty-filter-groups nil);; dont show empty groups

(defun git-root-dir (buf)
  "Return the git root directory of BUF, or nil."
  (when (buffer-file-name buf)
    (locate-dominating-file (buffer-file-name buf) ".git")))

(defun group-buffer-list ()
  "Return an ibuffer group for all current buffers."
  (ibuffer-remove-duplicates
   (delq 'nil
         (mapcar
          (lambda(buf)
            (when-let ((name (git-root-dir buf)))
              `(, name (filename . , (expand-file-name name)))))
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
               ("org" (mode . org-mode))
               ("mu4e" (or
                        (mode . message-mode)
                        (mode . mail-mode)))))))

;;; ibuffer.el ends here
