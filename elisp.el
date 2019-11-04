;;; elisp.el --- emacs lisp configuration
;;; Commentary:
;;; Code:

(add-hook 'emacs-lisp-mode-hook
          (lambda()
            (add-to-list 'company-backends 'company-elisp)))

;;; elisp.el ends here
