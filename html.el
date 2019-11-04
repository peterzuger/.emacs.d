;;; html.el --- html configuration
;;; Commentary:
;;; Code:

(add-hook 'html-mode-hook
          (lambda ()
            ;; Default indentation is usually 2 spaces, changing to 4.
            (set (make-local-variable 'sgml-basic-offset) 4)))
(add-to-list 'auto-mode-alist '("handlebars" . html-mode))

;;; html.el ends here
