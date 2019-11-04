;;; html.el --- html configuration
;;; Commentary:
;;; Code:

(require 'sgml-mode)

(setq sgml-basic-offset 4)

(add-to-list 'auto-mode-alist '("handlebars" . html-mode))

;;; html.el ends here
