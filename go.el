;;; go.el --- golang configuration
;;; Commentary:
;;; Code:

(require 'go-mode)
(require 'company)
(require 'smartparens)
(require 'yasnippet)

(sp-local-pair 'go-mode "{" nil :post-handlers '(("||\n[i]" "RET")))

(define-key go-mode-map (kbd "C-c C-l") 'compile)

(add-hook 'go-mode-hook
          (lambda()
            (yas-minor-mode-on)
            (add-to-list 'company-backends 'company-go)))

;;; go.el ends here
