;;; go.el --- golang configuration
;;; Commentary:
;;; Code:

(require 'go-mode)
(require 'company)
(require 'smartparens)
(require 'yasnippet)

(sp-local-pair 'go-mode "{" nil :post-handlers '(("||\n[i]" "RET")))

(define-key go-mode-map (kbd "C-c C-l") 'compile)
(define-key go-mode-map (kbd "C-c C-f") 'gofmt)
(define-key go-mode-map (kbd "M-.") 'godef-jump)
(define-key go-mode-map (kbd "M-,") 'pop-tag-mark)

(add-hook 'go-mode-hook
          (lambda()
            (yas-minor-mode-on)
            (add-to-list 'company-backends 'company-go)
            (add-hook 'before-save-hook 'gofmt-before-save)))

;;; go.el ends here
