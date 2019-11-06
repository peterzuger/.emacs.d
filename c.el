;;; c.el --- C/C++ configuration
;;; Commentary:
;;; Code:

(require 'cc-mode)
(require 'company)
(require 'irony)
(require 'make-mode)
(require 'smartparens)
(require 'yasnippet)

;; set indentation to 4 spaces
(setq c-default-style "k&r")
(setq c-basic-offset 4)
(setq irony-additional-clang-options '("-ferror-limit=0"))
(c-set-offset 'inextern-lang 0)

;; c++-mode does not know constexpr
(custom-set-variables '(c-noise-macro-names '("constexpr")))

(sp-local-pair 'c++-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
(sp-local-pair 'c-mode "{" nil :post-handlers '(("||\n[i]" "RET")))

(define-key makefile-mode-map (kbd "C-c C-l") 'compile)
(define-key c-mode-base-map (kbd "C-c C-l") 'compile)
(define-key c-mode-base-map "\C-m" 'c-context-line-break)

(add-hook 'c-mode-common-hook
          (lambda()
            (yas-minor-mode-on)
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1))
            (irony-mode)
            (flycheck-irony-setup)
            (flycheck-clang-analyzer-setup)
            (add-to-list 'company-backends 'company-irony)))

(add-hook 'irony-mode-hook
          (lambda()
            (irony-cdb-autosetup-compile-options)
            (company-irony-setup-begin-commands)))

;;; c.el ends here
