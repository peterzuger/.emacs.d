;; set indentation to 4 spaces
(setq c-default-style "k&r"
      c-basic-offset 4)

;; c++-mode does not know constexpr
(require 'cc-mode)
(custom-set-variables '(c-noise-macro-names '("constexpr")))

(sp-local-pair 'c++-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
(sp-local-pair 'c-mode "{" nil :post-handlers '(("||\n[i]" "RET")))

(add-hook 'c-mode-common-hook
          (lambda()
            (yas-minor-mode-on)
            (c-set-offset 'inextern-lang 0)
            (define-key c-mode-base-map (kbd "C-c C-l") 'compile)
            (when (derived-mode-p 'c-mode 'c++-mode)
              (ggtags-mode 1))
            (irony-mode)
            (setq irony-additional-clang-options '("-ferror-limit=0"))
            (flycheck-irony-setup)
            (flycheck-clang-analyzer-setup)
            (add-to-list 'company-backends 'company-irony)))

(add-hook 'makefile-mode-hook
          (lambda()
            (define-key makefile-mode-map (kbd "C-c C-l") 'compile)))

(add-hook 'irony-mode-hook
          (lambda()
            (irony-cdb-autosetup-compile-options)
            (company-irony-setup-begin-commands)))

(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)
