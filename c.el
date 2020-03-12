;;; c.el --- C/C++ configuration
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
            (add-to-list 'company-backends 'company-irony)))

(add-hook 'irony-mode-hook
          (lambda()
            (irony-cdb-autosetup-compile-options)
            (company-irony-setup-begin-commands)))

;;; c.el ends here
