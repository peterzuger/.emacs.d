;;; shell.el --- sh/bash/zsh/... configuration
;;; Commentary:
;;; Code:

(require 'company)

(add-hook 'shell-mode-hook
          (lambda()
            (add-to-list 'company-backends 'company-shell)))

;;; shell.el ends here
