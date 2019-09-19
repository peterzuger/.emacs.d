(add-hook 'shell-mode-hook
          (lambda()
            (add-to-list 'company-backends 'company-shell)))
