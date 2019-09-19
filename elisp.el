(add-hook 'emacs-lisp-mode-hook
	  (lambda()
	    (add-to-list 'company-backends 'company-elisp)))
