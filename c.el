;; set indentation to 4 spaces
(setq c-default-style "k&r"
      c-basic-offset 4)

(add-hook 'c-mode-common-hook
	  (lambda()
	    (c-set-offset 'inextern-lang 0)))

(defun my-make-CR-do-indent ()
  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
(add-hook 'c-initialization-hook 'my-make-CR-do-indent)
