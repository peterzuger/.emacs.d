;;; python.el --- python configuration
;;; Commentary:
;;; Code:

(require 'company)
(require 'flycheck)

(add-hook 'python-mode-hook
          (lambda()
            (add-to-list 'company-backends 'company-jedi)
            (flycheck-pycheckers-setup)))

;;; python.el ends here
