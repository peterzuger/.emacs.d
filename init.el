(require 'package)

;; add elpa and melpa archive
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; add packages for auto install
(setq package-list
      '(auctex                    ;;
        company                   ;;
        company-irony             ;;
        company-shell             ;;
        fill-column-indicator     ;;
        flycheck                  ;;
        flycheck-irony            ;;
        ggtags                    ;;
        jedi                      ;;
        magit                     ;;
        markdown-mode             ;;
        pdf-tools                 ;;
        pinentry                  ;;
        smartparens               ;;
        yasnippet                 ;;
        whitespace-cleanup-mode)) ;;
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; please dont litter my init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; start emacs in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq create-lockfiles nil) ;; don't create .#<filename> files
(setq global-linum-mode t)  ;; display line,column numbers
(setq column-number-mode t) ;; "
(global-flycheck-mode) ;; enable flycheck globaly
(menu-bar-mode -1)     ;; remove the menue bar
(setq ring-bell-function 'ignore)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq-default indent-tabs-mode nil)              ;; DONT EVER USE TABS !!
(smartparens-global-mode t)                      ;; global (){} completion
(show-smartparens-global-mode t)                 ;; gloabal (){} highlighting
(setq compilation-scroll-output t)               ;; scroll with the output
(tool-bar-mode -1)                               ;; remove the toolbar
(scroll-bar-mode -1)                             ;; remove the scrollbar
(add-hook 'before-save-hook 'whitespace-cleanup) ;; clean whitespace
(setq company-minimum-prefix-length 2)
(setq transient-default-level 5)
(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))
               ("mu4e" (or
                        (mode . message-mode)
                        (mode . mail-mode)))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(global-flycheck-mode)

;; enable some disabled commands
(defun downcase-char (arg)
  "Lowercasify ARG chars starting from point.  Point doesn't move."
  (interactive "p")
  (save-excursion
    (downcase-region (point) (progn (forward-char arg) (point)))))

(global-set-key "" (quote upcase-char))
(global-set-key "" (quote downcase-char))

(setq yas-snippet-dirs
  '("~/.emacs.d/snippets"))
(yas-global-mode)
(eval-after-load 'company
  (lambda()
    (global-company-mode)
    (setq company-idle-delay 0)
    (setq company-backends '(company-irony
                             company-files
                             company-capf
                             company-shell
                             company-elisp
                             company-keywords
                             company-yasnippet))))

;; move backup files to ~/.emacs.d/backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ;; Don't delink hardlinks
      version-control t      ;; Use version numbers on backups
      delete-old-versions t  ;; Automatically delete excess backups
      kept-new-versions 20   ;; how many of the newest versions to keep
      kept-old-versions 5)   ;; and how many of the old


;; load some additional configurations
(when (eq system-type 'darwin)(load-file    "~/.emacs.d/mac.el"  )) ;; Mac OS X
(when (eq system-type 'gnu/linux)(load-file "~/.emacs.d/linux.el")) ;; Linux
(load-file "~/.emacs.d/c.el")           ;; C/C++ configuration
(load-file "~/.emacs.d/elisp.el")       ;; elisp configuration
(load-file "~/.emacs.d/html.el")        ;; html configuration
(load-file "~/.emacs.d/javascript.el")  ;; javascript configuration
(load-file "~/.emacs.d/python.el")      ;; python configuration
(load-file "~/.emacs.d/org.el")         ;; org mode configuration
(load-file "~/.emacs.d/themes.el")      ;; custom themes
(load-file "~/.emacs.d/tex.el")         ;; LaTEX configuration
