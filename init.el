;;; init.el --- emacs configuration
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

(require 'package)

;; add elpa and melpa archive
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

;; add packages for auto install
(setq package-list
      '(ace-window              ;; Quickly switch windows
        auctex                  ;; Integrated environment for *TeX*
        avy                     ;; Jump to arbitrary positions in visible text and select text quickly
        company                 ;; Modular text completion framework
        company-emoji           ;; company-mode backend for emoji
        company-go              ;; company-mode backend for Go (using gocode)
        company-irony           ;; company-mode completion back-end for irony-mode
        company-jedi            ;; company-mode completion back-end for Python JEDI
        company-shell           ;; Company mode backend for shell functions
        counsel                 ;; Various completion functions using Ivy
        dockerfile-mode         ;; Major mode for editing Docker's Dockerfiles
        engine-mode             ;; Define and query search engines from within Emacs
        emojify                 ;; Display emojis in Emacs
        fill-column-indicator   ;; Graphically indicate the fill column
        flycheck                ;; On-the-fly syntax checking
        flycheck-irony          ;; Flycheck: C/C++ support via Irony
        flycheck-pycheckers     ;; multiple syntax checker for Python, using Flycheck
        ggtags                  ;; emacs frontend to GNU Global source code tagging system
        go-mode                 ;; Major mode for the Go programming language
        highlight-indent-guides ;; Minor mode to highlight indentation
        hydra                   ;; Make bindings that stick around.
        ivy                     ;; Incremental Vertical completYon
        ivy-hydra               ;; Additional key bindings for Ivy
        js2-mode                ;; Improved JavaScript editing mode
        js2-refactor            ;; A JavaScript refactoring library for emacs.
        magit                   ;; A Git porcelain inside Emacs.
        markdown-mode           ;; Major mode for Markdown-formatted text
        org                     ;; Outline-based notes management and organizer
        org-plus-contrib        ;; Outline-based notes management and organizer
        pdf-tools               ;; Support library for PDF documents.
        pinentry                ;; GnuPG Pinentry server implementation
        python-black            ;; Reformat Python using python-black
        ranger                  ;; Make dired more like ranger
        smartparens             ;; Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
        swiper                  ;; Isearch with an overview. Oh, man!
        xref-js2                ;; Jump to references/definitions using ag & js2-mode's AST
        yasnippet))             ;; Yet another snippet extension for Emacs.
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; dont display the splash screen when a file is opened directly
(when (> (length command-line-args) 1)
  (setq inhibit-splash-screen t))

;; don't quit immediately
(when (display-graphic-p)
  (setq confirm-kill-emacs 'y-or-n-p))

;; please dont litter my init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; core emacs config
(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; start emacs in fullscreen
(setq enable-recursive-minibuffers t)                        ;; enable minibuffers inside minibuffers
(setq ring-bell-function 'ignore)                            ;; no audible bell
(setq-default indent-tabs-mode nil)                          ;; DONT EVER USE TABS !!
(setq default-tab-width 4)                                   ;; use 4 spaces
(setq tab-width 4)                                           ;; use 4 spaces
(setq compilation-scroll-output t)                           ;; scroll with the output
(setq create-lockfiles nil)                                  ;; don't create .#<filename> files
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))) ;; move backup files to ~/.emacs.d/backup
(setq backup-by-copying t)                                   ;; Don't delink hardlinks
(setq version-control t)                                     ;; Use version numbers on backups
(setq delete-old-versions t)                                 ;; Automatically delete excess backups
(setq kept-new-versions 20)                                  ;; how many of the newest versions to keep
(setq kept-old-versions 5)                                   ;; and how many of the old
(setq vc-follow-symlinks t)                                  ;; always follow symlinks
(setq help-window-select t)                                  ;; automatically select help windows
(defalias 'yes-or-no-p 'y-or-n-p)                            ;; replace yes or no prompts by y-or-n prompts

;; keybindings
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<C-f5>") 'recompile)
(global-set-key (kbd "<M-f5>") (lambda()
                                 (interactive)
                                 (when (get-buffer "*compilation*")
                                   (pop-to-buffer "*compilation*"))))
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-x C-l") 'downcase-dwim)
(global-set-key (kbd "C-x C-u") 'upcase-dwim)

(smartparens-global-mode t)                                  ;; global (){} completion
(show-smartparens-global-mode t)                             ;; gloabal (){} highlighting
(ranger-override-dired-mode nil)                             ;; use ranger instead of dired
(setq ranger-override-dired 'ranger)                         ;; use ranger not deer
(setq ranger-cleanup-eagerly t)                              ;; auto kill unused buffers
(global-flycheck-mode)                                       ;; enable flycheck globaly
(setq flycheck-checker-error-threshold 1024)                 ;; sometimes this happens
(global-emojify-mode t)
(setq highlight-indent-guides-method 'character)

(defun my-whitespace-cleanup ()
  "Clean up the whitespace in a buffer, unless that buffer is in 'fundamental-mode'."
  (unless (eq major-mode 'fundamental-mode)
    (whitespace-cleanup)))

(add-hook 'before-save-hook 'my-whitespace-cleanup)

(defun fmq-compilation-finish (buffer status)
  "Create a desktop notification on compilation finish with the STATUS.
Only creates a notification if BUFFER is *compilation*."
  (when (string= (buffer-name buffer) "*compilation*")
    (call-process "notify-send" nil nil nil
                  "-a" "emacs"
                  "-i" (format "/usr/share/emacs/%s/etc/images/icons/hicolor/32x32/apps/emacs.png" emacs-version)
                  "Compilation finished"
                  status)))

(setq compilation-finish-functions
      '(fmq-compilation-finish))

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode)

(add-hook 'after-init-hook
          (lambda()
            (global-company-mode)))
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 2)
(setq company-backends '(company-yasnippet
                         company-files
                         company-capf
                         company-keywords
                         company-emoji))

(defun defer-garbage-collection-h()
  "Defer garbage collection."
  (setq gc-cons-threshold most-positive-fixnum))

(defun restore-garbage-collection-h()
  "Defer it so that commands launched immediately after will enjoy the benefits."
  (run-at-time 1 nil
               (lambda ()
                 (setq gc-cons-threshold gc-cons-threshold-default))))

(add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'restore-garbage-collection-h)

;; load some additional configurations
(when (eq system-type 'gnu/linux)(load-file "~/.emacs.d/linux.el")) ;; Linux
(when (require 'mu4e nil 'noerror)
  (load-file "~/.emacs.d/mail.el"))     ;; Mail configuration

(load-file "~/.emacs.d/ibuffer.el")     ;; ibuffer configuration
(load-file "~/.emacs.d/magit.el")       ;; magit configuration
(load-file "~/.emacs.d/org.el")         ;; org mode configuration
(load-file "~/.emacs.d/engine.el")      ;; engine-mode configuration
(load-file "~/.emacs.d/swiper.el")      ;; swiper/ivy/counsel configuration

(load-file "~/.emacs.d/c.el")           ;; C/C++ configuration
(load-file "~/.emacs.d/elisp.el")       ;; elisp configuration
(load-file "~/.emacs.d/go.el")          ;; golang configuration
(load-file "~/.emacs.d/html.el")        ;; html configuration
(load-file "~/.emacs.d/javascript.el")  ;; javascript configuration
(load-file "~/.emacs.d/python.el")      ;; python configuration
(load-file "~/.emacs.d/shell.el")       ;; shell configuration
(load-file "~/.emacs.d/tex.el")         ;; LaTEX configuration

(load-file "~/.emacs.d/themes.el")      ;; custom themes

;;; init.el ends here
