;;; init.el --- emacs configuration -*- lexical-binding: t; -*-
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

;; dont display the splash screen when a file is opened directly
(when (> (length command-line-args) 1)
  (setq inhibit-splash-screen t))

;; don't quit immediately
(when (display-graphic-p)
  (setq confirm-kill-emacs 'y-or-n-p)
  (unbind-key "C-x C-z")
  (unbind-key "C-z"))

;; please dont litter my init.el
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file t)

;; core emacs config
(mouse-avoidance-mode 'exile)                                ;; exile the mouse cursor
(setq-default indent-tabs-mode nil)                          ;; DONT EVER USE TABS !!
(setq-default truncate-lines t)                              ;; disable line wrap
(setq-default tab-width 4)                                   ;; use 4 spaces
(setq-default fill-column 80)                                ;; use 80 characters text width
(setq global-linum-mode t)                                   ;; display line,column numbers
(setq column-number-mode t)                                  ;; "
(setq use-dialog-box nil)                                    ;; don't use dialog boxes
(setq split-height-threshold 100)                            ;; don't split horizontally
(setq initial-major-mode 'fundamental-mode)                  ;; start the scratch buffer in fundamental mode
(setq initial-scratch-message nil)                           ;; no message for the scratch buffer
(setq ring-bell-function 'ignore)                            ;; no audible bell
(setq sentence-end-double-space nil)                         ;; one space is enough
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

(require 'package)

;; add elpa and melpa archive
(setq package-archives
      '(("org"   . "http://orgmode.org/elpa/")
        ("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/lisp/use-package/")
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)


(use-package ace-window                 ;; Quickly switch windows
  :bind* ("M-o" . ace-window))

(use-package latex                      ;; Integrated environment for *TeX*
  :ensure auctex
  :demand t
  :mode ("\\.tex\\'" . TeX-latex-mode)
  :bind (:map LaTeX-mode-map
              ("C-c C-s" . pdf-sync-forward-search))
  ;; enable forward/inverse search
  :hook (LaTeX-mode . TeX-source-correlate-mode)
  ;; Update PDF buffers after successful LaTeX runs
  :hook (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
  :config
  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t))

(use-package avy)                       ;; Jump to arbitrary positions in visible text and select text quickly

(use-package cc-mode                    ;; C, C++, Objective-C, Java, CORBA IDL Pike and AWK code
  :after smartparens
  :demand t
  :ensure nil ;; builtin
  :bind (:map c-mode-base-map
              ("\C-m" . c-context-line-break))
  :config
  (setq c-default-style "k&r")
  (setq c-basic-offset 4)
  (c-set-offset 'inextern-lang 0)

  ;; c++-mode does not know constexpr
  (custom-set-variables '(c-noise-macro-names '("constexpr")))

  (sp-local-pair 'c++-mode "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair 'c-mode "{" nil :post-handlers '(("||\n[i]" "RET"))))

(use-package company                    ;; Modular text completion framework
  :demand t
  :bind* ("<backtab>" . company-yasnippet)
  :preface
  (defun company-add-local-backend (hook backend)
    "Add a local BACKEND using the given HOOK."
    (add-hook hook
              (lambda ()
                (add-to-list (make-local-variable 'company-backends) backend))))

  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)

  (setq company-backends
        '(company-files
          company-capf
          company-keywords))

  (company-add-local-backend 'emacs-lisp-mode-hook 'company-elisp)

  (global-company-mode))

(use-package company-emoji              ;; company-mode backend for emoji
  :after company
  :config
  (add-to-list 'company-backends 'company-emoji t))

(use-package company-go                 ;; company-mode backend for Go (using gocode)
  :after (company go-mode)
  :config
  (company-add-local-backend 'go-mode-hook 'company-go))

(use-package company-irony              ;; company-mode completion back-end for irony-mode
  :after (company irony)
  :demand t
  :hook (irony-mode . company-irony-setup-begin-commands)
  :config
  (company-add-local-backend 'c-mode-common-hook 'company-irony))

(use-package company-jedi               ;; company-mode completion back-end for Python JEDI
  :after (company python)
  :config
  (company-add-local-backend 'python-mode-hook 'company-jedi))

(use-package company-shell              ;; Company mode backend for shell functions
  :after company
  :config
  (company-add-local-backend 'sh-mode-hook 'company-shell))

(use-package compile                    ;; compile mode configuration
  :ensure nil ;; builtin
  :bind (("C-c C-l" . compile)
         ("C-c M-l" . pop-to-compilation))
  :preface
  (defun pop-to-compilation ()
    (interactive)
    (when (get-buffer "*compilation*")
      (pop-to-buffer "*compilation*")))

  :config
  (setq compilation-scroll-output t)

  (defun fmq-compilation-finish (buffer status)
    "Create a desktop notification on compilation finish with the STATUS.
Only creates a notification if BUFFER is *compilation*."
    (when (string= (buffer-name buffer) "*compilation*")
      (call-process "notify-send" nil nil nil
                    "-a" "emacs"
                    "-i" (format "/usr/share/emacs/%s/etc/images/icons/hicolor/32x32/apps/emacs.png" emacs-version)
                    "Compilation finished"
                    status)))

  (add-hook 'compilation-finish-functions 'fmq-compilation-finish))

(use-package counsel                    ;; Various completion functions using Ivy
  :after ivy
  :bind (("C-x C-f" . counsel-find-file)
         ("C-x C-M-f" . counsel-locate))
  :config
  (setq counsel-find-file-ignore-regexp
        (concat
         "^" (regexp-opt '("__pycache__/" "GTAGS" "GRTAGS" "GPATH")) "$"
         "\\|" (regexp-opt '(".pyc" ".elc" ".o")) "$")))

(use-package dockerfile-mode)           ;; Major mode for editing Docker's Dockerfiles

(use-package engine-mode                ;; Define and query search engines from within Emacs
  :config
  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")

  (defengine wolfram-alpha
    "http://www.wolframalpha.com/input/?i=%s")

  (engine/set-keymap-prefix (kbd "C-c s"))
  (engine-mode 1))

(use-package emojify                    ;; Display emojis in Emacs
  :config
  (global-emojify-mode t))

(use-package fill-column-indicator)     ;; Graphically indicate the fill column

(use-package flycheck                   ;; On-the-fly syntax checking
  :config
  (global-flycheck-mode)                          ;; enable flycheck globaly
  (setq flycheck-checker-error-threshold 1024))   ;; sometimes this happens

(use-package flycheck-irony             ;; Flycheck: C/C++ support via Irony
  :after (cc-mode flycheck irony)
  :hook (c-mode-common . flycheck-irony-setup))

(use-package flycheck-pycheckers        ;; multiple syntax checker for Python, using Flycheck
  :after (flycheck python)
  :hook (python-mode . flycheck-pycheckers-setup))

(use-package ggtags                     ;; emacs frontend to GNU Global source code tagging system
  :after cc-mode
  :config
  (defun enable-ggtags-mode ()
    "Enable ggtags mode for C/C++ files."
    (when (derived-mode-p 'c-mode 'c++-mode)
      (ggtags-mode 1)))

  (add-hook 'c-mode-common-hook 'enable-ggtags-mode))

(use-package go-mode                    ;; Major mode for the Go programming language
  :after (company smartparens)
  :bind (:map go-mode-map
              ("M-." . godef-jump)
              ("M-," . pop-tag-mark))
  :config
  (sp-local-pair 'go-mode "{" nil :post-handlers '(("||\n[i]" "RET")))

  (defun add-gofmt-before-save-locally ()
    "Add gofmt-before-save to the local before-save-hook."
    (add-hook 'before-save-hook 'gofmt-before-save nil t))

  (add-hook 'go-mode-hook 'add-gofmt-before-save-locally))

(use-package highlight-indent-guides    ;; Minor mode to highlight indentation
  :config
  (setq highlight-indent-guides-method 'character))

(use-package hydra)                     ;; Make bindings that stick around.

(use-package ibuffer                    ;; An advanced replacement for BufferMenu
  :ensure nil ;; builtin
  :bind* ("C-x C-b" . ibuffer)
  :config
  (setq ibuffer-expert t)                    ;; delete unmodified buffers without asking
  (setq ibuffer-show-empty-filter-groups nil);; dont show empty groups

  (defun git-root-dir (buf)
    "Return the git root directory of BUF, or nil."
    (when (buffer-file-name buf)
      (locate-dominating-file (buffer-file-name buf) ".git")))

  (defun ibuffer-make-git-filter-group (buf)
    "Return an ibuffer filter group based on BUF, or nil."
    (when-let ((name (git-root-dir buf)))
      `(, name (filename . , (expand-file-name name)))))

  (defun ibuffer-make-git-filter-groups (buffers)
    "Return ibuffer filter groups based on the git-root of BUFFERS."
    (ibuffer-remove-duplicates
     (delq 'nil
           (mapcar #'ibuffer-make-git-filter-group buffers))))

  (defun ibuffer-append-git-filter-groups ()
    "Append git filter groups to ibuffer-filter-groups and update ibuffer."
    (ibuffer-switch-to-saved-filter-groups "default")
    (setq ibuffer-filter-groups
          (append ibuffer-filter-groups
                  (ibuffer-make-git-filter-groups (buffer-list))))
    (when-let ((ibuf (get-buffer "*Ibuffer*")))
      (with-current-buffer ibuf
        (pop-to-buffer ibuf)
        (ibuffer-update nil t))))

  (add-hook 'ibuffer-hook 'ibuffer-append-git-filter-groups)

  ;; simply overwrite the default size column
  (define-ibuffer-column size
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size) 'iec))

  (setq ibuffer-saved-filter-groups
        '(("default"
           ("dired" (mode . dired-mode))
           ("emacs" (or
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*Messages\\*$")))
           ("org" (mode . org-mode))
           ("pdf" (name . "\\.pdf"))
           ("mu4e" (or
                    (mode . message-mode)
                    (mode . mail-mode)))))))

(use-package irony                      ;; C/C++ minor mode powered by libclang
  :after cc-mode
  :demand t
  :init
  (setq irony-additional-clang-options '("-ferror-limit=0"))
  :hook (c-mode-common . irony-mode)
  :hook (irony-mode . irony-cdb-autosetup-compile-options)
  :config
  (unless (file-exists-p (concat irony-user-dir "bin/irony-server"))
    (call-interactively 'irony-install-server)))

(use-package image                      ;; builtin image support
  :ensure nil ;; builtin
  :config
  (imagemagick-register-types))

(use-package ivy                        ;; Incremental Vertical completYon
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-use-selectable-prompt t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")

  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))

  (ivy-mode 1))

(use-package ivy-hydra                  ;; Additional key bindings for Ivy
  :after (ivy hydra))

(use-package js2-mode                   ;; Improved JavaScript editing mode
  :mode "\\.js\\'"
  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so unbind it.
  :bind (:map js-mode-map
              ("M-." . nil)
              ("C-k" . js2r-kill))
  :config
  (setq js-indent-level 4)

  (js2r-add-keybindings-with-prefix "C-c C-r")

  (defun js2-mode-setup ()
    "Enable js2-mode extras and xref-js2."
    (add-hook 'xref-backend-functions 'xref-js2-xref-backend nil t)
    (js2-imenu-extras-mode)
    (js2-refactor-mode))

  (add-hook 'js2-mode-hook 'js2-mode-setup))

(use-package js2-refactor               ;; A JavaScript refactoring library for emacs.
  :after js2-mode)

(use-package json-mode)                 ;; Major mode for editing JSON files

(use-package magit                      ;; A Git porcelain inside Emacs.
  :demand t
  :bind ("C-c g" . magit-status)
  :hook (git-commit-setup . git-commit-turn-on-flyspell)
  :config
  (setq transient-default-level 5)
  (setq magit-diff-refine-hunk 'all)

  (setq magit-section-initial-visibility-alist
        '((stashes . hide) (ignored . hide) (local . hide)))

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-user-header
                          'magit-insert-status-headers nil)

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-local-branches
                          'magit-insert-stashes t)

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-ignored-files
                          'magit-insert-stashes t))

(use-package markdown-mode)             ;; Major mode for Markdown-formatted text

(use-package minibuffer                 ;; Minibuffer customization
  :ensure nil ;; builtin
  :config
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)

  (defun defer-garbage-collection-h ()
    "Defer garbage collection."
    (setq gc-cons-threshold most-positive-fixnum))

  (defun restore-garbage-collection-h ()
    "Restore garbage collection."
    (setq gc-cons-threshold gc-cons-threshold-default))

  (defun restore-garbage-collection-soon-h ()
    "Defer it so that commands launched immediately after will enjoy the benefits."
    (run-at-time 1 nil #'restore-garbage-collection-h))

  (add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
  (add-hook 'minibuffer-exit-hook #'restore-garbage-collection-soon-h))

(use-package mu4e                       ;; an emacs-based e-mail client based on mu
  :when (require 'mu4e nil 'noerror)
  :ensure nil
  :bind ("C-c m" . mu4e)
  :hook (mu4e-compose-mode-hook . flyspell-mode)
  :config
  (setq mail-user-agent 'mu4e-user-agent)

  (setq mu4e-maildir       "~/Mail")            ;; top-level Maildir
  (setq mu4e-sent-folder   "/Sent Messages")    ;; folder for sent messages
  (setq mu4e-drafts-folder "/Drafts")           ;; unfinished messages
  (setq mu4e-trash-folder  "/Deleted Messages") ;; trashed messages
  (setq mu4e-refile-folder "/Archive")          ;; saved messages
  (setq mu4e-attachment-dir "~/Downloads/Mail") ;; attachments

  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"            . ?i)
           ("/Sent Messages"    . ?s)
           ("/Deleted Messages" . ?t)
           ("/Archive"          . ?a)
           ("/Drafts"           . ?d)))

  (setq mu4e-use-fancy-chars t)
  (setq mu4e-view-prefer-html t)
  (setq mu4e-view-show-images t)
  (setq mu4e-decryption-policy 'ask)
  (setq mu4e-sent-messages-behavior 'sent)
  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-update-interval  300)
  (setq mu4e-compose-signature (concat user-full-name "\n"))

  (use-package smtpmail
    :ensure nil
    :config
    (setq message-send-mail-function 'smtpmail-send-it)
    (setq message-kill-buffer-on-exit t)    ;; don't keep message buffers around
    (setq starttls-use-gnutls t)
    (setq auth-sources '("~/.config/authinfo.gpg"))
    (setq smtpmail-starttls-credentials '(("smtp.mail.me.com" 587 nil nil)))
    (setq smtpmail-auth-credentials     '(("smtp.mail.me.com" 587 user-mail-address nil)))
    (setq smtpmail-default-smtp-server     "smtp.mail.me.com")
    (setq smtpmail-smtp-server             "smtp.mail.me.com")
    (setq smtpmail-smtp-service             587)))

(use-package org                        ;; Outline-based notes management and organizer
  :ensure org-plus-contrib
  :pin org
  :bind* (("C-c c" . org-capture)
          ("C-c l" . org-store-link)
          ("C-c a" . org-agenda))
  :config
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-cycle-separator-lines 1)
  (setq org-catch-invisible-edits 'smart)
  (setq org-image-actual-width nil)
  (setq org-return-follows-link t)
  (setq org-log-into-drawer t)
  (setq org-directory "~/Notes/")
  (setq org-default-notes-file (concat org-directory "todo.org"))
  (setq org-archive-location (concat org-directory "archive.org"))
  (setq org-contacts-files (concat org-directory "contacts.org"))
  (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are done, to TODO otherwise."
    (let (org-log-done org-log-states)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  (setq org-file-apps
        '((auto-mode . emacs)))

  (setq org-capture-templates
        '(("t" "personal todo" entry (file "todo.org")
           "* TODO %?
  :LOGBOOK:
  - Created on %U
  :END:")

          ("o" "order something" entry (file "todo.org")
           "* ORDER %?
  :LOGBOOK:
  - Created on %U
  :END:")

          ("w" "work todo" entry (file "work.org")
           "* TODO %?
  :LOGBOOK:
  - Created on %U
  :END:")

          ("n" "general note" entry (file "notes.org")
           "* %?
  %U")

          ("j" "journal entry" entry (file+datetree "journal.org")
           "* %?
  %U")))

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (C . t)
     (dot . t)
     (ditaa . t)
     (emacs-lisp . t)
     (python . t))))

(use-package pdf-tools                  ;; Support library for PDF documents.
  :magic ("%PDF" . pdf-view-mode)
  :config
  ;; initialize pdf-tools
  (pdf-tools-install))

(use-package pinentry                   ;; GnuPG Pinentry server implementation
  :init
  (setq epg-pinentry-mode 'loopback)
  :config
  (pinentry-start))

(use-package python-black               ;; Reformat Python using python-black
  :after python
  :hook (python-mode . python-black-on-save-mode))

(use-package ranger                     ;; Make dired more like ranger
  :config
  (ranger-override-dired-mode nil)      ;; use ranger instead of dired
  (setq ranger-override-dired 'ranger)  ;; use ranger not deer
  (setq ranger-cleanup-eagerly t))      ;; auto kill unused buffers

(use-package sgml-mode                  ;; The simplest mode to edit XML
  :ensure nil ;; builtin
  :mode ("handlebars" . html-mode)
  :config
  (setq sgml-basic-offset 4))

(use-package smartparens                ;; Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
  :config
  (smartparens-global-mode t)           ;; global (){} completion
  (show-smartparens-global-mode t))     ;; gloabal (){} highlighting

(use-package swiper                     ;; Isearch with an overview. Oh, man!
  :after ivy
  :bind ("C-s" . swiper))

(use-package whitespace                 ;; whitespace-cleanup customization's
  :ensure nil ;; builtin
  :config
  (defvar no-whitespace-cleanup nil
    "Variable for disabling whitespace-cleanup before save.")
  (put 'no-whitespace-cleanup 'safe-local-variable #'booleanp)

  (defun my-whitespace-cleanup ()
    "Clean up the whitespace in a buffer, unless that buffer is in 'fundamental-mode'."
    (unless (or (eq major-mode 'fundamental-mode) no-whitespace-cleanup)
      (whitespace-cleanup)))

  (add-hook 'before-save-hook 'my-whitespace-cleanup))

(use-package xref-js2                   ;; Jump to references/definitions using ag & js2-mode's AST
  :after js2-mode)

(use-package yasnippet                  ;; Yet another snippet extension for Emacs.
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode))


(defun my-emacs-uptime ()
  "Get the time since this EMACS instance was started."
  (format-time-string "%s.%6Ns" (time-since before-init-time)))

(defun random-character ()
  "Return a random character."
  (let ((alnum "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"))
    (elt alnum (random (length alnum)))))

(defun random-string (length)
  "Return a random string of LENGTH."
  (let ((output ""))
    (dotimes (_ length)
      (setq output (concat output (string (random-character)))))
    output))

(defun insert-random-string (arg)
  "Insert a random string of length ARG or 5."
  (interactive "P")
  (let ((str (random-string (if (numberp arg) (abs arg) 5))))
    (kill-new str)
    (insert str)))

;; global keybindings
(global-set-key (kbd "C-x C-g") 'insert-random-string)
(global-set-key (kbd "C-x C-l") 'downcase-dwim)
(global-set-key (kbd "C-x C-u") 'upcase-dwim)


;; load some additional configurations
(when (eq system-type 'darwin)    (load-file "~/.emacs.d/mac.el" ))  ;; macOS
(when (eq system-type 'gnu/linux) (load-file "~/.emacs.d/linux.el")) ;; Linux

(load-file "~/.emacs.d/themes.el") ;; custom themes

;; log how loong emacs took to start
(message "Loading %s...done (%s)" load-file-name (my-emacs-uptime))

(add-hook 'after-init-hook
          `(lambda ()
             (message "Loading %s...done (%s) [after-init]"
                      ,load-file-name (my-emacs-uptime))) t)

;;; init.el ends here
