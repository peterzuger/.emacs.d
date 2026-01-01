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

(when (version< emacs-version "28.2")
  (error "Emacs Version 28.2 required"))

;; don't display the splash screen when a file is opened directly
(when (> (length command-line-args) 1)
  (setq inhibit-splash-screen t))

;; don't quit immediately
(when (display-graphic-p)
  (setq confirm-kill-emacs 'y-or-n-p)
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-z")))

(eval-and-compile
  (defun emacs-path (path)
    "Expand PATH to the current `user-emacs-directory'."
    (expand-file-name path user-emacs-directory)))

;; please don't litter my init.el
(setq custom-file (emacs-path "custom.el"))
(load custom-file t)

;; core emacs config
(use-package emacs
  :ensure nil ;; pseudo package
  :demand t
  :config
  ;; C source code
  (setq-default truncate-lines t)                           ;; disable line wrap
  (setq-default tab-width 4)                                ;; use 4 spaces
  (setq-default fill-column 80)                             ;; use 80 characters text width
  (setq use-short-answers t)
  (setq use-dialog-box nil)                                 ;; don't use dialog boxes
  (setq create-lockfiles nil)                               ;; don't create .#<filename> files
  (setq message-log-max 16384)                              ;; increase max message buffer size
  (setq ring-bell-function 'ignore)                         ;; no audible bell
  (setq frame-title-format "GNU Emacs")                     ;; no dynamic title
  (setq history-length t)                                   ;; no truncation

  ;; startup.el
  (setq initial-major-mode 'fundamental-mode)               ;; start the scratch buffer in fundamental mode
  (setq initial-scratch-message nil)                        ;; no message for the scratch buffer

  ;; subr.el
  (defalias 'yes-or-no-p 'y-or-n-p)                         ;; replace yes or no prompts by y-or-n prompts

  ;; simple.el
  (size-indication-mode t)                                  ;; display the buffer size
  (column-number-mode t)                                    ;; display line,column numbers
  (setq-default indent-tabs-mode nil)                       ;; DON'T EVER USE TABS !!
  (setq kill-ring-max 16384)                                ;; large kill-ring, never loose anything
  (setq save-interprogram-paste-before-kill t)              ;; save system clipboard before overwriting

  ;; savehist.el
  (savehist-mode t)                                         ;; save minibuffer history
  (setq savehist-additional-variables                       ;; also save kill and search ring
        '(kill-ring
          search-ring
          regexp-search-ring))

  ;; paragraphs.el
  (setq sentence-end-double-space nil)                      ;; one space is enough

  ;; avoid.el
  (mouse-avoidance-mode 'cat-and-mouse)                     ;; play cat and mouse with the cursor

  ;; prog-mode.el
  (global-prettify-symbols-mode)                            ;; draw tokens as unicode glyph's

  ;; files.el
  (setq-default require-final-newline t)                    ;; default to requiring a newline
  (setq large-file-warning-threshold (* 64 1024 1024))      ;; 64MiB files are large
  (setq backup-by-copying t)                                ;; Don't delink hardlinks
  (setq version-control t)                                  ;; Use version numbers on backups
  (setq delete-old-versions t)                              ;; Automatically delete excess backups
  (setq kept-new-versions 20)                               ;; how many of the newest versions to keep
  (setq kept-old-versions 5)                                ;; and how many of the old
  (setq backup-directory-alist                              ;; move backup files to backup
        `(("." . ,(emacs-path "backup"))))
  (setq auto-save-file-name-transforms                      ;; move auto-save files to backup
        `((".*" ,(emacs-path "backup/") t))))

(eval-and-compile
  (require 'package)
  (package-initialize))

;; add elpa and melpa archive
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "http://melpa.org/packages/")))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package abbrev                     ;; abbrev mode commands for Emacs
  :ensure nil ;; builtin
  :diminish)

(use-package ace-window                 ;; Quickly switch windows
  :bind* ("M-o" . ace-window))

(use-package advice                     ;; An overloading mechanism for Emacs Lisp functions
  :ensure nil ;; builtin
  :config
  (setq ad-redefinition-action 'accept)) ;; no warning for advice redefinition

(use-package avy)                       ;; Jump to arbitrary positions in visible text and select text quickly

(use-package auth-source                ;; authentication sources for Gnus and Emacs
  :ensure nil);; builtin

(use-package auth-source-pass           ;; Integrate auth-source with password-store
  :after auth-source
  :ensure nil ;; builtin
  :init
  (setq auth-sources '(password-store)))

(use-package cc-mode                    ;; C, C++, Objective-C, Java, CORBA IDL Pike and AWK code
  :after smartparens
  :demand t
  :ensure nil ;; builtin
  :hook ((c-mode c++-mode) . hide-ifdef-mode)
  :custom (c-noise-macro-names '("constexpr"))
  :bind (:map c-mode-base-map
              ("\C-m" . c-context-line-break))
  :config
  (setq c-basic-offset 4)
  (setq c-default-style
        '((awk-mode . "awk")
          (other . "k&r")))
  (c-set-offset 'inextern-lang 0)

  (sp-local-pair '(c-mode c++-mode) "{" nil :post-handlers '(("||\n[i]" "RET"))))

(use-package cmake-mode)                ;; major-mode for editing CMake sources

(use-package company                    ;; Modular text completion framework
  :demand t
  :diminish
  :bind ("C-<tab>" . company-yasnippet)
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

  (global-company-mode))

(use-package company-emoji              ;; company-mode backend for emoji
  :after company
  :config
  (add-to-list 'company-backends 'company-emoji t))

(use-package company-shell              ;; Company mode backend for shell functions
  :after company
  :config
  (company-add-local-backend 'sh-mode-hook 'company-shell))

(use-package compile                    ;; compile mode configuration
  :ensure nil ;; builtin
  :bind* (("C-c C-l" . compile)
          ("C-c M-l" . pop-to-compilation))
  :preface
  (require 'ansi-color)
  (require 'notifications)

  (defun pop-to-compilation ()
    (interactive)
    (when (get-buffer "*compilation*")
      (pop-to-buffer "*compilation*")))

  :config
  (setq compilation-scroll-output t)

  (defvar compilation-time 0)
  (defun my-compilation-start (_)
    (setq compilation-time (current-time)))

  (defun my-compilation-finish (buffer status)
    "Create a desktop notification on compilation finish with the STATUS.
Only creates a notification if BUFFER is *compilation*."
    (when (string= (buffer-name buffer) "*compilation*")
      (notifications-notify :title "Compilation finished"
                            :body (format "%s (%s)"
                                          (string-trim status)
                                          (format-time-string "%s.%3Ns" (time-since compilation-time))))))

  (defun my-colorize-compilation-buffer ()
    "Interpret ANSI colors in the compilation buffer."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point))))

  (add-hook 'compilation-start-hook 'my-compilation-start)
  (add-hook 'compilation-finish-functions 'my-compilation-finish)
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(use-package conf-mode                  ;; Simple major mode for editing conf/ini/properties files
  :ensure nil ;; builtin
  :mode (rx
         (: "ssh" (? "d") "_config")
         string-end)
  :mode ((rx "rc" string-end) . conf-space-mode))

(use-package counsel                    ;; Various completion functions using Ivy
  :after ivy
  :bind (("C-x C-f" . counsel-find-file)
         ("C-x C-M-f" . counsel-locate))
  :config
  (setq counsel-find-file-ignore-regexp
        (rx
         (or
          (: bol (or "__pycache__/" ".mypy_cache/" "GTAGS" "GRTAGS" "GPATH"))
          (or ".pyc" ".elc" ".o" ".d" ".aux" ".synctex.gz"))
         eol)))

(use-package csv-mode                   ;; Major mode for editing comma/char separated values
  :custom (csv-separators '("," "\t" ";")))

(use-package diminish)                  ;; Diminished modes are minor modes with no modeline display

(use-package dired                      ;; directory-browsing commands
  :ensure nil ;; builtin
  :hook (dired-mode . hl-line-mode)
  :config
  (setq dired-listing-switches "-alh"))

(use-package display-fill-column-indicator ;; interface for display-fill-column-indicator
  :ensure nil ;; builtin
  :bind ("C-|" . #'display-fill-column-indicator-mode))

(use-package dockerfile-mode)           ;; Major mode for editing Docker's Dockerfiles

(use-package eldoc                      ;; Show function arglist or variable docstring in echo area
  :ensure nil ;; builtin
  :diminish)

(use-package elisp-mode                 ;; Emacs Lisp mode
  :ensure nil ;; builtin
  :after (company smartparens)
  :config
  (sp-local-pair 'emacs-lisp-mode "`" "'"))

(use-package engine-mode                ;; Define and query search engines from within Emacs
  :bind-keymap ("C-c s" . engine-mode-prefixed-map)
  :config
  (engine-mode t)

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s")

  (defengine pydocs
    "https://docs.python.org/3.9/search.html?q=%s"
    :keybinding "p")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")

  (defengine wolfram-alpha
    "http://www.wolframalpha.com/input/?i=%s"))

(use-package emojify                    ;; Display emojis in Emacs
  :hook (after-init . global-emojify-mode))

(use-package flycheck                   ;; On-the-fly syntax checking
  :init
  (global-flycheck-mode)                          ;; enable flycheck globally
  :config
  (setq flycheck-checker-error-threshold 1024))   ;; sometimes this happens

(use-package flycheck-pycheckers        ;; multiple syntax checker for Python, using Flycheck
  :after (flycheck python)
  :hook (python-mode . flycheck-pycheckers-setup)
  :config
  (setq flycheck-pycheckers-checkers '(pylint flake8 pyflakes bandit)))

(use-package flyspell                   ;; On-the-fly spell checker
  :ensure nil);; builtin

(use-package git-modes)                 ;; Major modes for editing Git configuration files

(use-package gptel                      ;; Interact with ChatGPT or other LLMs
  :functions gptel-make-tool gptel-make-preset
  :config
  (setq gptel-default-mode 'org-mode)
  (setq gptel-expert-commands t)
  (setq gptel-confirm-tool-calls t)
  (setq gptel-model 'qwen3:latest)
  (setq gptel-backend
        (gptel-make-ollama "Ollama"
                           :stream t
                           :models '(deepseek-coder-v2:latest
                                     deepseek-r1:latest
                                     gemma3:latest
                                     mistral:latest
                                     qwen3:latest)))
  )


(use-package grep                       ;; run `grep' and display the results
  :ensure nil ;; builtin
  :bind* ("C-c r" . rgrep))

(use-package gdb-mi                     ;; User Interface for running GDB
  :ensure nil ;; builtin
  :init
  (setq gdb-restore-window-configuration-after-quit t)
  (setq gdb-debuginfod-enable-setting nil)
  (setq gdb-show-main t)
  (setq gdb-use-colon-colon-notation t)
  (setq gdb-many-windows t))

(use-package haskell-mode               ;; A Haskell editing mode
  :hook (haskell-mode . interactive-haskell-mode))

(use-package help                       ;; help commands for Emacs
  :ensure nil ;; builtin
  :config
  (setq help-window-select t))          ;; automatically select help windows

(use-package hideif                     ;; hides selected code within ifdef
  :ensure nil ;; builtin
  :diminish hide-ifdef-mode)

(use-package highlight-indent-guides    ;; Minor mode to highlight indentation
  :config
  (setq highlight-indent-guides-method 'character))

(use-package hydra)                     ;; Make bindings that stick around.

(use-package ibuffer                    ;; An advanced replacement for BufferMenu
  :ensure nil ;; builtin
  :hook (ibuffer-mode . hl-line-mode)
  :bind* ("C-x C-b" . ibuffer)
  :functions ibuffer-update
  :config
  (use-package ibuf-ext                 ;; extensions for ibuffer
    :ensure nil
    :functions ibuffer-switch-to-saved-filter-groups)

  (setq ibuffer-expert t)                    ;; delete unmodified buffers without asking
  (setq ibuffer-show-empty-filter-groups nil);; don't show empty groups

  (eval-and-compile
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
      (seq-uniq
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
          (ibuffer-update nil t)))))

  (add-hook 'ibuffer-hook 'ibuffer-append-git-filter-groups)

  ;; simply overwrite the default size column
  (define-ibuffer-column size
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size) 'iec))

  (setq ibuffer-saved-filter-groups
        '(("default"
           ("dired" (mode . dired-mode))
           ("emacs" (or
                     (mode . Man-mode)
                     (mode . apropos-mode)
                     (mode . backtrace-mode)
                     (mode . grep-mode)
                     (mode . help-mode)
                     (mode . ibuffer-mode)
                     (mode . native-comp-limple-mode)
                     (name . "^\\*GNU Emacs\\*$")
                     (name . "^\\*Messages\\*$")
                     (name . "^\\*WoMan.*\\*$")
                     (name . "^\\*scratch\\*$")
                     (name . "^\\*xref\\*$")))
           ("org-roam" (and
                        (filename . "/roam/")
                        (file-extension . "org")))
           ("org" (mode . org-mode))
           ("pdf" (file-extension . "pdf"))
           ("magit" (derived-mode . magit-mode))
           ("mu4e" (or
                    (name . "^\\*mu4e-.*\\*$")
                    (derived-mode . message-mode)
                    (derived-mode . mail-mode))))))
  )

(use-package image                      ;; builtin image support
  :ensure nil ;; builtin
  :config
  (imagemagick-register-types))

(use-package immortal-scratch           ;; respawn the scratch buffer when it's killed
  :load-path "lisp/immortal-scratch/"
  :functions immortal-scratch-mode
  :config
  (immortal-scratch-mode))

(use-package ivy                        ;; Incremental Vertical completYon
  :diminish
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

(use-package lsp-jedi                   ;; Lsp client plugin for Python Jedi Language Server
  :ensure t)

(use-package json                       ;; JavaScript Object Notation parser / generator
  :ensure nil ;; builtin
  :config
  (setq json-encoding-default-indentation "    "))

(use-package json-mode                  ;; Major mode for editing JSON files
  :mode (rx ".json" string-end))

(use-package latex                      ;; Integrated environment for *TeX*
  :ensure auctex
  :after pdf-tools
  :demand t
  :mode ((rx ".tex" string-end) . latex-mode)
  :bind (:map LaTeX-mode-map
              ("C-c C-s" . pdf-sync-forward-search))
  ;; enable forward/inverse search
  :hook (LaTeX-mode . TeX-source-correlate-mode)
  :hook (LaTeX-mode . turn-on-reftex)
  ;; Update PDF buffers after successful LaTeX runs
  :hook (TeX-after-compilation-finished-functions . TeX-revert-document-buffer)
  :config
  (setq reftex-plug-into-AUCTeX t)
  ;; Use pdf-tools to open PDF files
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t))

(use-package lsp-mode                   ;; LSP mode
  :hook ((c-mode
          c++-mode
          python-mode)
         . lsp)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-modeline-code-actions-enable nil))

(use-package magit                      ;; A Git porcelain inside Emacs.
  :bind ("C-c g" . magit-status)
  :hook (git-commit-setup . git-commit-turn-on-flyspell)
  :config
  (setq transient-default-level 7)
  (setq magit-diff-refine-hunk 'all)

  (setq magit-display-buffer-function 'display-buffer)

  (setq magit-section-initial-visibility-alist
        '((stashes . hide) (ignored . hide) (local . hide)))

  (setq magit-log-section-commit-count 25)
  (setq magit-revision-show-gravatars t)

  (eval-and-compile
    (defun magit-insert-notes-header ()
      "Insert a header about the current note."
      (let ((note  (magit-git-output "notes" "show")))
        (unless (string-empty-p note)
          (magit-insert-section (note)
            (magit-insert-heading "Note")
            (insert note "\n"))))))

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-user-header
                          'magit-insert-status-headers)

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-repo-header
                          'magit-insert-status-headers)

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-remote-header
                          'magit-insert-status-headers)

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-branch-description
                          'magit-insert-untracked-files)

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-notes-header
                          'magit-insert-untracked-files)

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-local-branches
                          'magit-insert-stashes t)

  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-ignored-files
                          'magit-insert-stashes t))

(use-package man                        ;; browse UNIX manual pages
  :ensure nil ;; builtin
  :config
  (setq Man-notify-method 'aggressive))

(use-package markdown-mode              ;; Major mode for Markdown-formatted text
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package message                    ;; composing mail and news messages
  :ensure nil ;; builtin
  :config
  (setq message-signature (concat user-full-name "\n"))
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq message-kill-buffer-on-exit t))    ;; don't keep message buffers around

(use-package minibuffer                 ;; Minibuffer customization
  :ensure nil ;; builtin
  :config
  (setq completions-detailed t)
  (setq enable-recursive-minibuffers t)
  (minibuffer-depth-indicate-mode t)

  (eval-and-compile
    (defun defer-garbage-collection-h ()
      "Defer garbage collection."
      (setq gc-cons-threshold most-positive-fixnum))

    (defvar gc-cons-threshold-default)
    (defun restore-garbage-collection-h ()
      "Restore garbage collection."
      (setq gc-cons-threshold gc-cons-threshold-default))

    (defun restore-garbage-collection-soon-h ()
      "Defer it so that commands launched immediately after will enjoy the benefits."
      (run-at-time 1 nil #'restore-garbage-collection-h)))

  (add-hook 'minibuffer-setup-hook #'defer-garbage-collection-h)
  (add-hook 'minibuffer-exit-hook #'restore-garbage-collection-soon-h))

(use-package mu4e                       ;; an emacs-based e-mail client based on mu
  :when (require 'mu4e nil 'noerror)
  :ensure nil
  :after (flyspell message)
  :bind ("C-c C-m" . mu4e)
  :hook (mu4e-compose-mode . flyspell-mode)
  :config
  (setq mail-user-agent 'mu4e-user-agent)
  (setq read-mail-command 'mu4e)

  (setq mu4e-sent-folder   "/Sent Messages")    ;; folder for sent messages
  (setq mu4e-drafts-folder "/Drafts")           ;; unfinished messages
  (setq mu4e-trash-folder  "/Deleted Messages") ;; trashed messages
  (setq mu4e-refile-folder "/Archive")          ;; saved messages
  (setq mu4e-attachment-dir "~/Downloads/Mail") ;; attachments

  (setf (nth 0 mu4e-bookmarks)
        '( :name  "Unread messages"
           :query "flag:unread AND NOT flag:trashed AND NOT maildir:/Junk"
           :key   ?u))

  (setq mu4e-maildir-shortcuts
        '( ("/INBOX"            . ?i)
           ("/Sent Messages"    . ?s)
           ("/Deleted Messages" . ?t)
           ("/Archive"          . ?a)
           ("/Drafts"           . ?d)))

  (setq mu4e-use-fancy-chars t)
  (setq mu4e-sent-messages-behavior 'sent)
  (setq mu4e-get-mail-command "offlineimap")
  (setq mu4e-update-interval  300)

  (use-package smtpmail                 ;; simple SMTP protocol (RFC 821) for sending mail
    :ensure nil ;; builtin
    :after auth-source-pass
    :config
    (setq smtpmail-default-smtp-server "smtp.mail.me.com")
    (setq smtpmail-smtp-server         "smtp.mail.me.com")
    (setq smtpmail-smtp-user           user-mail-address)
    (setq smtpmail-smtp-service        587)))

(use-package multiple-cursors           ;; Multiple cursors for emacs.
  :bind
  (("C-c m l" . mc/edit-lines)
   ("C-c m n" . mc/mark-next-like-this)
   ("C-c m a" . mc/mark-all-like-this)
   ("C-c m m" . mc/mark-all-like-this-dwim)
   ("C-c m p" . mc/insert-numbers)))

(use-package org                        ;; Outline-based notes management and organizer
  :ensure org-contrib
  :after flyspell
  :bind (:map org-mode-map
              ("C-c i" . org-insert-link)
              ("C-c C-m" . nil))
  :bind* (("C-c c" . org-capture)
          ("C-c l" . org-store-link)
          ("C-c a" . org-agenda))
  :hook (org-mode . flyspell-mode)
  :hook (org-mode . auto-revert-mode)
  :hook (org-mode . hl-line-mode)
  ;; :hook (org-capture-after-finalize . revert-buffer-noconfirm)
  :hook (org-after-todo-statistics . org-summary-todo)
  :functions org-todo
  :preface
  (defun org-summary-todo (_ n-not-done)
    "Switch entry to DONE when all sub-entries are done, to TODO otherwise."
    (let (org-log-done _)   ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  (defun revert-buffer-noconfirm ()
    "Call revert-buffer with noconfirm = t."
    (interactive)
    (revert-buffer nil t))

  :config
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)
  (setq org-adapt-indentation t)
  (setq org-fold-catch-invisible-edits 'smart)
  (setq org-image-actual-width nil)
  (setq org-return-follows-link t)
  (setq org-insert-heading-respect-content t)
  (setq org-log-into-drawer t)
  (setq org-log-reschedule t)
  (setq org-log-redeadline t)
  (setq org-table-header-line-p t)
  (setq org-directory "~/Notes/")
  (setq org-default-notes-file (expand-file-name "todo.org" org-directory))
  (setq org-archive-location (expand-file-name "archive.org" org-directory))
  (setq org-blank-before-new-entry '((heading . t) (plain-list-item . nil)))

  (setq org-link-frame-setup
        '((file . find-file)))

  (use-package org-agenda               ;; Dynamic task and appointment lists for Org
    :ensure nil
    :config
    (setq org-agenda-window-setup 'current-window)
    (setq org-agenda-restore-windows-after-quit t))

  (setq org-file-apps
        '((auto-mode . emacs)))

  (use-package ox-extra                 ;; Convenience functions for org export
    :ensure nil
    :functions ox-extras-activate
    :config
    (ox-extras-activate '(ignore-headlines)))

  (add-to-list 'org-latex-classes '("scrlttr2" "\\documentclass{scrlttr2}"))

  (use-package ob                       ;; Working with Code Blocks in Org
    :ensure nil
    :config
    (use-package ob-rust)               ;; Org-babel functions for Rust

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C . t)
       (calc . t)
       (awk . t)
       (ditaa . t)
       (dot . t)
       (emacs-lisp . t)
       (latex . t)
       (makefile . t)
       (python . t)
       (rust . t)
       (shell . t))))

  (use-package org-capture              ;; Fast note taking in Org
    :ensure nil
    :config
    (setq org-capture-templates
          '(("t" "Tasks")
            ("tt" "personal todo" entry (file "todo.org")
             "* TODO %^{title}
  :LOGBOOK:
  - Created on %U
  :END:
  %?")

            ("ta" "appointment" entry (file "todo.org")
             "* APPT %^{title}
  %^t
  :LOGBOOK:
  - Created on %U
  :END:
  %?")

            ("tr" "repair something" entry (file "todo.org")
             "* REPAIR %^{title}
  :LOGBOOK:
  - Created on %U
  :END:
  %?")

            ("tl" "loaned something" entry (file "todo.org")
             "* LOAN %^{title}
  :LOGBOOK:
  - Created on %U
  :END:
  %?")

            ("to" "order something" entry (file "todo.org")
             "* ORDER %^{title}
  :LOGBOOK:
  - Created on %U
  :END:
  %?")

            ("tp" "email todo" entry (file "todo.org")
             "* TODO %:fromname: %a %^{title}
  :LOGBOOK:
  - Created on %U
  :END:
  %?")

            ("w" "Work")

            ("wt" "todo" entry (file "work.org")
             "* TODO %^{title}
  :LOGBOOK:
  - Created on %U
  :END:
  %?")

            ("wp" "phone call" entry (file+headline "work.org" "Calls")
             "* %<%H:%M> %^{title}
  :PROPERTIES:
  :CALLER: %^{caller}
  :END:
  %?")

            ("wm" "meeting" entry (file+headline "work.org" "Meeting Notes")
             "* %<%H:%M> %^{title}
  %?")

            ("n" "general note" entry (file "notes.org")
             "* %?")

            ("j" "journal entry" entry (file+datetree "journal.org")
             "* %<%H:%M> %^{title}
  %?" :time-prompt t)

            ("c" "contact" entry (file "contacts.org")
             "* %^{full name}
  :PROPERTIES:
  :EMAIL:    %^{email}
  :PHONE:    %^{phone number}
  :ADDRESS:  %^{address}
  :BIRTHDAY: %^{birthday}u
  :END:" :immediate-finish t)))

    (setq org-capture-templates-contexts
          '(("p" ((in-mode . "mu4e-headers") (in-mode . "mu4e-view"))))))

  (use-package org-roam                 ;; A database abstraction layer for Org-mode
    :bind (("C-c n f" . org-roam-node-find)
           ("C-c n c" . org-roam-capture))
    :bind (:map org-mode-map
                ("C-c n d" . org-id-get-create)
                ("C-c n b" . org-roam-buffer-toggle)
                ("C-c n n" . org-roam-node-insert)
                ("C-c n i" . org-roam-ref-add)
                ("C-c n a" . org-roam-alias-add)
                ("C-c n q" . my-org-roam-tag-add))
    :functions (org-roam-db-query org-roam-tag-add)
    :preface
    (defun my-org-roam-tag-add ()
      "Add a tag to the node at point with completion from existing tags."
      (interactive)
      (let* ((tags (seq-uniq (flatten-list (org-roam-db-query [:select tag :from tags]))))
             (tag (completing-read "Tag: " tags)))
        (org-roam-tag-add (list tag))))

    :config
    (setq org-roam-capture-templates
          '(("d" "default" plain "%?"
             :target (file+head "${slug}.org"
                                "#+title: ${title}\n#+date: %U\n")
             :unnarrowed t)))

    (setq org-roam-node-display-template "${title:80} ${tags}")
    (setq org-roam-directory (expand-file-name "roam" org-directory))
    (org-roam-db-autosync-mode))

  (use-package org-roam-ui              ;; User Interface for Org-roam
    :after org-roam
    :bind ("C-c n g" . org-roam-ui-mode)
    :config
    (setq org-roam-ui-follow nil)))

(use-package orgit                      ;; Support for Org links to Magit buffers
  :after (magit org))

(use-package pdf-tools                  ;; Support library for PDF documents.
  :magic ("%PDF" . pdf-view-mode)
  :bind (:map pdf-view-mode-map
              ("C-s" . isearch-forward)
              ("C-g" . pdf-view-goto-page))
  :config
  ;; initialize pdf-tools
  (pdf-tools-install :no-query)
  (use-package pdf-sync                 ;; Use synctex to correlate LaTeX-Sources with PDF positions.
    :ensure nil))

(use-package pinentry                   ;; GnuPG Pinentry server implementation
  :init
  (setq epg-pinentry-mode 'loopback)
  :config
  (pinentry-start))

(use-package python-black               ;; Reformat Python using python-black
  :after python
  :hook (python-mode . python-black-on-save-mode))

(use-package python                     ;; Python's flying circus support for Emacs
  :ensure nil);; builtin

(use-package pyvenv                     ;; Python virtual environment interface
  :hook (python-mode . pyvenv-mode)
  :config
  (add-hook 'pyvenv-post-activate-hooks 'pyvenv-restart-python)
  (add-hook 'pyvenv-post-deactivate-hooks 'pyvenv-restart-python))

(use-package scad-mode)                 ;; A major mode for editing OpenSCAD code

(use-package server                     ;; Lisp code for GNU Emacs running as server process
  :when (display-graphic-p)             ;; only start the server in X
  :unless (server-running-p)            ;; unless there is already a server running
  :ensure nil ;; builtin
  :no-require
  :hook (after-init . server-start))

(use-package sgml-mode                  ;; The simplest mode to edit XML
  :ensure nil ;; builtin
  :mode ((rx ".handlebars" string-end) . html-mode)
  :config
  (setq sgml-basic-offset 4))

(use-package simple                     ;; basic editing commands for Emacs
  :ensure nil ;; builtin
  :bind (("M-n" . next-error)
         ("M-p" . previous-error)))

(use-package smartparens                ;; Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
  :diminish
  :functions sp-local-pair
  :config
  (smartparens-global-mode)             ;; global (){} completion
  (show-smartparens-global-mode))       ;; global (){} highlighting

(use-package systemd)                   ;; Major mode for editing systemd units

(use-package swiper                     ;; Isearch with an overview. Oh, man!
  :after ivy
  :bind ("C-s" . swiper))

(use-package vc                         ;; drive a version-control system from within Emacs
  :ensure nil ;; builtin
  :config
  (setq vc-follow-symlinks t)           ;; always follow symlinks
  (setq vc-make-backup-files t))        ;; also backup version controlled files

(use-package whitespace                 ;; whitespace-cleanup customization's
  :ensure nil ;; builtin
  :config
  (defvar no-whitespace-cleanup nil
    "Variable for disabling whitespace-cleanup before save.")
  (put 'no-whitespace-cleanup 'safe-local-variable 'booleanp)

  (defun my-whitespace-cleanup ()
    "Clean up whitespace in a buffer, unless that buffer is in `fundamental-mode'."
    (unless (or (eq major-mode 'fundamental-mode) no-whitespace-cleanup)
      (whitespace-cleanup)))

  (add-hook 'before-save-hook 'my-whitespace-cleanup))

(use-package window                     ;; GNU Emacs window commands aside from those written in C
  :ensure nil ;; builtin
  :config
  (setq split-width-threshold nil)  ;; don't split vertically
  (setq split-height-threshold nil) ;; don't split horizontally

  (setq switch-to-buffer-obey-display-actions t)
  (setq display-buffer-base-action '(display-buffer-use-some-window))

  (setq display-buffer-alist
        `((,(rx bol (or
                     (: "*Org Select*")
                     (: "*Capture*")
                     (: "CAPTURE-" (* nonl) ".org")
                     (: " *Org tags*")
                     (: "*mu4e-main*")
                     (: "*Ibuffer*")
                     (: "*Messages*")
                     (: "magit-log:" (* nonl))
                     eol))
           display-buffer-use-some-window)
          (,(lambda (name _) (with-current-buffer name (derived-mode-p 'gptel-mode)))
           display-buffer-use-some-window)
          (,(rx bol (or
                     (: "*mu4e-draft*")
                     (: "*mu4e-headers*"))
                eol)
           display-buffer-same-window)
          (,(rx (: "*mu4e-article*"))
           display-buffer-below-selected)
          (,(rx
             (or
              (: bol "magit:" (* nonl))
              (: ".pdf" (? (group "<" (1+ (not ">")) ">")))
              (: bol "*Help*")
              (: bol "*Man " (* nonl) "*")
              (: bol "*WoMan " (* nonl) "*")
              (: bol "*xref*")
              (: bol "*grep*")
              (: bol "*org-roam*")
              (: bol "*Org Agenda" (* nonl) "*")
              (: bol "COMMIT_EDITMSG")
              (: bol "*Python*"))
             eol)
           display-buffer-in-side-window
           (dedicated . side)
           (side . right)
           (window-width . 0.4)
           (window-height . 0.66)
           (window-parameters . ((no-other-window . t))))
          (,(rx bol (or
                     (: "*compilation*")
                     (: "*Warnings*")
                     (: "*Org Links*")
                     (: "*eshell*")
                     (: "*shell*")
                     (: "*terminal*")
                     (: "*TeX Help*")
                     (: "*ansi-term*"))
                eol)
           display-buffer-in-side-window
           (dedicated . side)
           (inhibit-same-window . t)
           (side . right)
           (slot . 1)
           (window-width . 0.4)
           (window-height . 0.33)
           (window-parameters . ((no-other-window . t))))
          (,(lambda (name _) (with-current-buffer name (derived-mode-p 'term-mode)))
           display-buffer-in-side-window
           (dedicated . side)
           (inhibit-same-window . t)
           (side . right)
           (slot . 1)
           (window-width . 0.4)
           (window-height . 0.33)
           (window-parameters . ((no-other-window . t))))
          (,(rx bol
                (or
                 (: " *Agenda Commands*"))
                eol)
           display-buffer-in-side-window
           (side . bottom)
           (slot . 0)
           (window-height . 0.35)
           (window-parameters . ((no-other-window . t))))
          (,(rx bol
                (or
                 (: " *transient*"))
                eol)))))

(use-package yaml-mode)                 ;; Major mode for editing YAML files

(use-package yasnippet                  ;; Yet another snippet extension for Emacs.
  :diminish yas-minor-mode
  :config
  (yas-global-mode))


(defun my-emacs-uptime ()
  "Get the time since this EMACS instance was started."
  (format-time-string "%s.%6Ns" (time-since before-init-time)))

(defun random-string (length)
  "Return a random string of LENGTH."
  (let* ((alnum "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
         (len-alnum (length alnum))
         (output ""))
    (dotimes (_ length)
      (setq output (concat output (string (elt alnum (random len-alnum))))))
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
(when (eq system-type 'darwin)    (load (emacs-path "mac")))   ;; macOS
(when (eq system-type 'gnu/linux) (load (emacs-path "linux"))) ;; Linux

(load (emacs-path "themes")) ;; custom themes

;; log how long emacs took to start
(message "Loading %s...done (%s)" load-file-name (my-emacs-uptime))

(add-hook 'after-init-hook
          `(lambda ()
             (message "Loading %s...done (%s) [after-init]"
                      ,load-file-name (my-emacs-uptime))) t)

;;; init.el ends here
