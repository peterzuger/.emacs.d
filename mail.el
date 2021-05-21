;;; mail.el --- mail configuration -*- lexical-binding: t; -*-
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

(require 'use-package)

(use-package mu4e                       ;; an emacs-based e-mail client based on mu
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
    (setq smtpmail-starttls-credentials '(("smtp.mail.me.com" 587 nil nil)))
    (setq smtpmail-auth-credentials     '(("smtp.mail.me.com" 587 user-mail-address nil)))
    (setq smtpmail-default-smtp-server     "smtp.mail.me.com")
    (setq smtpmail-smtp-server             "smtp.mail.me.com")
    (setq smtpmail-smtp-service             587)))

;;; mail.el ends here
