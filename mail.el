;;; mail.el --- mail configuration
;;; Commentary:
;;; Code:

(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; set the location of my Maildir
(setq
 mu4e-maildir       "~/Mail"            ;; top-level Maildir
 mu4e-sent-folder   "/Sent Messages"    ;; folder for sent messages
 mu4e-drafts-folder "/Drafts"           ;; unfinished messages
 mu4e-trash-folder  "/Deleted Messages" ;; trashed messages
 mu4e-refile-folder "/Archive")         ;; saved messages

;; don't save message to Sent Messages, IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; allow for updating mail using 'U' in the main view:
(setq mu4e-get-mail-command "offlineimap")
(setq mu4e-update-interval  300)          ;; update every 5 minutes

;; setup shortcuts
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"            . ?i)
         ("/Sent Messages"    . ?s)
         ("/Deleted Messages" . ?t)
         ("/Archive"          . ?a)
         ("/Drafts"           . ?d)))

;; setup the default signature
(setq mu4e-compose-signature (concat user-full-name "\n"))

;; setup compose mode
(add-hook 'mu4e-compose-mode-hook
          (lambda ()
            (set-fill-column 80)
            (flyspell-mode)))

;; setup outgoing mail
(require 'smtpmail)
(setq message-send-mail-function 'smtpmail-send-it)
(setq starttls-use-gnutls t)
(setq smtpmail-starttls-credentials '(("smtp.mail.me.com" 587 nil nil)))
(setq smtpmail-auth-credentials     '(("smtp.mail.me.com" 587 "zueger.peter@icloud.com" nil)))
(setq smtpmail-default-smtp-server     "smtp.mail.me.com")
(setq smtpmail-smtp-server             "smtp.mail.me.com")
(setq smtpmail-smtp-service             587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; use 'fancy' non-ascii characters in various places in mu4e
(setq mu4e-use-fancy-chars t)

;; save attachment to Downloads
(setq mu4e-attachment-dir "~/Downloads/Mail")

;; attempt to show images when viewing messages
(setq mu4e-view-show-images t)

;; use imagemagick, if available
(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

;; ask if messages should be decrypted
(setq mu4e-decryption-policy 'ask)

;; pinentry for gpg signed and encrypted messages
(setq epa-pinentry-mode 'loopback)
(pinentry-start)

;;; mail.el ends here
