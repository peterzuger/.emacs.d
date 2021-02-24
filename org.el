;;; org.el --- org mode configuration
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

(require 'org)
(require 'org-contacts)

(setq org-cycle-separator-lines 1)
(setq org-src-fontify-natively t)
(setq org-catch-invisible-edits 'smart)
(setq org-image-actual-width nil)
(setq org-return-follows-link t)

(setq org-src-tab-acts-natively t)
(setq org-src-preserve-indentation t)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-contacts-files "~/Notes/contacts.org")

(setq org-capture-templates
      (quote (
              ("t" "todo" entry (file "~/Notes/todo.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)

              ("n" "note" entry (file "~/Notes/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)

              ("j" "journal" entry (file+datetree "~/Notes/journal.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)

              ("c" "contact" entry (file "~/Notes/contacts.org")
               "* %(org-contacts-template-name) %^g
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:PHONE: %^{Phone Number}
:ADDRESS: %^{Adress}
:BIRTHDAY: %^{Birthday}
:NOTE: %^{Note}
:END:" :empty-lines 0)
              )))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (C . t)
   (dot . t)
   (ditaa . t)
   (emacs-lisp . t)
   (python . t)))

;;; org.el ends here
