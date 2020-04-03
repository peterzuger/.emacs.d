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

(setq org-cycle-separator-lines 1)
(setq org-src-fontify-natively t)
(setq org-catch-invisible-edits 'smart)

(global-set-key (kbd "C-c c") 'org-capture)

(setq org-capture-templates
      (quote (
              ("t" "todo" entry (file "~/Notes/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)

              ("n" "note" entry (file "~/Notes/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)

              ("j" "journal" entry (file+datetree "~/Notes/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
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
