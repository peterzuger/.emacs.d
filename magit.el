;;; magit.el --- magit configuration
;;;
;;; The MIT License (MIT)
;;;
;;; Copyright (c) 2021 Peter Züger
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

(require 'magit)

(global-set-key (kbd "<f6>") 'magit-status)

(add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)

(setq transient-default-level 5)
(setq magit-diff-refine-hunk 'all)

(setq magit-section-initial-visibility-alist
      '((stashes . hide) (ignored . hide) (local . hide)))

(defun magit-ignored-files ()
  "Command to show all ignored files."
  (magit-git-items "ls-files" "--others" "--ignored" "--exclude-standard" "-z" "--directory"))

(defun magit-insert-ignored-files ()
  "Ignored files section for magit-status buffer."
  (-when-let (files (magit-ignored-files))
    (magit-insert-section (ignored)
      (magit-insert-heading "Ignored files")
      (dolist (file files)
        (magit-insert-section (file file)
          (insert (propertize file 'font-lock-face 'magit-filename) ?\n)))
      (insert ?\n))))

(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-user-header
                        'magit-insert-status-headers nil)

(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-local-branches
                        'magit-insert-stashes t)

(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-ignored-files
                        'magit-insert-stashes t)

;;; magit.el ends here
