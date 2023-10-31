;;; early-init.el --- emacs configuration -*- lexical-binding: t; -*-
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

(defconst gc-cons-threshold-default (* 16 1024 1024)
  "The default threshold for the GC increased from 800kB to 16MB.
Reset `gc-cons-threshold' to this value to prevent runaway memory usage.")

(setq gc-cons-threshold most-positive-fixnum)

(add-to-list 'default-frame-alist '(fullscreen . maximized)) ;; start emacs in fullscreen
(menu-bar-mode -1)                                           ;; remove the menue bar
(tool-bar-mode -1)                                           ;; remove the toolbar
(scroll-bar-mode -1)                                         ;; remove the scrollbar
(tooltip-mode -1)                                            ;; disable tooltips

(let ((file-name-handler-alist-tmp file-name-handler-alist))
  (setq-default file-name-handler-alist nil)

  (defun late-startup-hook ()
    (setq gc-cons-threshold gc-cons-threshold-default)
    (setq file-name-handler-alist
          (delete-dups (append file-name-handler-alist
                               file-name-handler-alist-tmp))))
  (add-hook 'emacs-startup-hook 'late-startup-hook t))

;;; early-init.el ends here
