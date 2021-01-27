;;; early-init.el --- emacs configuration
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

(defvar gc-cons-threshold-default gc-cons-threshold)
(setq gc-cons-threshold (* 1024 1024 1024)) ;; 1GB
(run-with-idle-timer 5 nil
                     (lambda()
                       (setq gc-cons-threshold gc-cons-threshold-default)))

(menu-bar-mode -1)                                           ;; remove the menue bar
(tool-bar-mode -1)                                           ;; remove the toolbar
(scroll-bar-mode -1)                                         ;; remove the scrollbar
(mouse-avoidance-mode 'exile)                                ;; exile the mouse cursor
(setq global-linum-mode t)                                   ;; display line,column numbers
(setq column-number-mode t)                                  ;; "
(setq use-dialog-box nil)                                    ;; don't use dialog boxes

;;; early-init.el ends here
