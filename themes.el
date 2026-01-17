;;; themes.el --- custom emacs themes -*- lexical-binding: t; -*-
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


(let ((basedir (expand-file-name "themes/" user-emacs-directory)))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
             (file-directory-p (concat basedir f)))
        (add-to-list 'custom-theme-load-path (concat basedir f)))))

(defun my-adapt-font-size (&optional frame)
  "Adapt the font size of FRAME to make the text 2mm high."
  (let* ((attrs (frame-monitor-attributes frame))
         (size (alist-get 'mm-size attrs))
         (geometry (alist-get 'geometry attrs))
         (ppm (/ (caddr geometry) (float (car size))))
         (target-height (round (* ppm 20))))
    (message "PPM: %s, Target height: %s" ppm target-height)
    (set-face-attribute 'default frame :height target-height)))

(add-function :after after-focus-change-function #'my-adapt-font-size)
(add-hook 'after-make-frame-functions #'my-adapt-font-size)

;; enable transparency in the terminal
;; alpha-background does not seem to work for this
(unless (display-graphic-p)
  (add-hook 'tty-setup-hook
            (lambda ()
              (modify-frame-parameters nil '((background-color))))))

(defun my-toggle-transparency ()
  "Toggle transparency between 95 and 100."
  (let* ((current (cdr (assoc 'alpha-background (frame-parameters))))
         (new (if (= current 100) 95 100)))
    (set-frame-parameter nil 'alpha-background new)))

;; load zenburn on start
(load-theme 'zenburn t)

;;; themes.el ends here
