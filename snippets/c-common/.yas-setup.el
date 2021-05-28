;;; snippets/c-common/.yas-setup.el --- C snippet helpers -*- lexical-binding: t; -*-
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

(defun c-header-file-name (filename)
  "Figure out the filename for @file in C/C++ files for FILENAME."
  (let ((root-dir (locate-dominating-file filename ".git")))
    (if root-dir
        (file-relative-name filename (concat root-dir "/.."))
      (file-name-nondirectory filename))))

(defun include-guard-string (filename)
  "Return an include guard string for the given FILENAME."
  (when filename
    (concat
     (unless (file-name-extension filename) "__")
     (upcase (file-name-nondirectory (file-name-sans-extension filename)))
     (if (file-name-extension filename)
         (concat "_" (upcase (file-name-extension filename)))
       "__"))))

(defun file-extension-c-header-p (filename)
  "Check if FILENAME indicates that it is a C header."
  (when filename
    (string-equal "h" (file-name-extension filename))))

(defun c-extern-c-begin (filename)
  "Return the 'extern C' for C headers if FILENAME is a C header."
  (when (file-extension-c-header-p filename)
    "\n#if defined(__cplusplus)\nextern \"C\"{\n#endif /* defined(__cplusplus) */\n"))

(defun c-extern-c-end (filename)
  "Return the end of 'extern C' if FILENAME indicated C."
  (when (file-extension-c-header-p filename)
    "\n#if defined(__cplusplus)\n}\n#endif /* defined(__cplusplus) */\n"))

;;; .yas-setup.el ends here
