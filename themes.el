;;; themes.el --- custom emacs themes
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

;; https://github.com/bbatsov/zenburn-emacs
;; https://github.com/purcell/color-theme-sanityinc-tomorrow
;; https://github.com/kuanyui/moe-theme.el
;; https://github.com/cpaulik/emacs-material-theme
;; https://github.com/n3mo/cyberpunk-theme.el
;; https://github.com/m00natic/anti-zenburn-theme
;; https://github.com/jordonbiondo/ample-theme
;; https://github.com/alezost/alect-themes
;; https://github.com/wasamasa/gotham-theme
;; https://github.com/juba/color-theme-tangotango
;; https://github.com/rexim/gruber-darker-theme
;; https://github.com/mjwall/ample-zen
;; https://github.com/sjrmanning/noctilux-theme
;; https://github.com/osener/emacs-afternoon-theme
;; https://github.com/steckerhalter/grandshell-theme
;; https://github.com/cryon/subatomic
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/zenburn/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/moe/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/material/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/cyberpunk/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/ample/")
(add-to-list 'load-path              "~/.emacs.d/themes/alect/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/alect/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/gotham/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/tangotango/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/gruber/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/ample-zen/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/noctilux/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/afternoon/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/grandshell/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/blackboard/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/tron-legacy-emacs-theme/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/wilmersdorf-emacs-theme/")

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(unless (display-graphic-p)
  (add-hook 'after-load-theme-hook
            (lambda ()
              (set-background-color "ARGBBB000000"))))

;; load zenburn on start
(load-theme 'zenburn t)

;;; themes.el ends here
