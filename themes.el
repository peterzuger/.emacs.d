;;; themes.el --- custom emacs themes
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

;; load zenburn on start
(load-theme 'zenburn t)

(unless (display-graphic-p)
  (set-background-color "ARGBBB000000"))

;;; themes.el ends here
