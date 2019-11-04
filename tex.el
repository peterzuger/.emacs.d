;;; tex.el --- TEX configuration
;;; Commentary:
;;; Code:

(require 'latex)

;; goto pdf
(define-key LaTeX-mode-map (kbd "C-c C-g") 'pdf-sync-forward-search)

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
          'TeX-revert-document-buffer)

(setq TeX-source-correlate-method 'synctex)
(setq TeX-source-correlate-start-server t)
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (TeX-source-correlate-mode)))

;; initialize pdf-tools
(pdf-loader-install)

;;; tex.el ends here
