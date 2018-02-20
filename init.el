(require 'package)

;; add melpa archive
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" . "http://melpa.org/packages/")))

;; add packages for auto install
(setq package-list
      '(auctex                  ;; 
	company                 ;; 
	company-irony           ;; 
	company-shell           ;; 
	fill-column-indicator   ;; 
	flycheck                ;; 
	irony                   ;; 
	jedi                    ;; 
	markdown-mode           ;; 
	smartparens             ;; 
	whitespace-cleanup-mode ;; 
))

(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; load some additional configurations
(when (eq system-type 'darwin)(load-file    "~/.emacs.d/mac.el"  )) ;; Mac OS X
(when (eq system-type 'gnu/linux)(load-file "~/.emacs.d/linux.el")) ;; Linux
(load-file "~/.emacs.d/c.el")           ;; C/C++ configuration
(load-file "~/.emacs.d/elisp.el")       ;; elisp configuration
(load-file "~/.emacs.d/html.el")        ;; html configuration
(load-file "~/.emacs.d/javascript.el")  ;; javascript configuration
(load-file "~/.emacs.d/markdown.el")    ;; markdown configuration
(load-file "~/.emacs.d/tex.el")         ;; tex configuration
(load-file "~/.emacs.d/python.el")      ;; python configuration
(load-file "~/.emacs.d/themes.el")      ;; custom themes

;; Display emacs logo on startup
(defun use-fancy-splash-screens-p ()
  (when (and (display-graphic-p)
             (or (and (display-color-p)
		      (image-type-available-p 'xpm))
                 (image-type-available-p 'pbm)))
    (let ((frame (fancy-splash-frame)))
      (when frame
	(let* ((img (create-image (fancy-splash-image-file)))
	       (image-height (and img (cdr (image-size img nil frame))))
	       (frame-height (1- (frame-height frame))))
	  (> frame-height (+ image-height 17)))))))

;; start emacs in fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; don't create .#<filename> files
(setq create-lockfiles nil)

;; display line,column numbers
(setq global-linum-mode t)
(setq column-number-mode t)

;; move backup files to ~/.emacs.d/backup
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t    ;; Don't delink hardlinks
      version-control t      ;; Use version numbers on backups
      delete-old-versions t  ;; Automatically delete excess backups
      kept-new-versions 20   ;; how many of the newest versions to keep
      kept-old-versions 5    ;; and how many of the old
      )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879"
     "5e3fc08bcadce4c6785fc49be686a4a82a356db569f55d411258984e952f194a"
     "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6"
     "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58"
     "a0feb1322de9e26a4d209d1cfa236deaf64662bb604fa513cca6a057ddf0ef64"
     "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d"
     "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e"
     "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a"
     "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58"
     "65a2801c39211a69af6cc4c8441f3f51871404b9d12a6f2966a7bc33468995c7"
     "b9a9204174c09936593d7c6e69ba300486b58999cae067d4af5d5cb180784b42"
     "43b0db785fc313b52a42f8e5e88d12e6bd6ff9cee5ffb3591acf51bbd465b3f4"
     "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0"
     "3c03b55aeb17a451e5fd23747e418f98a80db9950de203c534ac177ec32c42cf"
     "d409bcd828a041ca8c28433e26d1f8a8e2f0c29c12c861db239845f715a9ea97"
     "1ba61848d0d8c78e037867c26f118875705c20f5ad64949a8cee8c8059e5c50f"
     "a19265ef7ecc16ac4579abb1635fd4e3e1185dcacbc01b7a43cf7ad107c27ced"
     "b9a06c75084a7744b8a38cb48bc987de10d68f0317697ccbd894b2d0aca06d2b"
     "b9cbfb43711effa2e0a7fbc99d5e7522d8d8c1c151a3194a4b176ec17c9a8215"
     "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3"
     "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a"
     "858a353233c58b69dbe3a06087fc08905df2d8755a0921ad4c407865f17ab52f"
     "95f16059541fffdf6141fa704dc9d26a20a6b8f4528b1f666dbfc80e109380e6"
     "c0155fc5592601de74e3071998d2291bc9801e113109371915b06c65d2631837"
     "5b20570781c33819c0b4bcb009305dbe5a9ed12fcedca10e29f1703b5b9d3f96"
     "7bf3008de4c1be932cac1f213859613f5d6d06189d0bd6f5b2000a8430fe9a58"
     "0eb173dcdd23dbc02e0a178c7e8d3d9a9697786c11ef68b77b8b6255d4163cfd"
     "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016"
     "109c37722f5b922ab2c023d364bbe3bd1e2a49e6c3267ff7bca2ccdddbf9f9c2"
     "ace9f12e0c00f983068910d9025eefeb5ea7a711e774ee8bb2af5f7376018ad2"
     "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea"
     "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596"
     "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f"
     "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481"
     "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9"
     "c335adbb7d7cb79bc34de77a16e12d28e6b927115b992bccc109fb752a365c72"
     "8e4efc4bed89c4e67167fdabff77102abeb0b1c203953de1e6ab4d2e3a02939a"
     default)))
 '(nil nil t)
 '(package-selected-packages
   (quote
    (whitespace-cleanup-mode smartparens markdown-mode
			     jedi flycheck fill-column-indicator
			     company-shell company-irony company
			     auctex))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
