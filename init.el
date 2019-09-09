;;; package -- summary

;; Configuration that I've found useful for using emacs on c/c++
;; development.

;;; Commentary:

;;; Code:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))

  (add-to-list 'package-archives
	       (cons "melpa"
		     (concat proto "://melpa.org/packages/")) t))

(package-initialize)

(setq package-list '(flycheck whitespace fill-column-indicator
			      protobuf-mode helm  helm-gtags
			      color-theme-sanityinc-tomorrow))

(unless package-archive-contents (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(setq c-default-style "linux"  c-basic-offset 4)

(global-linum-mode t)
(setq column-number-mode t)
(setq linum-format "%4d \u2502")

(require 'flycheck)
(setq-default flycheck-disabled-checkers '(c/c++-clang))

(add-hook 'c++-mode-hook
	  (lambda () (setq flycheck-gcc-language-standard "c++11")))

(global-flycheck-mode)



(add-to-list 'load-path "~/.emacs.d/custom")
(load "google-c-style.el")

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(require 'whitespace)
(setq whitespace-style '(face trailing))
(add-hook 'after-change-major-mode-hook 'whitespace-mode)

(require 'fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)

(require 'protobuf-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm config taken from https://tuhdo.github.io/c-ide.html

(require 'helm)
(require 'helm-config)

;; adjust helm's prefix
(global-set-key (kbd "C-c h") 'helm-command-prefix)

;; default find files is helm now:
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; fixup helm's keystrokes
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)


;; fixup helm configs
(setq helm-move-to-line-cycle-in-source     t
      helm-ff-search-library-in-sexp        t
      helm-scroll-amount                    8
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(helm-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm-gtags config taken from https://tuhdo.github.io/c-ide.html

(require 'helm-gtags)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

;; use speedbar
;;; (require 'sr-speedbar)
;;; (require 'speedbar)
;;; (setq sr-speedbar-right-side nil) ; put on left side

;;; (provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(package-selected-packages
   (quote
    (color-theme-sanityinc-tomorrow color-theme protobuf-mode google-c-style function-args sr-speedbar helm stickyfunc-enhance flycheck fill-column-indicator))))

(load-theme 'sanityinc-tomorrow-eighties)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(speedbar-directory-face ((t (:inherit font-lock-string-face)))))



