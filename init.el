;;; package -- summary
;; Configuration that I've found useful for using emacs on c/c++
;; development.

;;; Commentary:

;;; Code:

(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))


;; load directory for configuration files for emacs
(add-to-list 'load-path (concat user-emacs-directory "package-configs/"))

;; set home and emacs directories
(defvar user-home-directory (concat (getenv "HOME") "/"))
(setq user-emacs-directory (concat user-home-directory ".emacs.d/"))

;; Silence the haters (warnings)
(setq warning-minimum-level :error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme and Display settings

;;; add line numbers in programming modes.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq column-number-mode t)


;; fill-column at 80 by default
(setq-default fill-column 80)

;; add a newline at the end of files if necessary
(setq require-final-newline t)

;; change noise:
(setq visible-bell t)

;; use the color theme.
(use-package color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-eighties t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup a couple of useful commands

;; resizing windows
(global-set-key (kbd "S-C-<left>")  'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>")  'shrink-window)
(global-set-key (kbd "S-C-<up>")    'enlarge-window)

;; change goto line
(global-set-key (kbd "C-x l") 'goto-line)

;; undo / reod
(global-set-key (kbd "C--") 'undo)


;; place backup files in a less annoying place
(setq backup-directory-alist        '((".*" . "~/.Trash")))

;; use custom packages
;; flyckeck---give nice errors as we go along
(load "flycheck-config.el")
;; (load "fill-column-indicator-config.el")
(load "helm-config.el")
(load "helm-gtags-config.el")
(load "org-config.el")
(load "ws-butler-config.el")

;; use standard packages
(use-package protobuf-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Whitespace mode---to stop leaving garbage spaces everywhere
(use-package whitespace)
(setq whitespace-style '(face trailing))
(add-hook 'after-change-major-mode-hook 'whitespace-mode)


(provide 'init)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(package-selected-packages
   '(markdown-mode rust-mode ws-butler clang-format
     color-theme-sanityinc-tomorrow color-theme protobuf-mode
      function-args sr-speedbar helm stickyfunc-enhance flycheck)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(speedbar-directory-face ((t (:inherit font-lock-string-face)))))
