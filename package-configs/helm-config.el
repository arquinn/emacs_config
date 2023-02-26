;;; helm-config.el --- Summary

;;; Commentary:
;;; Settings for helm.
;;; Helm config taken from https://tuhdo.github.io/c-ide.html

;;; Code:

(use-package helm)
;;(use-package helm-config)


(setq helm-split-window-inside-p t)

;; adjust helm's prefix
(global-set-key (kbd "C-c h") 'helm-command-prefix)

;; default find files is helm now:
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; something to help out with buffer opening
(global-set-key (kbd "C-x b") 'helm-mini)

;; use Helm's meta-X for doing commands
(global-set-key (kbd "M-x") 'helm-M-x)

;; fixup helm's keystrokes
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)

;; helm will show you stuff you killed a while back with M-y
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(set-face-attribute 'helm-source-header nil :height 0.1)

;; fixup helm configs
(setq helm-move-to-line-cycle-in-source     t)
;;(setq helm-ff-search-library-in-sexp        t)

;;(setq helm-ff-file-name-history-use-recentf t)

;;(setq helm-echo-input-in-header-line t)
(setq helm-display-header-line nil) ;; t by default

;;(setq helm-buffers-fuzzy-matching t
;;      helm-recentf-fuzzy-match    t)

(helm-autoresize-mode 1)
(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)

(helm-mode 1)

(provide 'helm-config)
;;; helm-config.el ends here
