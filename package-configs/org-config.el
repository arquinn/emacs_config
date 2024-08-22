;;; org-config.el --- Summary

;;; Commentary:
;;; Settings for org.
;;; Org config taken from https://tuhdo.github.io/c-ide.html

;;; Code:
(use-package org)

;; setup some file paths
(defvar org-base (getenv "ORG_HOME"))
(defvar org-todos (concat org-base "todos.org"))

(setq org-agenda-files org-base)


;; When a TODO is set to a done state, record a timestamp
(setq org-log-done 'time)

;; Follow the links
(setq org-return-follows-link  t)

;; Associate all org files with org mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;; Make the indentation look nicer
(add-hook 'org-mode-hook 'org-indent-mode)

;; Remap the change priority keys to use the UP or DOWN key
(define-key org-mode-map (kbd "C-c <up>") 'org-priority-up)
(define-key org-mode-map (kbd "C-c <down>") 'org-priority-down)

;; Shortcuts for storing links, viewing the agenda, and starting a capture
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; When you want to change the level of an org item, use SMR
(define-key org-mode-map (kbd "C-c C-g C-r") 'org-shiftmetaright)


;; Wrap the lines in org mode so that things are easier to read
(add-hook 'org-mode-hook 'visual-line-mode)

;;; set some keybindings for org mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)


;;; Capture template for a todo...?
(setq org-capture-templates
      '(
        ("t" "General To-Do"
         entry (file+headline org-todos "General Tasks")
         "* TODO [#B] %?\n:Created: %T\n "
         :empty-lines 0)
      ))



(provide 'org-config)
;;; org-config.el ends here

