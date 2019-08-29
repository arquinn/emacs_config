
;;; Code:
(setq c-pdefault-style "linux" c-basic-offset 4)

(global-linum-mode t)

(setq linum-format "%4d \u2502 ")

;;;(setq backup-directory-alist '(("." . "~/MyEmacsBackups")))

(add-to-list 'backup-directory-alist (cons "." "~/.emacs.d/backups/"))
(customize-set-variable 'tramp-backup-directory-alist backup-directory-alist)

(provide '.emacs)
;;; .emacs ends here
