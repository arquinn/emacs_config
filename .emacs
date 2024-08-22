
;;; Code:
;; (setq c-default-style "linux" c-basic-offset 4)



(add-to-list 'backup-directory-alist (cons "." "~/.emacs.d/backups/"))
(customize-set-variable 'tramp-backup-directory-alist backup-directory-alist)

(setq tramp-default-method "ssh")

(provide '.emacs)
;;; .emacs ends here

