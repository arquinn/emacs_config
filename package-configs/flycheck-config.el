;;; flycheck-config.el --- Summary

;;; Commentary:
;;; special settings for using clang-format


;;; Code:
(use-package flycheck)

(setq-default flycheck-disabled-checkers '(c/c++-gcc))

(defvar flycheck-clang-args)
(setq flycheck-clang-args "-std=c++20")

(global-flycheck-mode)

(provide 'flycheck-config)
;;; flycheck-config.el ends here
