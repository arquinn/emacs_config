;;; flycheck-config.el --- Summary

;;; Commentary:
;;; special settings for using clang-format


;;; Code:
(use-package flycheck)
;;(setq-default flycheck-disabled-checkers '(c/c++-clang))

(add-hook 'c++-mode-hook
	  (lambda () (setq flycheck-gcc-language-standard "c++11")))

(global-flycheck-mode)

(provide 'clang-fromat-config)
;;; flycheck-config.el ends here
