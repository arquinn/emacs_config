;;; fill-column-indicator-config.el --- Summary

;;; Commentary:
;;; Settings for fill-column-indicator

;;; Code:
(use-package fill-column-indicator)
(add-hook 'after-change-major-mode-hook 'fci-mode)

(provide 'fill-column-indicator-config)
;;; fill-column-indicator-config.el ends here
