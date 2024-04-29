;;; init-auto-save.el --- Init for auto-save.el
;; Filename: init-auto-save.el
;;; Commentary:
;;
;; Init for auto-save.el
;;
;;; Require

(require 'auto-save)

;;; Code:

(auto-save-enable)
(setq auto-save-silent t)
(setq auto-save-delete-trailing-whitespace t)

(provide 'init-auto-save)

;;; init-auto-save.el ends here
