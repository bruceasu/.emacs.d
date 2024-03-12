;; init-check.el --- Initialize check configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Check configurations.
;;

;;; Code:
(eval-when-compile
  (require 'init-package))


;; 语法检查包
(use-package flycheck
  :ensure t
  :defer 3)


(use-package flymake
  :diminish
  :functions my-elisp-flymake-byte-compile
  :bind ("C-c f" . flymake-show-buffer-diagnostics)
  :hook (prog-mode . flymake-mode)
  :init (setq flymake-no-changes-timeout nil
              flymake-fringe-indicator-position 'right-fringe)
  :config
  ;; Check elisp with `load-path'
  (defun my-elisp-flymake-byte-compile (fn &rest args)
    "Wrapper for `elisp-flymake-byte-compile'."
    (let ((elisp-flymake-byte-compile-load-path
           (append elisp-flymake-byte-compile-load-path load-path)))
      (apply fn args)))
  (advice-add 'elisp-flymake-byte-compile :around #'my-elisp-flymake-byte-compile))

;; (use-package sideline-flymake
;;   :ensure nil
;;   :diminish sideline-mode
;;   :hook (flymake-mode . sideline-mode)
;;   :init (setq sideline-flymake-display-mode 'point
;;               sideline-backends-right '(sideline-flymake)))

(provide 'init-lang-check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-check.el ends here
