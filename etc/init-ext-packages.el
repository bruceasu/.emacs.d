;;; init-ext-package.el --- Initialize Extension Packages configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Package management configurations.
;;

;;; Code:

(eval-when-compile
  (require '+const)
  (require '+custom)
  (require 'init-package)
  )

;; Persistent the scratch buffer
(use-package persistent-scratch
  :diminish
  :bind (:map persistent-scratch-mode-map
			  ([remap kill-buffer] . (lambda (&rest _)
									   (interactive)
									   (user-error "Scratch buffer cannot be killed")))
			  ([remap revert-buffer] . persistent-scratch-restore)
			  ([remap revert-this-buffer] . persistent-scratch-restore))
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode))
  :init (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
			  persistent-scratch-backup-directory
			  (expand-file-name "var/persistent-scratch" user-emacs-directory)))

;; Misc

(unless sys/win32p
  (use-package daemons)                 ; system services/daemons
  )
(provide 'init-ext-packages)
