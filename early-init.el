(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.8)

(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-message t)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'.
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)
;; `use-package' is builtin since 29.
;; It must be set before loading `use-package'.
(setq use-package-enable-imenu-support t)


;; System default coding
(set-language-environment 'utf-8)
;; Cleaner GUI
;;  (menu-bar-mode -1) ; keep the menu is better.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(toggle-frame-maximized)
 ;; kill buffer without my confirmation
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
(setq find-file-visit-truename t)
(provide 'early-init)
;;(global-unset-key (kbd "C-SPC"))
