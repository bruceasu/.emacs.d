;;; early-init --- early initial
;;; Commentary:
(provide 'early-init)



;; Defer garbage collection further back in the startup process

(defvar my-computer-has-smaller-memory-p nil
  "Installing&Compiling many packages could cost too much memory.")

;; @see https://www.reddit.com/r/emacs/comments/ofhket/further_boost_start_up_time_with_a_simple_tweak/
(unless my-computer-has-smaller-memory-p
;; Defer garbage collection further back in the startup process
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6)
)

(add-hook 'after-init-hook (lambda ()
			     (setq gc-cons-threshold 800000)))


;; Prevent unwanted runtime compilation for gccemacs (native-comp) users;
;; packages are compiled ahead-of-time when they are installed and site files
;; are compiled when gccemacs is installed.
(setq native-comp-deferred-compilation nil ;; obsolete since 29.1
      native-comp-jit-compilation nil)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

(setq inhibit-startup-message t)

;; In Emacs 27+, package initialization occurs before `user-init-file' is
;; loaded, but after `early-init-file'. Doom handles package initialization, so
;; we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)
;; Do not allow loading from the package cache (same reason).
(setq package-quickstart nil)
(setq load-prefer-newer noninteractive)
;; `use-package' is builtin since 29.
;; It must be set before loading `use-package'.
(setq use-package-enable-imenu-support t)


;; System default coding
(set-language-environment 'utf-8)

;; Cleaner GUI
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(setq-default mode-line-format nil)
(toggle-frame-maximized)
(require 'subr-x)
;;(global-unset-key (kbd "C-SPC"))
