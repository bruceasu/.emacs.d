;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

(setq suk-full-name "Bruce Asu")             ; User full name
(setq suk-mail-address "bruceasu@163.com")   ; Email address
;; (setq suk-proxy "127.0.0.1:1080")         ; Network proxy
(setq suk-package-archives 'tuna)            ; Package repo: melpa, melpa-mirror, emacs-china netease or tuna
(setq suk-cnfonts nil)                       ; Use cnfonts not: t or nil
(setq suk-enable-web t)

;; Misc.
(setq confirm-kill-emacs 'y-or-n-p)


;;; custom.el ends here
