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


;; Misc.
(setq confirm-kill-emacs 'y-or-n-p)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   (quote
    ((eval progn
           (defun my-org-download-method
               (link)
             (let
                 ((filename
                   (file-name-nondirectory
                    (car
                     (url-path-and-query
                      (url-generic-parse-url link)))))
                  (dirname
                   (file-name-sans-extension
                    (buffer-name))))
               (unless
                   (file-exists-p dirname)
                 (make-directory dirname))
               (expand-file-name filename dirname)))
           (setq-local org-download-method
                       (quote my-org-download-method)))))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit (quote font-lock-keyword-face) :height 3.0))))
 '(aw-mode-line-face ((t (:inherit (quote mode-line-emphasis) :bold t))))
 '(diff-hl-change ((t (:background "#46D9FF"))))
 '(diff-hl-delete ((t (:background "#ff6c6b"))))
 '(diff-hl-insert ((t (:background "#98be65"))))
 '(hl-todo ((t (:box t :bold t))))
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button))))))

;;; custom.el ends here
