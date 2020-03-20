;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Copy custom-template.el to custom.el and change the configurations, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

(setq suk-logo nil)                           ; Logo file or nil (official logo)
(setq suk-full-name "Bruce Asu")              ; User full name
(setq suk-mail-address "bruceasu@163.com")    ; Email address
;; (setq suk-proxy "127.0.0.1:1080")          ; Network proxy
(setq suk-package-archives 'emacs-china)    ; Package repo: melpa, melpa-mirror, emacs-china netease or tuna
;; (setq suk-theme 'classic)                  ; Color theme: default, classic, doom, dark, light or daylight
(setq suk-cnfonts t)                          ; Use cnfonts not: t or nil
;; (setq suk-dashboard nil)                   ; Use dashboard at startup or not: t or nil
;; (setq suk-lsp nil)                         ; Set LSP client: lsp-mode, eglot or nil
;; (setq suk-ivy-icon nil)                    ; Display icons in ivy or not: t or nil
;; (setq suk-pretty-magit nil)                ; Prettify magit or not: t or nil
;; (setq suk-company-enable-yas t)            ; Enable yasnippet for company or not: t or nil
;; (setq suk-benchmark t)                     ; Enable initialization benchmark or not: t or nil

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
;; (when (and (not suk-cnfonts) (display-graphic-p))
;;   ;; Set a default font
;;   (cond
;;    ((member "Source Code Pro" (font-family-list))
;;     (set-face-attribute 'default nil :font "Source Code Pro"))
;;    ((member "Menlo" (font-family-list))
;;     (set-face-attribute 'default nil :font "Menlo"))
;;    ((member "Monaco" (font-family-list))
;;     (set-face-attribute 'default nil :font "Monaco"))
;;    ((member "DejaVu Sans Mono" (font-family-list))
;;     (set-face-attribute 'default nil :font "DejaVu Sans Mono"))
;;    ((member "Consolas" (font-family-list))
;;     (set-face-attribute 'default nil :font "Consolas")))

;;   (cond
;;    (sys/mac-x-p
;;     (set-face-attribute 'default nil :height 130))
;;    (sys/win32p
;;     (set-face-attribute 'default nil :height 110)))

;;   ;; Specify font for all unicode characters
;;   (when (member "Symbola" (font-family-list))
;;     (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;;   ;; Specify font for chinese characters
;;   (cond
;;    ((member "WenQuanYi Micro Hei" (font-family-list))
;;     (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Micro Hei"))
;;    ((member "Microsoft Yahei" (font-family-list))
;;     (set-fontset-font t '(#x4e00 . #x9fff) "Microsoft Yahei")))
;;   )

(when (eq system-type 'windows-nt)
  (setq locale-coding-system 'gb18030)  ;此句保证中文字体设置有效
  (setq w32-unicode-filenames 'nil)       ; 确保file-name-coding-system变量的设置不会无效
  (setq file-name-coding-system 'gb18030) ; 设置文件名的编码为gb18030
  )
  
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
