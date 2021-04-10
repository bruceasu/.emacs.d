;;; init-ui.el --- Initialize ui configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Suk
;;
;; Author: Suk
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Visual (UI) configurations.
;;

;;; Code:

(eval-when-compile
  (require '+const)
  (require '+custom))

;; 去除默认启动界面
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen t)
;; 关闭工具栏
(tool-bar-mode -1)
;; 关闭菜单栏
(menu-bar-mode -1)
(require 'lazycat-theme)
(lazycat-theme-load-dark)
;; 简洁的mode-line
;;(require 'awesome-tray)
;;(awesome-tray-mode 1)
;; 懒猫(王勇)的超简洁modeline
(use-package awesome-tray
   ;;:disabled
   :load-path "~/.emacs.d/site-lisp/awesome-tray"
   :hook (after-init . awesome-tray-mode))

;; 高亮当前行
;; Highlight the current line
(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

;; 切换buffer焦点时高亮动画
(use-package beacon
  :ensure t
  :hook (after-init . beacon-mode))

;; Title
(setq frame-title-format
      '("Emacs - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; ===================================
;; Theme
;; -----------------------------------
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(defun is-doom-theme-p (theme)
  "Check whether the THEME is a doom theme. THEME is a symbol."
  (string-prefix-p "doom" (symbol-name theme)))

(defun suk-load-theme (theme)
  "Set color THEME. Infact, only support lazycat-theme-load-dark"
  (interactive
    (require 'lazycat-theme)
	(lazycat-theme-load-dark)
	)
  )



;; 这是 purcell 写的一个插件，按照描述来看就是把 ^L显示为一个整洁的水平线。
;; 这个^L其实并不是^与L的组合，而是一个单一的字符。我查了一下，很可能这个代表的
;; 意思是软回车。C-q C-L 来输入。
(use-package page-break-lines
  :ensure t
  :hook ('after-init .  'page-break-lines-mode))

;; 图形界面插件的设置
(when (display-graphic-p)
  ;; 图标支持
  (use-package all-the-icons :ensure t)

  ;; dired模式图标支持
  ;; (use-package all-the-icons-dired
  ;; 	:ensure t
  ;; 	:hook ('dired-mode . 'all-the-icons-dired-mode))

  ;; 表情符号
  ;; (use-package emojify
  ;; 	:after telega
  ;; 	:custom (emojify-emojis-dir "~/.emacs.d/var/emojis")
  ;; 	:config
  ;; 	(global-emojify-mode))
  ;;
  ;; 浮动窗口支持
  (use-package posframe :ensure t)

  ;; 感觉变得凌乱，还是正常使用好。
  ;; 缩进线
  ;; (use-package indent-guide
  ;;   :ensure t
  ;;   :hook (prog-mode . indent-guide-mode))

  ;; Highlight indentions
  ;; (use-package highlight-indent-guides
  ;; 	:disabled
  ;; 	:diminish
  ;; 	:hook (prog-mode . highlight-indent-guides-mode)
  ;; 	:config
  ;; 	(setq highlight-indent-guides-method 'character)
  ;; 	(setq highlight-indent-guides-responsive t))



)



;; Highlight uncommitted changes
(use-package diff-hl
  :defines desktop-minor-mode-table
  :commands diff-hl-magit-post-refresh
  :custom-face
  (diff-hl-change ((t (:background "#46D9FF"))))
  (diff-hl-delete ((t (:background "#ff6c6b"))))
  (diff-hl-insert ((t (:background "#98be65"))))
  :bind (:map diff-hl-command-map
              ("SPC" . diff-hl-mark-hunk))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq diff-hl-draw-borders nil)
  (setq fringes-outside-margins t)
  (set-fringe-mode '(4 . 8))

  (unless (display-graphic-p)
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil))))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; Highlight matching paren
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren t)
  (setq show-paren-when-point-in-periphery t))

;; 括号匹配
(use-package smartparens
  :ensure t
  :hook ('prog-mode . 'smartparens-global-mode))

;; Mode-line
(defun mode-line-height ()
   "Get current height of mode-line."
   (- (elt (window-pixel-edges) 3)
     (elt (window-inside-pixel-edges) 3)))

(use-package hide-mode-line
  :hook (((completion-list-mode
           completion-in-region-mode
           neotree-mode
           treemacs-mode)
          . hide-mode-line-mode)))

;; 解决卡顿
(setq inhibit-compacting-font-caches t) ;

;; Fonts
(when (or sys/mac-x-p sys/linux-x-p)
  (set-frame-font "Noto Sans CJK SC-12"))

(when sys/win32p
  (set-frame-font "Simsun 12"))

(require 'load-set-font)

(when (and suk-cnfonts (display-graphic-p))
  ;; cnfonts doesn't support terminal
  (use-package cnfonts
    :hook (after-init . cnfonts-enable)
    :config
    ;; NOTE: on macOS, the frame size is changed during the startup without below.
    ;; Keep frame size
    (setq cnfonts-keep-frame-size nil)
    (add-hook 'window-setup-hook
              (lambda ()
                (setq cnfonts-keep-frame-size t)))

    ;; Set profiles
    (setq cnfonts-use-cache t)
    (setq cnfonts-profiles
          '("program" "org-mode" "read-book"))
    (setq cnfonts--profiles-steps '(("program-normal" . 4)
                                    ("org-mode" . 6)
                                    ("read-book" . 8)))))

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


;; Line and Column
(setq-default fill-column 65)
(setq column-number-mode t)
;; (setq line-number-mode t)

;; 相对行号，默认未开启，价值不是很大。
;; (use-package linum-relative
;;   :ensure t
;;   :disabled
;;   :hook ('prog-mode . 'linum-relative-mode))

(defun buffer-too-big-p ()
  "Check whether the buffer is too big."
  (or (> (buffer-size) (* 5000 80))
      (> (line-number-at-pos (point-max)) 5000)))

(add-hook 'prog-mode-hook
          (lambda ()
            ;; turn off `linum-mode' when there are more than 5000 lines
            (if (buffer-too-big-p) (linum-mode -1))))

;; ;; Show native line numbers if possible, otherwise use linum
;; (if (fboundp 'display-line-numbers-mode)
;;     (use-package display-line-numbers
;;       :ensure nil
;;       :hook (prog-mode . display-line-numbers-mode))
;;   (use-package linum-off
;;     :demand
;;     :defines linum-format
;;     :hook (after-init . global-linum-mode)
;;     :config
;;     (setq linum-format "%4d ")

;;     ;; Highlight current line number
;;     (use-package hlinum
;;       :disabled
;;       :ensure nil
;;       :defines linum-highlight-in-all-buffersp
;;       :hook (global-linum-mode . hlinum-activate)
;;       :init
;;       (setq linum-highlight-in-all-buffersp t)
;;       (custom-set-faces
;;        `(linum-highlight-face
;;          ((t (:inherit 'default :background ,(face-background 'default) :foreground ,(face-foreground 'default)))))))
;;     ))

;; Mouse & Smooth Scroll
;; Scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil
      scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

;; Toggle fullscreen <F11> also bind to fullscreen
(bind-keys ("C-<f11>" . toggle-frame-fullscreen)
           ("C-S-f" . toggle-frame-fullscreen) ; Compatible with macOS
           ("M-S-<return>" . toggle-frame-fullscreen) ; Compatible with Windos
		   )

;; for windows settings
(when (eq system-type 'windows-nt)
  (setq locale-coding-system 'gb18030)    ; 此句保证中文字体设置有效
  (setq w32-unicode-filenames 'nil)       ; 确保file-name-coding-system变量的设置不会无效
  (setq file-name-coding-system 'gb18030) ; 设置文件名的编码为gb18030

)

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
