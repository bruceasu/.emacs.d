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
  (use-package all-the-icons
    ;; :ensure t
    :load-path "~/.emacs.d/site-lisp/all-the-icons"
    )
  ;; 浮动窗口支持
  (use-package posframe :ensure t)
)
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


(require 'load-set-font)

;; Line and Column
(setq-default fill-column 65)
(setq column-number-mode t)
(defun buffer-too-big-p ()
  "Check whether the buffer is too big."
  (or (> (buffer-size) (* 5000 80))
      (> (line-number-at-pos (point-max)) 5000)))

(add-hook 'prog-mode-hook
          (lambda ()
            ;; turn off `linum-mode' when there are more than 5000 lines
            (if (buffer-too-big-p) (linum-mode -1))))

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
