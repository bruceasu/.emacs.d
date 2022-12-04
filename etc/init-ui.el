;;; init-ui.el --- Initialize ui configurations.    -*- lexical-binding: t -*-

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
;; 字体
(require 'load-set-font)

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
(require 'lazycat-theme)
(lazycat-theme-load-dark)


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

;; Line and Column
(linum-mode t)
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


(require 'sort-tab)
(sort-tab-mode 1)
;;Usage
;;sort-tab-select-next-tab: select next tab
;;sort-tab-select-prev-tab: select previous tab
;;sort-tab-select-first-tab: select first tab
;;sort-tab-select-last-tab: select last tab
;;sort-tab-close-current-tab: close current tab
(global-set-key (kbd "C-c 1") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-c 2") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-c 3") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-c 4") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-c 5") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-c 6") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-c 7") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-c 8") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-c 9") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-c 0") 'sort-tab-select-visible-tab)
(global-set-key (kbd "C-c q") 'sort-tab-close-mode-tabs)
(global-set-key (kbd "C-c Q") 'sort-tab-close-all-tabs)
(global-set-key (kbd "C-;") 'sort-tab-close-current-tab)
(global-set-key (kbd "C-x <left>") 'sort-tab-select-prev-tab)
(global-set-key (kbd "C-x <right>") 'sort-tab-select-next-tab)

(require 'awesome-tray)
(awesome-tray-mode 1)

;;(require 'awesome-tab)
;;(require 'all-the-icons)
;;(awesome-tab-mode t)

(provide 'init-ui)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui.el ends here
