;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Suk

;; Author: Suk

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
;; Emacs HEAD (27+) introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold 80000000)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Faster to disable these here (before they've been initialized)
;(unless (and (display-graphic-p) (eq system-type 'darwin))
;  (setq menu-bar-mode nil))
(setq tool-bar-mode nil)
;(setq scroll-bar-mode nil)
(modify-all-frames-parameters '((vertical-scroll-bars)))



(setq system-time-locale "C")
;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
(setq suggest-key-bindings 1)
;;只渲染当前屏幕语法高亮，加快显示速度
(setq font-lock-maximum-decoration t)
;; 选中文本后输入会覆盖
(delete-selection-mode 1)
(auto-compression-mode 1)
(size-indication-mode 1)
(blink-cursor-mode -1)
;; 高亮对应的括号
(show-paren-mode 1)
;; --------------------------------------------------------------
;;备份策略
;; --------------------------------------------------------------
(setq backup-directory-alist '(("" . "~/tmp/emacs/backup")))
(setq make-backup-files t)
;; 允许多次备份
(setq version-control t)
;; 保留最早的2个备份文件
(setq kept-old-versions 2)
;; 保留最近的6个备份文件
(setq kept-new-versions 10)
;; 自动删除旧的备份文件
(setq delete-old-versions t)

;; 更友好及平滑的滚动
(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)


;; (setq initial-scratch-message nil)
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq kill-whole-line t)                   ; C-k deletes the end of line
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
(setq-default major-mode 'text-mode)

;; 设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插 入两个空格。
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq kill-ring-max 200)
;; 关闭自动调节行高
(setq auto-window-vscroll nil)
;; 让光标无法离开视线
(setq mouse-yank-at-point nil)
;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)
;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode t)

;; 创建新行的动作
;; 回车时创建新行并且对齐
(global-set-key (kbd "RET") 'newline-and-indent)
;; 取消对齐创建的新行
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)
;; 让'_'被视为单词的一部分
(add-hook 'after-change-major-mode-hook (lambda ()(modify-syntax-entry ?_ "w")))
;; "-" 同上)
(add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?- "w")))

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)

(setq visible-bell t)
(setq track-eol t)                      ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)
(setq inhibit-compacting-font-caches t) ; Don’t compact font caches during GC.
;; 关闭工具栏
(tool-bar-mode -1)

;; 关闭菜单栏
(menu-bar-mode -1)

;; 设置光标样式
(setq-default cursor-type 'box)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
