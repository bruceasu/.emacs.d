;; init-basic.el --- Initialize basic configurations.	-*- lexical-binding: t -*-

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
;; Basic configuration.
;;

;;; Code:

(eval-when-compile
  (require '+const)
  (require '+custom)
  (require 'subr-x)
  )

;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;; 去除默认启动界面
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen t)
;; 关闭工具栏
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; 关闭菜单栏
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Personal information
(setq user-full-name suk-full-name)
(setq user-mail-address suk-mail-address)

(setq-default major-mode 'text-mode
               fill-column 80
              tab-width 4
              c-basic-offset 4
              indent-tabs-mode nil)     ;; Permanently indent with spaces, never with TABs

;; follow symlinks
(setq vc-follow-symlinks t)

;;只渲染当前屏幕语法高亮，加快显示速度
(setq font-lock-maximum-decoration t)
(setq initial-scratch-message nil)
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
;; Repeating C-SPC after popping mark pops it again
(setq set-mark-command-repeat-pop t)
(setq-default major-mode 'text-mode)
;; 设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插 入两个空格。
(setq sentence-end "\\([。！？￥%×（）—]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
;; 让'_'被视为单词的一部分
(add-hook 'after-change-major-mode-hook (lambda ()(modify-syntax-entry ?_ "w")))
;; "-" 同上)
(add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?- "w")))
(setq sentence-end-double-space nil)

;; 更友好及平滑的滚动
(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)


;; Browse URL
;;;###autoload
(defun suk-webkit-browse-url (url &optional pop-buffer new-session)
  "Browse URL with xwidget-webkit' and switch or pop to the buffer.

  POP-BUFFER specifies whether to pop to the buffer.
  NEW-SESSION specifies whether to create a new xwidget-webkit session."
  (interactive (progn
                 (require 'browse-url)
                 (browse-url-interactive-arg "xwidget-webkit URL: ")))
  (or (featurep 'xwidget-internal)
      (user-error "Your Emacs was not compiled with xwidgets support"))

  (xwidget-webkit-browse-url url new-session)
  (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
    (when (buffer-live-p buf)
      (and (eq buf (current-buffer)) (quit-window))
      (if pop-buffer
          (pop-to-buffer buf)
        (switch-to-buffer buf)))))



(autoload 'calendar "init-calendar" "Config Chinese calendar " t)

;; Increase how much is read from processes in a single chunk (default is 4kb)
(setq read-process-output-max #x10000)  ; 64kb


;;====================================================
;; 编码设置 begin
;;====================================================
;; Set UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)            ;缓存文件编码
(setq default-file-name-coding-system 'utf-8-unix)              ;文件名编码
(setq default-keyboard-coding-system 'utf-8-unix)               ;键盘输入编码
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)) ;进程输出输入编码
(setq default-sendmail-coding-system 'utf-8-unix)               ;发送邮件编码
(setq default-terminal-coding-system 'utf-8-unix)               ;终端编码


(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; 23.2 之后废弃，用buffer-file-coding-system
;;(setq default-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)
(setq session-save-file-coding-system 'utf-8)

;; 据说设置为UTF-8不会卡顿
(set-language-environment "UTF-8")
;; (set-language-environment 'Chinese-GB)

;; 重要提示:写在最后一行的，实际上最优先使用; 最前面一行，反而放到最后才识别。
;; utf-16le-with-signature 相当于 Windows 下的 Unicode 编码，这里也可写成
;; utf-16 (utf-16 实际上还细分为 utf-16le, utf-16be, utf-16le-with-signature等多种)
;; Unicode
;; (prefer-coding-system 'utf-16le-with-signature)
;; (prefer-coding-system 'utf-16)
;; (prefer-coding-system 'utf-8-dos)
;; 新建文件以utf-8编码，行末结束符平台相关
(prefer-coding-system 'utf-8)

(when sys/win32p
   (setq w32-unicode-filenames t) ; 启用 Unicode 文件名支持
  (setq file-name-coding-system 'utf-8) ; 设置文件名编码为 UTF-8
  (setq locale-coding-system 'utf-8) ; 设置区域设置编码为 UTF-8

  ;; 简体
  ;;(prefer-coding-system 'gb2312)
  ;;(prefer-coding-system 'cp936)
  ;;(prefer-coding-system 'gb18030)
  ;;(setq file-name-coding-system 'gb18030)
  ;;(setq locale-coding-system 'gb18030)    ; 此句保证中文字体设置有效

  ;; 繁体
  ;; (prefer-coding-system 'cp950)
  ;; (prefer-coding-system 'big5-hkscs)
  ;; (setq file-name-coding-system 'big5-hkscs) ; Hong Kong and Taiwan
  ;; (setq locale-coding-system 'big5-hkscs)

  ;; (setq file-name-coding-system 'cp932)      ; Japanese
  ;; (setq locale-coding-system 'cp932)

  ;; Key Modifiers
  ;; make PC keyboard's Win key or other to type Super or Hyper
  ;; (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)    ; Left Windows key
  (setq w32-apps-modifier 'hyper)       ; Menu/App key
  ;; w32-register-hot-key 在 Emacs 中是用来在Windows系统上注册全局热键的函数，
  ;; 但它并不直接关联到执行 Emacs Lisp 函数。
  ;; 这个函数更多的是告诉Windows操作系统，
  ;; “当这个按键组合被按下时，应该通知Emacs”。
  ;; 要使Emacs在按下这个热键时执行特定的Elisp函数，还需要在Emacs内部设置相应的
  ;; 响应机制。这通常涉及到编写一些额外的Elisp代码来监听这个热键，
  ;; 并在它被按下时触发相应的操作。
  ;; 实际上，w32-register-hot-key 更多地用于在操作系统级别处理特定的按键组合，
  ;; 而不是在Emacs的编辑环境内。如果您想在Emacs内部绑定热键并执行函数，
  ;; 通常会使用像 global-set-key 或 define-key 这样的函数。
  (w32-register-hot-key [s-t])
  ;; scroll-bar
  (set-scroll-bar-mode 'right)
  ;; Optimization
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-use-native-image-API t         ; use native w32 API
        w32-pipe-read-delay 0              ; faster IPC
        w32-pipe-buffer-size 65536)       ; read more at a time (64K, was 4K)
  )

;; Unix like OS.
(unless sys/win32p
   ;; 新建文件使用utf-8-unix方式
   (prefer-coding-system 'utf-8-unix)
   (setq system-time-locale "C")
   (set-selection-coding-system 'utf-8))

(unless sys/macp
  (setq command-line-ns-option-alist nil))

(unless sys/linuxp
  (setq command-line-x-option-alist nil))



(defun childframe-workable-p ()
  "Whether childframe is workable."
  (not (or noninteractive
           emacs-basic-display
           (not (display-graphic-p)))))

(defun childframe-completion-workable-p ()
  "Whether childframe completion is workable."
  (and (eq suk-completion-style 'childframe)
       (childframe-workable-p)))

(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and suk-icon
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))

(defun suk-treesit-available-p ()
  "Check whether tree-sitter is available.
Native tree-sitter is introduced since 29.1."
  (and suk-tree-sitter
       (fboundp 'treesit-available-p)
       (treesit-available-p)))

(defun too-long-file-p ()
  "Check whether the file is too long."
  (or (> (buffer-size) 100000)
      (and (fboundp 'buffer-line-statistics)
           (> (car (buffer-line-statistics)) 10000))))

;; Increase selected region by semantic units
(defun suk-treesit-available-p ()
  "Check whether tree-sitter is available.
Native tree-sitter is introduced since 29.1."
  (and suk-tree-sitter
	   (fboundp 'treesit-available-p)
	   (treesit-available-p)))


;; GUI Environment
(when (display-graphic-p)
  (progn
	;; 隐藏垂直滚动条。
	;; 其实在有鼠标的环境，阅读文档时，使用滚动条有时会轻松一点。
	;;  (modify-all-frames-parameters '((vertical-scroll-bars)))
	)
  )

;; =========================================================
;; 通过编辑配置文件使其可以调用外部程序，来为其添加功能。
;; 增加命令
;;(defun lxr (names)
;;  (interactive "s查找联系人，请输入条件：")
;;  (call-process-shell-command "lxr" nil t t "-s" names))
;;执行命令
;;首先按功能键，Alt+x，然后输入命令 lxr 。
;;系统提示：“查找联系人，请输入条件："。
;;输入完成后，emacs 会执行命令lxr -s names，并输出执行的结果。
;; =========================================================
(provide 'init-basic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; init-basic.el ends here
