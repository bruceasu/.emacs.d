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
  (require '+custom))

;; Key Modifiers
(when sys/win32p
  ;; make PC keyboard's Win key or other to type Super or Hyper
  ;; (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)    ; Left Windows key
  (setq w32-apps-modifier 'hyper)       ; Menu/App key
  ;; (w32-register-hot-key [s-])
  (w32-register-hot-key [s-t])
  ;; scroll-bar
  (set-scroll-bar-mode 'right)
  )


;; Environment
(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; Personal information
(setq user-full-name suk-full-name)
(setq user-mail-address suk-mail-address)


;; 隐藏垂直滚动条。
;; 其实在有鼠标的环境，阅读文档时，使用滚动条有时会轻松一点。
;;  (modify-all-frames-parameters '((vertical-scroll-bars)))

(setq system-time-locale "C")
;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
(setq suggest-key-bindings 1)
;;只渲染当前屏幕语法高亮，加快显示速度
(setq font-lock-maximum-decoration t)


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
;;(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
(setq-default major-mode 'text-mode)

;; 设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插 入两个空格。
(setq sentence-end "\\([。！？￥%×（）—]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
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
(setq track-eol t)                      ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)
(setq inhibit-compacting-font-caches t) ; Don’t compact font caches during GC.
;; 设置光标样式
(setq-default cursor-type 'box)
;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; 忽略 cl 过期警告
(setq byte-compile-warnings '(cl-function))

;; =========================================================
;; Start server
(use-package server
  :ensure nil
  :defer 1
  :hook (after-init . server-mode))

;; Emacs可以做为一个server, 然后用emacsclient连接这个server,
;; 无需再打开两个Emacs，windows下还不支持daemon的方式。

;;(server-force-delete)
;;(server-start)


;; 设置缓存文件/杂七杂八的文件存放的地址
;; 不好的做法
;; (setq user-emacs-directory "~/.emacs.d/var")

;; History
;; 回到关闭文件前光标的位置
(use-package saveplace
  :ensure nil
  :defer 1
  :hook (after-init . save-place-mode)
  :config (setq save-place-file "~/.emacs.d/var/saveplace")

  )

(use-package recentf
  :ensure nil
  :defer 1
  ;; lazy load recentf
  :hook (find-file . (lambda () (unless recentf-mode
                              (recentf-mode)
                              (recentf-track-opened-file))))
  :init
  (add-hook 'after-init-hook #'recentf-mode)
  (setq recentf-max-saved-items 500)
  (setq recentf-max-saved-items 17)
  (setq recentf-save-file "~/.emacs.d/var/recentf")
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude ".cache")
  (add-to-list 'recentf-exclude ".cask")
  (add-to-list 'recentf-exclude ".elfeed")
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "cache")
  (add-to-list 'recentf-exclude "persp-confs")
  (add-to-list 'recentf-exclude "recentf")
  (add-to-list 'recentf-exclude "url")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (defun simon-recentf-exclude-p (file)
  (let ((file-dir (file-truename (file-name-directory file))))
    (-any-p (lamdba (dir)
        (string-prefix-p dir file-dir))
        (mapcar 'file-truename (list var package-user-dir)))))
    (add-to-list 'recentf-exclude 'simon-recentf-exclude-p))

;; maybe cause slowly, so disabled.
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
               savehist-autosave-interval 300
               savehist-file "~/.emacs.d/var/history"))

; 设置amx保存文件的路径
(setq amx-save-file "~/.emacs.d/var/amx-items")
;; 设置自动保存路径前缀
(setq auto-save-list-file-prefix "~/.emacs.d/var/auto-save-list/.saves-")
;; 设置eshell历史记录
(setq eshell-history-file-name "~/.emacs.d/var/eshell/history")

;; projectitle-bookmarks
(setq projectile-known-projects-file "~/.emacs.d/var/projectile-bookmarks.eld")

;; --------------------------------------------------------------
;;备份策略
;; --------------------------------------------------------------
(setq backup-directory-alist '(("" . "~/tmp/emacs/backup")))
(setq make-backup-files t)
;; 允许多次备份
(setq version-control t)
;; 保留最早的2个备份文件
(setq kept-old-versions 2)
;; 保留最近的100个备份文件
(setq kept-new-version 100)
;; 自动删除旧的备份文件
(setq delete-old-versions t)

;; 开启行号显示
;; (global-linum-mode t)
;;(display-time-mode 1)

;; 如果有两个重名buffer, 则再前面加上路径区别
(require 'uniquify)
;; (setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; chmod +x
;; ref. http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#chmod
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

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
