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

;; Personal information
(setq user-full-name suk-full-name)
(setq user-mail-address suk-mail-address)

;; Key Modifiers
(when sys/win32p
  ;; make PC keyboard's Win key or other to type Super or Hyper
  ;; (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)    ; Left Windows key
  (setq w32-apps-modifier 'hyper)       ; Menu/App key
  ;; (w32-register-hot-key [s-])
  (w32-register-hot-key [s-t])
  ;; scroll-bar
  (set-scroll-bar-mode 'right))

;; Environment
(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))


;; =========================================================
;; Start server
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; Emacs可以做为一个server, 然后用emacsclient连接这个server,
;; 无需再打开两个Emacs，windows下还不支持daemon的方式。

;;(server-force-delete)
;; (server-start)

;; 图形化插件特殊设置
(if (not (display-graphic-p))
	(add-hook 'after-make-frame-functions
			  (lambda (new-frame)
				(select-frame new-frame)
				(dolist (elisp-code graphic-only-plugins-setting)
				  (eval elisp-code))))
	(dolist (elisp-code graphic-only-plugins-setting)
				  (eval elisp-code)))


;; ==============================================================
;; 这将从base添加所有第一级dirs并排除exclude-list中的dirs，
;; 而对于include-list中的dirs，它将添加该dir的所有第一级dirs。
;; (add-to-list 'load-path "~/.emacs.d/elpa/company-20170715.1035")
;; (add-to-list 'load-path "~/.local/site-lisp")
;;(suk/add-to-list-with-subdirs "~/.local/site-lisp"
;;                          '(".", "..")
;;                          '())
;; --------------------------------------------------------------
(defun suk/add-to-list-with-subdirs (base exclude-list include-list)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (member f exclude-list)))
        (add-to-list 'load-path name)
        (when (member f include-list)
          (add-to-list-with-subdirs name exclude-list include-list)))))
  (add-to-list 'load-path base))

;; ==============================================================
;; Update the git of emacs configuration
;; --------------------------------------------------------------
(defun suk-update-config ()
  "Update suk Emacs configurations to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (if (file-exists-p dir)
        (progn
          (message "Updating Emacs configurations...")
          (cd dir)
          (shell-command "git pull --rebase")
          (message "Update finished. Restart Emacs to complete the process."))
      (message "\"%s\" doesn't exist." dir))))

(declare-function upgrade-packages 'init-package)

(defalias 'suk-update-packages 'upgrade-packages)

(defun suk-update()
  "Update confgiurations and packages."
  (interactive)
  (suk-update-config)
  (suk-update-packages nil))

(declare-function upgrade-packages-and-restart 'init-package)
(defalias 'suk-update-packages-and-restart 'upgrade-packages-and-restart)
(defun suk-update-and-restart ()
  "Update configurations and packages, then restart."
  (interactive)
  (suk-update-config)
  (suk-update-packages-and-restart nil))

;; =========================================================
;; 方便的切换major mode
;; ---------------------------------------------------------
(defvar suk/switch-major-mode-last-mode nil)
;; ---------------------------------------------------------
(defun suk/major-mode-heuristic (symbol)
  (and (fboundp symbol)
       (string-match ".*-mode$" (symbol-name symbol))))
;; ---------------------------------------------------------
(defun suk/switch-major-mode (mode)
  "Change major mode to MODE."
  (interactive
  (let ((fn suk/switch-major-mode-last-mode) val)
    (setq val
          (completing-read
           (if fn (format "Change major-mode(default:%s): " fn) "Change major mode: ")
           obarray 'suk/major-mode-heuristic t nil nil (symbol-name fn)))
    (list (intern val))))
  (let ((last-mode major-mode))
    (funcall mode)
    (setq suk/switch-major-mode-last-mode last-mode)
	(message "Change to %s." major-mode))
)
;; ---------------------------------------------------------
;; show major mode
(defun suk/get-mode-name ()
  "显示`major-mode'及`mode-name'."
  (interactive)
  (message "major-mode:%s, mode-name:%s" major-mode mode-name))

(defun suk/toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0)))

;; =========================================================
;; 段落格式化
;; --------------------------------------------------------------
(defun suk/unfill-paragraph (&optional region)
    "Takes a multi-line paragraph (or REGION) and make it into a single line of text."
    (interactive (progn
                   (barf-if-buffer-read-only)
                   (list t)))
    (let ((fill-column (point-max)))
      (fill-paragraph nil region)))
(bind-key "M-Q" 'my/unfill-paragraph)

;; M-q will fill the paragraph normally, and C-u M-q will unfill it.
;; --------------------------------------------------------------
(defun suk/fill-or-unfill-paragraph (&optional unfill region)
    "Fill paragraph (or REGION).
With the prefix argument UNFILL, unfill it instead."
    (interactive (progn
                   (barf-if-buffer-read-only)
                   (list (if current-prefix-arg 'unfill) t)))
    (let ((fill-column (if unfill (point-max) fill-column)))
      (fill-paragraph nil region)))
(bind-key "M-q" 'suk/fill-or-unfill-paragraph)

;; ==============================================================
;; Recompile elpa directory
;; --------------------------------------------------------------
(defun suk/recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (byte-recompile-directory package-user-dir nil t))

;; Recompile site-lisp directory
;; --------------------------------------------------------------
(defun suk/recompile-site-lisp ()
  "Recompile packages in site-lisp directory."
  (interactive)
  (byte-recompile-directory
   (concat user-emacs-directory "site-lisp") 0 t))

;; 设置缓存文件/杂七杂八的文件存放的地址
;; 不好的做法
;; (setq user-emacs-directory "~/.emacs.d/var")

;; History
;; 回到关闭文件前光标的位置
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
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
;; (use-package savehist
;;   :ensure nil
;;   :hook (after-init . savehist-mode)
;;   :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
;;               history-length 1000
;;               savehist-additional-variables '(mark-ring
;;                                               global-mark-ring
;;                                               search-ring
;;                                               regexp-search-ring
;;                                               extended-command-history)
;;               savehist-autosave-interval 300
;;               savehist-file "~/.emacs.d/var/history))

; 设置amx保存文件的路径
(setq amx-save-file "~/.emacs.d/var/amx-items")
;; 设置自动保存路径前缀
(setq auto-save-list-file-prefix "~/.emacs.d/var/auto-save-list/.saves-")
;; 设置eshell历史记录
(setq eshell-history-file-name "~/.emacs.d/var/eshell/history")
  



(setq system-time-locale "C")
(auto-compression-mode 1)

;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
(setq suggest-key-bindings 1)
;;只渲染当前屏幕语法高亮，加快显示速度
(setq font-lock-maximum-decoration t)
;; 选中文本后输入会覆盖
(delete-selection-mode 1)

;; --------------------------------------------------------------
;;备份策略
;; --------------------------------------------------------------
(setq backup-directory-alist '(("" . "~/tmp/emacs/backup")))
(setq make-backup-files t)
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 10)
(setq delete-old-versions t)

;; 开启行号显示
;; (global-linum-mode t)
;;(display-time-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; 高亮对应的括号
(show-paren-mode 1)

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
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
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

;; chmod +x
;; ref. http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#chmod
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)
;; 让'_'被视为单词的一部分
(add-hook 'after-change-major-mode-hook (lambda () 
                                          (modify-syntax-entry ?_ "w")))
;; "-" 同上)
(add-hook 'after-change-major-mode-hook (lambda () 
                                          (modify-syntax-entry ?- "w")))

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
