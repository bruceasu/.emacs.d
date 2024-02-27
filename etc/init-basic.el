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
(menu-bar-mode -1)
(when (fboundp 'tooltip-mode) (tooltip-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Personal information
(setq user-full-name suk-full-name)
(setq user-mail-address suk-mail-address)

;; follow symlinks
(setq vc-follow-symlinks t)
;; enable winner mode globally for undo/redo window layout changes
(winner-mode t)

(show-paren-mode t)

;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
(setq suggest-key-bindings 1)
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

;; Misc
(fset 'yes-or-no-p 'y-or-n-p)
;; 忽略 cl 过期警告
(setq byte-compile-warnings '(cl-function))

;; Frequently-accessed files

;; Registers allow you to jump to a file or other location quickly.
;; To jump to a register, use C-x r j followed by the letter of the register.
;; Using registers for all these file shortcuts is probably a bit of
;; a waste since I can easily define my own keymap, but since I rarely
;; go beyond register A anyway. Also, I might as well add shortcuts for refiling.

(require 'bookmark)
(defvar my-refile-map (make-sparse-keymap))
(defmacro my-defshortcut (key file)
  `(progn
     (set-register ,key (cons 'file ,file))
     (define-key my-refile-map
       (char-to-string ,key)
       (lambda (prefix)
         (interactive "p")
         (let ((org-refile-targets '(((,file) :maxlevel . 6)))
               (current-prefix-arg (or current-prefix-arg '(4))))
           (call-interactively 'org-refile))))))

;;(define-key my-refile-map "," 'my-org-refile-to-previous-in-file)
(my-defshortcut ?e "~/.emacs.d/init.el")
(my-defshortcut ?E "~/.emacs.d/custom.el")
;; (my-defshortcut ?i "~/cloud/orgzly/Inbox.org")
;; (my-defshortcut ?o "~/cloud/orgzly/organizer.org")
;; (my-defshortcut ?s "~/personal/sewing.org")
;; (my-defshortcut ?b "~/personal/business.org")
;; (my-defshortcut ?p "~/personal/google-inbox.org")
;; (my-defshortcut ?P "~/personal/google-ideas.org")
;; (my-defshortcut ?B "~/Dropbox/books")
(my-defshortcut ?n "~/notes")
;; (my-defshortcut ?N "~/sync/notes/QuickNote.md")
;; (my-defshortcut ?w "~/Dropbox/public/sharing/index.org")
;; (my-defshortcut ?W "~/Dropbox/public/sharing/blog.org")
;; (my-defshortcut ?j "~/personal/journal.org")
;; (my-defshortcut ?J "~/cloud/a/Journal.csv")
;; (my-defshortcut ?I "~/Dropbox/Inbox")
;; (my-defshortcut ?g "~/sachac.github.io/evil-plans/index.org")
;; (my-defshortcut ?c "~/code/dev/elisp-course.org")
;; (my-defshortcut ?C "~/personal/calendar.org")
;; (my-defshortcut ?l "~/dropbox/public/sharing/learning.org")
;; (my-defshortcut ?q "~/sync/notes/QuickNote.md")
;; (my-defshortcut ?Q "~/personal/questions.org")

;;;###autoload
(defun term()
  ;; bash in windows.
  (interactive)
  (if sys/win32p
	  (let (
			(explicit-shell-file-name windows-bash-path)
			)
		(call-interactively 'shell))
  (let ((explicit-shell-file-name "/bin/bash"))
	(call-interactively 'shell)))
)

;;;###autoload
(defun github-code-search ()
  "Search code on github for a given language."
  (interactive)
  (let ((language (completing-read
                   "Language: "
                   '("Java" "C/C++" "Emacs Javascript" "Lisp"  "Python" "Rust")))
        (code (read-string "Code: ")))
    (browse-url
     (concat "https://github.com/search?l=" language
             "&type=code&q=" code))))

;;;###autoload
(defun google-search-str (str)
  (browse-url
   (concat "https://www.google.com/search?q=" str)))

;;;###autoload
(defun google-search ()
  "Google search region, if active, or ask for search string."
  (interactive)
  (if (region-active-p)
      (google-search-str
       (buffer-substring-no-properties (region-beginning)
                                       (region-end)))
    (google-search-str (read-from-minibuffer "Search: "))))


;; 切换透明
;;;###autoload
(defun suk/toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; Also handle undocumented (<active> <inactive>) form.
                    ((numberp (cadr alpha)) (cadr alpha)))
              100)
         '(85 . 85) '(100 . 100)))))


;; 增加或减少透明
;;;###autoload
(defun suk/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

;; 增加透明度
;;;###autoload
(defun suk/less-alpha ()
  (interactive)
  (suk/adjust-opacity nil -2))
;; 减少透明度
;;;###autoload
(defun suk/plus-alpha ()
  (interactive)
  (suk/adjust-opacity nil 2))
;; set the alpha background
(setq-default alpha-list '((90 100) (100 100)))

;;;###autoload
(defun loop-alpha ()
  ;;doc
  (interactive)
  (let ((h (car alpha-list)))
    ((lambda (a ab)
       (set-frame-parameter (selected-frame) 'alpha (list a ab))
       (add-to-list 'default-frame-alist (cons 'alpha (list a ab)))
       ) (car h) (car (cdr h)))
    (setq alpha-list (cdr (append alpha-list (list h))))
    )
)
;; (loop-alpha)

(when  (eq system-type 'gnu/linux)
;; 调节屏幕亮度
;;;###autoload
  (defun suk/set-backlight (&optional light-value)
    (interactive "s请输入亮度(小数表示的百分比): ")
    (let ((max-backlight (string-to-number (string-trim-right
					    (shell-command-to-string
					     "cat /sys/class/backlight/intel_backlight/max_brightness")))))
      (when (and light-value (floatp (string-to-number light-value)))
	(shell-command
	 (concat "echo "
		 (format "%d" (* max-backlight (string-to-number light-value)))
		 " > /sys/class/backlight/intel_backlight/brightness")))))

;; 增加10%屏幕亮度
;;;###autoload
  (defun suk/plus-backlight ()
    (interactive)
    (let* (
	   ;; 最大亮度
	   (max-backlight (string-to-number (string-trim-right
					     (shell-command-to-string "cat /sys/class/backlight/intel_backlight/max_brightness"))))
	   ;; 当前亮度
	   (current-backlight (string-to-number (string-trim-right
						 (shell-command-to-string "cat /sys/class/backlight/intel_backlight/brightness"))))
	   ;; 增加后的亮度
	   (add-backlight (+ current-backlight (* max-backlight 0.1))))
      (if (< add-backlight max-backlight)
	  (progn (shell-command
		  (concat "echo "
			  (format "%d" add-backlight)
			  " > /sys/class/backlight/intel_backlight/brightness"))
		 (message "亮度+10%"))
	(message "亮度MAX!!"))))

;; 减少屏幕亮度
;;;###autoload
  (defun suk/less-backlight ()
    (interactive)
    (let* (
	   ;; 最大亮度
	   (max-backlight (string-to-number (string-trim-right
					     (shell-command-to-string "cat /sys/class/backlight/intel_backlight/max_brightness"))))
	   ;; 当前亮度
	   (current-backlight (string-to-number (string-trim-right
						 (shell-command-to-string "cat /sys/class/backlight/intel_backlight/brightness"))))
	   ;; 减少后的亮度
	   (less-backlight (- current-backlight (* max-backlight 0.1))))
      (if (> less-backlight (* max-backlight 0.1) )
	  (progn (shell-command
		  (concat "echo "
			  (format "%d" less-backlight)
			  " > /sys/class/backlight/intel_backlight/brightness"))
		 (message "亮度-10%"))
	(message "亮度Min!!"))))
)


;; ==============================================================
;; Network Proxy
;; --------------------------------------------------------------
;;;###autoload
(defun suk/proxy-http-show ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is \"%s\"" suk-proxy)
    (message "No proxy")))

;;;###autoload
(defun suk/proxy-http-enable ()
  "Enable http/https proxy."
  (interactive)
  (setq url-proxy-services `(("http" . suk-proxy)
                             ("https" . suk-proxy)
                             ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (suk/proxy-http-show))

;;;###autoload
(defun suk/proxy-http-disable ()
  "Disable http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (suk/proxy-http-show))

;;;###autoload
(defun suk/proxy-http-toggle ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
      (suk/proxy-http-disable)
    (suk/proxy-http-enable)))

;;;###autoload
(defun suk/proxy-socks-enable ()
  "Enable Socks proxy."
  (interactive)
  (setq url-gateway-method 'socks)
  (setq socks-noproxy '("localhost"))
  (setq socks-server '("Default server" "127.0.0.1" 1080 5))
  (message "Enable socks proxy."))

;;;###autoload
(defun suk/proxy-socks-disable ()
  "Disable Socks proxy."
  (interactive)
  (setq url-gateway-method 'native)
  (setq socks-noproxy nil)
  (message "Disable socks proxy."))

;;;###autoload
(defun now ()
  (interactive)
  ( insert (org-time-stamp)))

(autoload 'calendar "init-calendar" "Config Chinese calendar " t)



;; 编码设置 begin
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
;; 新建文件以utf-8编码，行末结束符平台相关
(prefer-coding-system 'utf-8)

(if sys/win32p
   ;; (setq file-name-coding-system 'cp932)      ;; Japanese
   ;; (setq file-name-coding-system 'big5-hkscs) ;; Hong Kong and Taiwan
   ;; (setq file-name-coding-system 'euc-cn)
   (setq file-name-coding-system 'gb18030)
   (setq locale-coding-system 'gb18030)    ; 此句保证中文字体设置有效
   (setq w32-unicode-filenames 'nil)       ; 确保file-name-coding-system变量的设置不会无效
   ;; (setq-default pathname-coding-system 'euc-cn)
   ;; 简体
   ;;(prefer-coding-system 'gb2312)
   ;;(prefer-coding-system 'cp936)
   ;;(prefer-coding-system 'gb18030)

   ;; 繁体
   ;; (prefer-coding-system 'cp950)
   ;; (prefer-coding-system 'big5-hkscs)

   ;; Unicode
   ;; (prefer-coding-system 'utf-16le-with-signature)
   ;; (prefer-coding-system 'utf-16)
   ;; (prefer-coding-system 'utf-8-dos)
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
)

;; Unix like OS.
(unless sys/win32p
   ;; 新建文件使用utf-8-unix方式
   (prefer-coding-system 'utf-8-unix)
   (setq system-time-locale "C")
)

;; GUI Environment
(when (or sys/mac-x-p sys/linux-x-p sys/win32p)
	(progn
	;; 隐藏垂直滚动条。
	;; 其实在有鼠标的环境，阅读文档时，使用滚动条有时会轻松一点。
	;;  (modify-all-frames-parameters '((vertical-scroll-bars)))
	)
)


(require 'file-encoding)



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
