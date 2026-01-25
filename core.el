(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.8)
;; Reset the GC setting
(add-hook 'emacs-startup-hook
	  (lambda ()
	    ;; default: 0.8MB
	    ;;(setq gc-cons-threshold 80000000)
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)
	    (defun my-cleanup-gc ()
	      "Clean up gc."
	      (setq gc-cons-threshold  67108864) ; 64M
	      (setq gc-cons-percentage 0.1) ; original value
	      (garbage-collect))
	    (run-with-idle-timer 4 nil #'my-cleanup-gc)
	    (add-hook 'focus-out-hook 'garbage-collect)))

(defconst sys/is-gui (display-graphic-p))
(defconst sys/is-tui (not sys/is-gui))
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(defconst emacs/>=30p
  (>= emacs-major-version 30)
  "Emacs is 30 or above.")

;; set const
(defconst custom-template-file
  (expand-file-name "custom-template.el" user-emacs-directory)
  "Custom template file of Suk's Emacs.")

(defconst suk-homepage
  "https://github.com/bruceasu/.emacs.d"
  "The Github page of this Emacs config.")


(defgroup suk nil
  "suk Emacs customizations."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/bruceasu/.emacs.d"))

(defcustom org-roam-directory (expand-file-name "RoamNotes" user-home-dir)
  "The org roam directory."
  :group 'suk
  :type 'string)

(defcustom org-files-directory (expand-file-name "org" user-home-dir)
  "The org roam directory."
  :group 'suk
  :type 'string)

(defcustom my-org-inline-css-file "~/.emacs.d/share/my-org-style-min.css"
  "The org css style file."
  :group 'suk
  :type 'string)

(defcustom suk-rime t
  "Display use emacs rime or not."
  :group 'suk
  :type 'boolean)

(defcustom suk-copilot-enable nil
  "Enable copilot mode"
  :group 'suk
  :type 'boolean)

;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-template-file
       (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
	   (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))

(load-file custom-file)
;;(if (file-exists-p custom-file)
;;    (load custom-file))

;; Load `custom-post.el'
;; Put personal configurations to override defaults here.
(add-hook 'after-init-hook
	  (lambda ()
	    (let ((file
		   (expand-file-name "custom-post.el" user-emacs-directory)))
	      (if (file-exists-p file)
		  (load file)))))

;; This sets up the load path so that we can override it
(setq warning-suppress-log-types '((package reinitialization)))
;; 指定ELPA目录
(setq package-user-dir (expand-file-name "elpa" "~/.local/share"))
(add-subdirs-to-load-path package-user-dir t)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

(setq package-check-signature nil) ; 个别时候会出现签名校验失败

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Should set before loading `use-package'
;; make use-package default behavior better
;; with `use-package-always-ensure' you won't need ":ensure t" all the time
;; with `use-package-always-defer' you won't need ":defer t" all the time
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  )

(require 'use-package)

;; Compatibility
(use-package compat :demand t)

(require 'lazy-load)
  ;;; ### Unset key ###
;;; --- 卸载按键
(lazy-load-unset-keys                   ;全局按键的卸载
 ;; '("C-z"  "s-W" "s-z" "M-h" "C-\\" "s-c" "s-x" "s-v"))
 '("C-z" ))

;; basic settings
(setq-default
 major-mode 'text-mode ; 默认使用text模式
 indent-tabs-mode nil  ; 默认只使用空格，不使用tab
 )
(server-mode 1)
(global-hl-line-mode 1)                    ;高亮当前行
(show-paren-mode t)                        ;显示括号匹配
(setq inhibit-startup-message t)           ; 关闭启动欢迎界面
(setq inhibit-startup-echo-area-message t) ; 关闭启动时回显区的提示信息
(setq read-process-output-max #x10000)  ; 64kb.  Increase how much is read from processes in a single chunk (default is 4kb)
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq sentence-end-double-space nil)
(setq sentence-end
      "\\(?:[.?!。！？…]+\\)[]\"')}”’）】』」》]*\\(?:$\\|[ \t]\\)[ \t\n]*") ; 测定句子结束识别同埋标点，不用在 `fill` 时，再句号后插入 2 个空行。

(add-hook 'after-change-major-mode-hook (lambda ()(modify-syntax-entry ?_ "w"))) ;; 让 `_` 被视为单词嘅组成部分
(add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?- "w"))) ;; `-` 符号同样
(setq suggest-key-bindings 1)             ;当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
(setq browse-kill-ring-quit-action        ;设置退出动作
      (quote save-and-restore))           ;保存还原窗口设置
(setq max-lisp-eval-depth 4096)           ;lisp最大执行深度
(setq kill-ring-max 1024)                 ;用一个很大的 kill ring. 这样防止我不小心删掉重要的东西
(setq mark-ring-max 1024)                 ;设置的mark ring容量
(setq eval-expression-print-length nil)   ;设置执行表达式的长度没有限制
(setq eval-expression-print-level nil)    ;设置执行表达式的深度没有限制
(setq read-quoted-char-radix 16)          ;设置 引用字符 的基数
(setq global-mark-ring-max 1024)          ;设置最大的全局标记容量
(setq isearch-allow-scroll t)             ;isearch搜索时是可以滚动屏幕的
(setq enable-recursive-minibuffers t)     ;minibuffer 递归调用命令
(setq history-delete-duplicates t)        ;删除minibuffer的重复历史
(setq minibuffer-message-timeout 2)       ;显示消息超时的时间
(setq auto-revert-mode 1)                 ;自动更新buffer
(setq show-paren-style 'parentheses)      ;括号匹配显示但不是烦人的跳到另一个括号。
(setq message-log-max t)                  ;设置message记录全部消息, 而不用截去
(setq require-final-newline nil)          ;不自动添加换行符到末尾, 有些情况会出现错误
(setq ediff-window-setup-function
      (quote ediff-setup-windows-plain))  ;比较窗口设置在同一个frame里
(setq x-stretch-cursor t)                 ;光标在 TAB 字符上会显示为一个大方块
(setq print-escape-newlines t)            ;显示字符窗中的换行符为 \n
(setq tramp-default-method "ssh")         ;设置传送文件默认的方法
(setq auto-window-vscroll nil)            ;关闭自动调节行高
(setq mouse-yank-at-point nil)            ;让光标无法离开视线
(setq kill-whole-line t)                  ; C-k deletes the end of line
(setq delete-by-moving-to-trash t)        ; Deleting files go to OS's trash folder
(setq save-interprogram-paste-before-kill t) ; Save clipboard contents into kill-ring before replace them

;; Hanlde minified code
(if emacs/>=27p
    (add-hook 'after-init-hook #'global-so-long-mode))

;; 如果有两个重名buffer, 则再前面加上路径区别
(require 'uniquify)
(with-eval-after-load 'uniquify
  ;; (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  )

(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

;; text-mode 下自动开启合适的换行宽度
(add-hook 'text-mode-hook
	  (lambda ()
	    (setq-local fill-column 100) ; 80/100/120
	    (auto-fill-mode 1)))

;; backup settings
(setq make-backup-files t)
(setq version-control t)     ; 允许多次备份
(setq kept-old-versions 2)   ; 保留最早的2个备份文件
(setq kept-new-version 64)  ; 保留最近的64个备份文件
(setq delete-old-versions t) ; 自动删除旧的备份文件

(setq enable-recursive-minibuffers t ; Allow commands in minibuffers
      history-length 1000
      savehist-additional-variables '(mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history)
      savehist-autosave-interval 300
      savehist-file (expand-file-name "history" suk-emacs-var-dir) ; "~/.emacs.d/var/history"
      )
(savehist-mode 1)

;; 重要提示:写在最后一行的，实际上最优先使用; 最前面一行，反而放到最后才识别。
;; utf-16le-with-signature 相当于 Windows 下的 Unicode 编码，这里也可写成
;; utf-16 (utf-16 ham:  utf-16le, utf-16be, utf-16le-with-signature dang)
;; Unicode
;; (prefer-coding-system 'utf-16le-with-signature)
;; (prefer-coding-system 'utf-16)
;; (prefer-coding-system 'utf-8-dos)
;; Set UTF-8 as the default coding system
;; -----------------------------
;; Emacs UTF-8 编码统一设置
;; -----------------------------

;; 1. 设置语言环境为 UTF-8（会影响默认 buffer/file/keyboard/terminal）
(set-language-environment "UTF-8")

;; 2. 告诉 Emacs 优先使用 UTF-8 作为各种操作的默认编码
(prefer-coding-system 'utf-8)

;; 3. 设置默认编码系统（文件读写、缓冲区等）
(set-default-coding-systems 'utf-8)

;; 4. 设置终端和键盘输入输出为 UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; 5. 设置进程 I/O 使用 UTF-8
(setq default-process-coding-system '(utf-8 . utf-8))

;; （可选）如果你用 Emacs 发送邮件，可以启用下面这行
(setq default-sendmail-coding-system 'utf-8)

(require 'buffer-extension)

;; ### Buffer Edit ### 
;; --- 缓存编辑
(lazy-load-set-keys
 '(
   ("C-x C-x" . exchange-point-and-mark)       ;交换当前点和标记点
   ("M-o"     . backward-delete-char-untabify) ;向前删除字符
   ("C-M-S-h" . mark-paragraph)                ;选中段落
   ("M-SPC"   . just-one-space)                ;只有一个空格在光标处
   ))

;;; ### basic-toolkit ###
(lazy-load-global-keys
 '(
   ;;("" . strip-blank-lines)             ; 删除空行
   ;; ("<f2>" . refresh-file)             ;自动刷新文件
   ("C-S-j"   . join-lines)               ;连接下行
   ("C-c d"   . suk/delte-to-char)
   ("M-q"     . suk/fill-or-unfill-paragraph)
   ("C-x n N" . suk/xah-narrow-to-region)
   )
 "basic-toolkit")

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))
;;(global-set-key (kbd "C-x C-b") 'ibuffer)

(with-eval-after-load 'ibuffer
  ;; Display icons for buffers
  (when (display-graphic-p)
    (use-package nerd-icons-ibuffer
      :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
      :init (setq nerd-icons-ibuffer-icon t)))
  )

;; Persistent the scratch buffer
(run-with-idle-timer
 1 nil
 #'(lambda()
     (use-package persistent-scratch
       :diminish
       :bind (:map persistent-scratch-mode-map
		   ([remap kill-buffer] . (lambda (&rest _)
					    (interactive)
					    (user-error "Scratch buffer cannot be killed")))
		   ([remap revert-buffer] . persistent-scratch-restore)
		   ([remap revert-this-buffer] . persistent-scratch-restore))
       :hook ((after-init . persistent-scratch-autosave-mode)iu
	      (lisp-interaction-mode . persistent-scratch-mode))
       :init
       ;; 创建 var 文件夹
       (make-directory (expand-file-name "var" user-emacs-directory) t)

       (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
	     persistent-scratch-backup-directory (expand-file-name "var/persistent-scratch" user-emacs-directory)
	     persistent-scratch-save-file (expand-file-name "var/.persistent-scratch" user-emacs-directory))
       (persistent-scratch-setup-default)

       )))

(lazy-load-global-keys
 '(
   ("M-<f7>" . suk-read-mode)
   ("<f7>"   . olivetti-mode)
   )
 "suk-read-mode")

;; --- 缓存移动
(lazy-load-set-keys
 '(
   ;;("C-z i" . beginning-of-buffer)      ;缓存开始 M-<
   ;;("C-z k" . end-of-buffer)            ;缓存结尾 M->
   ("C-M-f" . forward-paragraph)        ;下一个段落
   ("C-M-b" . backward-paragraph)       ;上一个段落
   ("C-M-y" . backward-up-list)         ;向左跳出 LIST
   ("C-M-u" . backward-down-list)       ;向左跳进 LIST
   ("C-M-o" . up-list)                  ;向右跳出 LIST
   ("C-M-i" . down-list)                ;向右跳进 LIST
   ("C-M-a" . beginning-of-defun)       ;函数开头
   ("C-M-e" . end-of-defun)             ;函数末尾
   ))

;; Automatically reload files was modified by external program
(run-with-idle-timer
 1 nil
 #'(lambda()
     (use-package autorevert)
     (use-package autorevert
       :ensure nil
       :diminish
       :defer 2
       :hook (after-init . global-auto-revert-mode))))

(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)
;;(setq auto-save-delete-trailing-whitespace t)

;; ### vdiff ###
(lazy-load-global-keys
 '(
   ("M-s-u" . vdiff-buffers))
 "vdiff")

(lazy-load-global-keys
 '(
   ("C-c C-<up>"    . buf-move-up)   
   ("C-c C-<down>"  . buf-move-down)
   ("C-c C-<left>"  . buf-move-left)  
   ("C-c C-<right>" . buf-move-right)   
   )
 "buffer-move")

;;; ### goto-line-preview ###
(lazy-load-global-keys
 '(
   ("g p" . goto-line-preview)
  )
 "goto-line-preview"
 "C-z")

(lazy-load-global-keys
 '(
   ("C-x r ," . remember-init)    ;记忆初始函数
   ("C-x r ." . remember-jump)    ;记忆跳转函数
   ("C-x r <" . point-stack-pop)  ;buffer索引跳转
   ("C-x r >" . point-stack-push) ;buffer索引标记
   )
 "my-bookmark"
 )

;;; --- 滚动其他窗口

;; 下面这两个键模拟Vi的光标不动屏幕动效果, 我很喜欢, 几乎总在使用.
;;(global-set-key [(meta N)] 'window-move-up)        
;;(global-set-key [(meta P)] 'window-move-DOWN)

;; 同上, 但是是另一个buffer窗口上下移动. 常常查看帮助用这个.
;;(global-set-key [(control N)] 'other-window-move-up)
;;(global-set-key [(control P)] 'other-window-move-down) 
(lazy-load-global-keys
 '(
   ("M-N" . other-window-move-up)   ;向下滚动其他窗口  scroll-other-window-up
   ("M-P" . other-window-move-down) ;向上滚动其他窗口  scroll-other-window-down
   ("M-n" . window-move-up)         ;向下滚动当前窗口  scroll-up
   ("M-p" . window-move-down)       ;向上滚动当前窗口  scroll-down
   )
 "win-move")

;;; ### move text ###
(lazy-load-global-keys
 '(
   ("C-S-n" . move-text-down) ;把光标所在的整行文字(或标记)下移一行
   ("C-S-p" . move-text-up)   ;把光标所在的整行文字(或标记)上移一行
   ("C-S-<down>" . move-text-down)  ;把光标所在的整行文字(或标记)下移一行
   ("C-S-<up>"   . move-text-up)    ;把光标所在的整行文字(或标记)上移一行
   )
 "move-text")

;;; ### open new line ###
(lazy-load-global-keys
 '(
   ("C-o" . open-newline-above) ;在上面一行新建一行
   ("C-l" . open-newline-below) ;在下面一行新建一行
   )
 "open-newline")

;; ### duplicate-line ###
(lazy-load-global-keys
 '(
   ("C-S-o" . duplicate-line-or-region-above) ;向上复制当前行或区域
   ("C-S-l" . duplicate-line-or-region-below) ;向下复制当前行或区域
   ;;("C-S-s-o" . duplicate-line-above-comment) ;复制当前行到上一行, 并注释当前行
   ;;("C-S-s-l" . duplicate-line-below-comment) ;复制当前行到下一行, 并注释当前行
   ("M-;" . comment-or-uncomment-region+) ;注释当前行
   )
 "duplicate-line")

;; ### String Inflection ###
;; --- 单词语法风格快速转换

 ;;; ### cases ###
(lazy-load-global-keys
 '(
   ("M-l" . downcase-char)
   ("M-u" . upcase-char)
   )
 "cases")

 (lazy-load-global-keys
 '(
   ("C-c C-u" .  string-inflection-all-cycle)
   )
 "string-inflection")

;;; ### THing-edit ###
(lazy-load-global-keys
 '(
   ;; ("C-c w" . thing-copy-word)
   ;; ("C-c s" . thing-copy-symbol)
   ;; ("C-c m" . thing-copy-email)
   ;; ("C-c f" . thing-copy-filename)
   ;; ("C-c u" . thing-copy-url)
   ;; ("C-c x" . thing-copy-sexp)
   ;; ("C-c g" . thing-copy-page)
   ;; ("C-c t" . thing-copy-sentence)
   ;; ("C-c o" . thing-copy-witespace)
   ;; ("C-c i" . thing-copy-list)
   ;; ("C-c c" . thing-copy-comment)
   ;; ("C-c h" . thing-copy-defun)
   ;; ("C-c p" . thing-copy-parentheses)
   ;; ("C-c l" . thing-copy-line)
   ;; ("C-c a" . thing-copy-to-line-begining)
   ;; ("C-c e" . thing-copy-to-line-end)

   ;; ("C-c W" . thing-cut-word)
   ;; ("C-c S" . thing-cut-symbol)
   ;; ("C-c M" . thing-cut-email)
   ;; ("C-c F" . thing-cut-filename)
   ;; ("C-c G" . thing-cut-page)
   ;; ("C-c T" . thing-cut-sentence)
   ;; ("C-c O" . thing-cut-whitespace)
   ;; ("C-c I" . thing-cut-list)
   ;; ("C-c C" . thing-cut-comment)
   ;; ("C-c H" . thing-cut-defun)
   ;; ("C-c P" . thing-cut-parentheses)
   ;; ("C-c L" . thing-cut-line)
   ;; ("C-c A" . thing-cut-to-line-beginning)
   ;; ("C-c E" . thing-cut-to-line-end)
   ("C-c e" . hydra-thing-edit/body)
   )
 "init-thing-edit"
 )

;; expand-region
(run-with-idle-timer
 2 nil
 #'(lambda()
     ;; (use-package expand-region ; I prefer stable version
     ;;    :load-path "~/.emacs.d/extensions/expand-region"
     ;; )
     (with-eval-after-load 'expand-region
       (defun treesit-mark-bigger-node ()
	 "Use tree-sitter to mark regions."
	 (let* ((root (treesit-buffer-root-node))
		(node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
		(node-start (treesit-node-start node))
		(node-end (treesit-node-end node)))
	   ;; Node fits the region exactly. Try its parent node instead.
	   (when (and (= (region-beginning) node-start) (= (region-end) node-end))
	     (when-let ((node (treesit-node-parent node)))
	       (setq node-start (treesit-node-start node)
		     node-end (treesit-node-end node))))
	   (set-mark node-end)
	   (goto-char node-start)))
       )))

;; Add shortcut.
(lazy-load-global-keys
 '(("M-=" . er/expand-region)
   ("M--" . er/contract-region)
   ("M-S-<Right>" . er/expand-region)
   ("M-S-<Left>" . er/contract-region)
   )
 "expand-region")

;; Treat undo history as a tree, ^x u
(run-with-idle-timer
 2 nil
 #'(lambda()
     (if emacs/>=28p
         (progn
           ;; vundo :load-path "~/.emacs.d/extensions/vundo"
           ;; (requir 'vundo)
           (with-eval-after-load 'vundo
             (setq vundo-glyph-alist vundo-unicode-symbols)))
       (progn
         (setq undo-tree-visualizer-timestamps t
               undo-tree-visualizer-diff t
               undo-tree-enable-undo-in-region nil
               undo-tree-auto-save-history nil)
         ;; HACK: keep the diff window
         (with-no-warnings
           (make-variable-buffer-local 'undo-tree-visualizer-diff)
           (setq-default undo-tree-visualizer-diff t))
         (with-eval-after-load 'undo-tree
           (add-hook 'after-init-hook #'global-undo-tree-mode))
         ))
     ))
;; Add shortcuts.
(if emacs/>=28p
    (lazy-load-global-keys
     '(("C-x u" . vundo)
       ("C-/" . vundo)
       )
     "vundo")
  (lazy-load-global-keys
   '(("C-x u" . undo-trees)
     ("C-/"   . undo-tree-undo)
     ("C-?  " . undo-tree-redo)
     )
   "undo-tree")
  )

;;; ### Advice ###
;;; --- 各种emacs行为建议
;; 在特定地模式下粘贴时自动缩进
(defadvice yank (after indent-region activate)
  "To make yank content indent automatically."
  (if (member major-mode
	      '(emacs-lisp-mode
		java-mode
		web-mode
		c-mode
		c++-mode
		js-mode
		latex-mode
		plain-tex-mode))
      (indent-region (region-beginning) (region-end) nil)))

;;在 Windows 上，如果你用 Emacs GUI 版本，默认有时会找不到 PATH 中的程序，
;;可以在 Emacs 配置里手动添加环境变量，或者使用
;; exec-path-from-shell 等包（但这个包通常在 macOS/Linux下更常用；
;;Windows 下可能需要手工设置 exec-path）。
(use-package exec-path-from-shell)
;; (setq exec-path (append exec-path '("C:/path/to/python" "C:/path/to/node")))
;; (setenv "PATH" (concat (getenv "PATH") ";C:\\path\\to\\python;C:\\path\\to\\node"))




;; (use-package counsel) ; counsel => swiper => ivy
;; (use-package counsel-bbdb)
;; (use-package counsel-gtags)
;; (use-package counsel-css)
(use-package bbdb)
(use-package find-file-in-project)
(use-package pinyinlib)
(use-package ace-pinyin)
(use-package goto-chg)

(use-package ivy
  :ensure t
  :diminish (ivy-mode)
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy)

  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (when (display-graphic-p)    
    (use-package ivy-posframe))
  )

(use-package swiper
  :bind
  (
   ("C-x M-s" . swiper)
   ("C-s"     . swiper-isearch)
   ("C-r"     . swiper-isearch)
   ("C-c C-r" . ivy-resume)
   )
  :config
  (progn
    ;;(ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    ;;(setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))
  )

(use-package counsel
  :after ivy
  :ensure t
  :bind
  (
   ;; ("M-y" . counsel-yank-pop)
   ;; ("C-x C-f" . counsel-find-file)
   ("M-x" . counsel-M-x)
   )
  )

(use-package avy)
(use-package avy-zap)
(lazy-load-global-keys
 '(("C-:"   . avy-goto-char)
   ("C-M-;" . avy-goto-char-2)
   ("M-g l" . avy-goto-line)
   ("M-g w" . avy-goto-word-1)
   ("M-g W" . avy-goto-word-0))
 "avy"
 )

(with-eval-after-load 'avy
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        avy-style 'pre)
  (add-hook 'after-init-hook #'avy-setup-default)
  )

(with-eval-after-load 'avy-zap
  ;; Kill text between the point and the character CHAR
  (lazy-load-global-keys
   '(("M-z" . avy-zap-to-char-dwim)
     ("M-Z" . avy-zap-up-to-char-dwim))
   "avy-zap"
   "C-z")
  )

;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
	wgrep-change-readonly-file t))

;; Search tool
(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :init
  (when (executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")))


;; Fast search tool `ripgrep'
(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
	      ("c" . rg-dwim-current-dir)
	      ("f" . rg-dwim-current-file)
	      ("m" . rg-menu))
  :init (setq rg-group-result t
	      rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases))

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
	 ("C-c M-x" . consult-mode-command)
	 ([remap Info-search]        . consult-info)
	 ([remap isearch-forward]    . consult-line)
	 ([remap recentf-open-files] . consult-recent-file)

	 ;; C-x bindings in `ctl-x-map'
	 ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
	 ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
	 ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
	 ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
	 ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
	 ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
	 ;; Custom M-# bindings for fast register access
	 ("M-#"     . consult-register-load)
	 ("M-'"     . consult-register-store)      ;; orig. abbrev-prefix-mark (unrelated)
	 ("C-M-#"   . consult-register)
	 ;; Other custom bindings
	 ("M-y"     . consult-yank-pop)            ;; orig. yank-pop
	 ;; M-g bindings in `goto-map'
	 ("M-g e"   . consult-compile-error)
	 ("M-g g"   . consult-goto-line)           ;; orig. goto-line
	 ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
	 ("M-g o"   . consult-outline)             ;; Alternative: consult-org-heading
	 ("M-g m"   . consult-mark)
	 ("M-g k"   . consult-global-mark)
	 ("M-g i"   . consult-imenu)
	 ("M-g I"   . consult-imenu-multi)
	 ;; ("M-g M"   . consult-man)
	 ;; ("M-g I"   . consult-info)
	 ("M-g t"   . consult-theme)
	 ("M-g h"   . consult-history)
	 ;; M-s bindings in `search-map'
	 ("M-s d"   . consult-find)
	 ("M-s D"   . consult-locate)
	 ("M-s g"   . consult-grep)
	 ("M-s G"   . consult-git-grep)
	 ("M-s r"   . consult-ripgrep)
	 ("M-s l"   . consult-line)
	 ("M-s L"   . consult-line-multi)
	 ("M-s k"   . consult-keep-lines)
	 ("M-s u"   . consult-focus-lines)
	 ;; Isearch integration
	 ("M-s e"   . consult-isearch-history)
	 ("C-r" .  consult-line)
	 :map isearch-mode-map
	 ("M-e"     . consult-isearch-history)
	 ("M-s e"   . consult-isearch-history)
	 ("M-s l"   . consult-line)
	 ("M-s L"   . consult-line-multi)
	 ) 

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (use-package consult-flyspell
    :bind ("M-g s" . consult-flyspell))

  (use-package consult-yasnippet
    :bind ("M-g y" . consult-yasnippet))
  ;; Use Consult to select xref locations with preview
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
	  xref-show-definitions-function #'consult-xref))
  )

;; Recentf
(setq recentf-save-file (concat suk-emacs-var-dir "/recentf"))
;;(setq recentf-save-file "~/.emacs.d/var/recentf")
(use-package recentf
  :ensure nil
  :defer 1
  :init
  (setq recentf-save-file (concat suk-emacs-var-dir "/recentf"))
  ;;(setq recentf-save-file "~/.emacs.d/var/recentf")
  ;;(add-hook 'after-init-hook #'recentf-mode)
  (setq recentf-max-saved-items 500)
  (setq recentf-max-saved-items 17)
  (recentf-mode)
  (recentf-track-opened-file)

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
  (defun suk/recentf-exclude-p (file)
    (let ((file-dir (file-truename (file-name-directory file))))
      (-any-p (lamdba (dir)
		      (string-prefix-p dir file-dir))
	      (mapcar 'file-truename (list var package-user-dir)))))
  (add-to-list 'recentf-exclude #'suk/recentf-exclude-p)
  )

;; 折叠和收缩代码
;; builtin
(use-package hideshow
  :diminish hs-minor-mode
  :bind
  (:map hs-minor-mode-map
	("C-c ." . hs-toggle-hiding)
	("C-c ," . hs-show-all)
	)
  :hook (prog-mode . hs-minor-mode)
  :config
  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
		   (concat
		    " "
		    (propertize
		     (if (char-displayable-p ?⏷) "⏷" "...")
		     'face 'shadow)
		    (propertize
		     (format " (%d lines)"
			     (count-lines (overlay-start ov)
					  (overlay-end ov)))
		     'face '(:inherit shadow :height 0.8))
		    " "))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts)
  )

;; setup hydra
(run-with-idle-timer
 1 nil
 #'(lambda()
     (use-package hydra
       :hook (emacs-lisp-mode . hydra-add-imenu)
       :config
       (with-eval-after-load 'posframe
         (setq hydra-hint-display-type 'posframe)
         (defun hydra-set-posframe-show-params ()
           "Set hydra-posframe style."
           (setq hydra-posframe-show-params
                 `(
                   :left-fringe 8
                   :right-fringe 8
                   :internal-border-width 2
                   :internal-border-color ,(face-background 'posframe-border nil t)
                   :background-color ,(face-background 'tooltip nil t)
                   :foreground-color ,(face-foreground 'tooltip nil t)
                   :lines-truncate t
                   )))
         (hydra-set-posframe-show-params)
         (add-hook 'after-load-theme-hook #'hydra-set-posframe-show-params t))
       )))

;;;; The emacsclient call depends on the daemon or `server-mode' (I use the latter)
(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

;; Display available keybindings in popup

(use-package which-key
  :diminish
  :bind (("C-h M-m" . which-key-show-major-mode)
         (:map help-map ("C-h" . which-key-C-h-dispatch)))

  :hook (after-init . which-key-mode)
  ;;:custom
  ;; 弹出方式，底部弹出
  ;;(which-key-popup-type 'side-window)
  :init (setq which-key-max-description-length 30
              which-key-lighter nil
              which-key-show-remaining-keys t)
  :config
  (which-key-mode))
(when sys/is-gui
  (use-package which-key-posframe
    :after (which-key posframe)
    :config
    (which-key-posframe-mode 1)))

;;; ### Company en words ###
;;; --- 英文助手
(lazy-load-global-keys
 '(
   ("M-r" . toggle-company-english-helper) ;英文助手
   )
 "company-english-helper")
