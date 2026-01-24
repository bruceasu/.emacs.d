(setq user-full-name "Suk")
(setq user-mail-address "bruceasu@gmail.com")

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

(require 'init-key)

;; 定义一个Leader-key mode. 使用 M-m 开启，原来的功能重新绑定到 M-i
;; (global-set-key (kbd "M-i") #'back-to-indentation)
(require 'leader-key)

(use-package bind-key)
;;(bind-key "C-c x" #'some-function some-package-mode-map)
;;(bind-key "C-c y" #'another-function)

;; (bind-keys
;;  ("C-x C-c" . save-buffers-kill-terminal)
;;  ("C-x C-f" . find-file)
;;  ("C-x C-s" . save-buffer))

;; (bind-keys :map python-mode-map
;;            ("C-c C-c" . python-shell-send-buffer)
;;            ("C-c C-r" . python-shell-send-region))


;; Toggle fullscreen <F11> also bind to fullscreen
(bind-keys ("C-<f11>" . toggle-frame-fullscreen)
	    ("C-S-f" . toggle-frame-fullscreen) ; Compatible with macOS
	    ("M-S-<return>" . toggle-frame-fullscreen) ; Compatible with Windos
)

(global-set-key (kbd "C-x R") 'recentf-open)    ;; 快捷键 C-x R 用于打开最近文件

(if (display-graphic-p)
    (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

(when emacs/>=29p
  ;; (keymap-global-set <key> <cmmd>)
  (keymap-set global-map "C-<f11>" #'toggle-frame-fullscreen)       ;; 快捷键 C-<f11> 用于切换全屏模式
  (keymap-set global-map "M-s-<return>" #'toggle-frame-fullscreen)  ;; 快捷键 M-s-<return> 也用于切换全屏模式
  ) 
(unless emacs/>=29p
  (global-set-key (kbd "C-<f11>") 'toggle-frame-fullscreen)      ;; 快捷键 C-<f11> 用于切换全屏模式
  (global-set-key (kbd "M-s-<return>") 'toggle-frame-fullscreen)
  )

(global-set-key  (kbd "C-S-SPC") 'set-mark-command)

;;; ### Toolkit ###
(lazy-load-set-keys
 '(
   ("C-," . bury-buffer)                            ;;隐藏当前buffer
   ("C-." . unbury-buffer)                          ;;反隐藏当前buffer
   ("C-<f12>" . lazycat-theme-toggle)
   )
 (current-global-map)
 "C-z"
 )

;; 一啲方便嘅函数
;; 用于向后跳跃到上一个 sexp C-M-<left> / ESC C-<left>
;; 用于向前跳跃到下一个 sexp C-M-<right> / ESC C-<right>
;; eval-expression 执行表达式 M-: M-ESC :
;; sort-lines, calendar, align-regexp
;; C-t transpose-char M-t tanspose-word
;; toggle-truncate-lines C-x x t
;; revert-buffer-quick C-x x g
;; C-c TAB indent-region
;; C-u C-c TAB => (un)indent-region

;; M-x global-set-key RET 交互式的绑定你的键。
;; C-x Esc Esc 调出上一条“复杂命令”

;;Emacs 自动排版
;;很简单：C-x h C-M-\
;;其中C-x h 是全选
;;C-M-\ 是排版

;; C-x C-q set/unset readonly
;; 大小写转换： M-u, M-l, M-c

;; M-x align-regexp 可以方便的对齐一些文字

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

;; 自定义删除到字符函数
(defun my/delete-to-char (char)
  "删除光标位置到下一个出现的字符 CHAR 之间的所有内容（不包括 CHAR 本身）。"
  (interactive "cDelete to char: ")
  (let ((start (point))
	(end (progn
	       (search-forward (char-to-string char) (line-end-position) t))))
    (if end
	(progn
	  (goto-char end)
	  (delete-region start (- end 1))
	  (message "Deleted from %d to %d" start (- end 1)))
      (progn
	(delete-region start (line-end-position))
	(message "Character '%c' not found. Deleted to end of line." char)))))

;; 绑定快捷键 C-c d 到 my/delete-to-char 函数
(global-set-key (kbd "C-c d") 'my/delete-to-char)

;;;###autoload
(with-eval-after-load 'hydra
  (defhydra my-hydra-describe (:color blue :hint nil)
    "
Describe Something: (q to quit)
_a_ all help for everything screen
_b_ bindings
_c_ char
_C_ coding system
_f_ function
_i_ input method
_k_ key briefly
_K_ key
_l_ language environment
_m_ major mode
_M_ minor mode
_n_ current coding system briefly
_N_ current coding system full
_o_ lighter indicator
_O_ lighter symbol
_p_ package
_P_ text properties
_s_ symbol
_t_ theme
_v_ variable
_w_ where is something defined
"
    ("b" describe-bindings)
    ("C" describe-categories)
    ("c" describe-char)
    ("C" describe-coding-system)
    ("f" describe-function)
    ("i" describe-input-method)
    ("K" describe-key)
    ("k" describe-key-briefly)
    ("l" describe-language-environment)
    ("M" describe-minor-mode)
    ("m" describe-mode)
    ("N" describe-current-coding-system)
    ("n" describe-current-coding-system-briefly)
    ("o" describe-minor-mode-from-indicator)
    ("O" describe-minor-mode-from-symbol)
    ("p" describe-package)
    ("P" describe-text-properties)
    ("q" nil)
    ("a" help)
    ("s" describe-symbol)
    ("t" describe-theme)
    ("v" describe-variable)
    ("w" where-is))
  (keymap-global-set "C-x C-h" #'my-hydra-describe/body))

;;; Rectangle
(lazy-load-global-keys
 '(
   ("r" . hydra-rectangle/body)
   )
 "init-rectangle"
 "C-z"
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

(run-with-idle-timer
 2 nil
 #'(lambda()
     (use-package paredit) ;; useful for lisp
     (use-package wgrep) ;; eidt the grep / rg result then apply to the origin buffer. Cancel is supportted.
     (use-package writeroom-mode)
     ))

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

;; Toggle two most recent buffers
(fset 'quick-switch-buffer [?\C-x ?b return])
(global-set-key (kbd "C-z b") 'quick-switch-buffer)

;; Restore old window configurations
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo) ;; C-c <Left>/C-c <Right>
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
				      "*Compile-Log*"
				      "*inferior-lisp*"
				      "*Fuzzy Completions*"
				      "*Apropos*"
				      "*Help*"
				      "*cvs*"
				      "*Buffer List*"
				      "*Ibuffer*"
				      "*esh command on file*"))
  )

;; Quickly switch windows
(use-package ace-window
  :bind (([remap other-window] . ace-window)
	 ("C-c w" . ace-window-hydra/body))
  :hook (emacs-startup . ace-window-display-mode)
  :config
  (defun toggle-window-split ()
    (interactive)
    (if (= (count-windows) 2)
	(let* ((this-win-buffer (window-buffer))
	       (next-win-buffer (window-buffer (next-window)))
	       (this-win-edges (window-edges (selected-window)))
	       (next-win-edges (window-edges (next-window)))
	       (this-win-2nd (not (and (<= (car this-win-edges)
					   (car next-win-edges))
				       (<= (cadr this-win-edges)
					   (cadr next-win-edges)))))
	       (splitter
		(if (= (car this-win-edges)
		       (car (window-edges (next-window))))
		    'split-window-horizontally
		  'split-window-vertically)))
	  (delete-other-windows)
	  (let ((first-win (selected-window)))
	    (funcall splitter)
	    (if this-win-2nd (other-window 1))
	    (set-window-buffer (selected-window) this-win-buffer)
	    (set-window-buffer (next-window) next-win-buffer)
	    (select-window first-win)
	    (if this-win-2nd (other-window 1))))
      (user-error "`toggle-window-split' only supports two windows")))

  ;; Bind hydra to dispatch list
  (add-to-list 'aw-dispatch-alist '(?w ace-window-hydra/body) t)

  ;; Select widnow via `M-1'...`M-9'
  (defun aw--select-window (number)
    "Slecet the specified window."
    (when (numberp number)
      (let ((found nil))
	(dolist (win (aw-window-list))
	  (when (and (window-live-p win)
		     (eq number
			 (string-to-number
			  (window-parameter win 'ace-window-path))))
	    (setq found t)
	    (aw-switch-to-window win)))
	(unless found
	  (message "No specified window: %d" number)))))
  (dotimes (n 9)
    (bind-key (format "M-%d" (1+ n))
	      (lambda ()
		(interactive)
		(aw--select-window (1+ n))))))

(suk/wait-for-modules
 (lambda()
   (with-eval-after-load 'pretty-hydra
     (pretty-hydra-define+ ace-window-hydra
       (:title (pretty-hydra-title "Window Management" 'faicon "nf-fa-th")
	       :foreign-keys warn
	       :quit-key ("q" "C-g"))
       ("Actions"
	(;;("TAB" other-window "switch")
	 ("x" ace-delete-window "delete")
	 ("X" ace-delete-other-windows "delete other" :exit t)
	 ("s" ace-swap-window "swap")
	 ("a" ace-select-window "select" :exit t)
	 ("m" toggle-frame-maximized "maximize" :exit t)
	 ("u" toggle-frame-fullscreen "fullscreen" :exit t))
	"Resize"
	(("<left>" shrink-window-horizontally "shrink H")
	 ("<right>" enlarge-window-horizontally "enlarge H")
	 ("<up>" shrink-window "shrink V")
	 ("<down>" enlarge-window "enlarge V")
	 ("n" balance-windows "balance"))
	"Split"
	(("r" split-window-right "horizontally")
	 ("R" split-window-horizontally-instead "horizontally instead")
	 ("v" split-window-below "vertically")
	 ("V" split-window-vertically-instead "vertically instead")
	 ("t" toggle-window-split "toggle")
	 ("o" delete-other-windows "only this"))
	"Zoom"
	(("+" text-scale-increase "in")
	 ("=" text-scale-increase "in")
	 ("-" text-scale-decrease "out")
	 ("0" (text-scale-increase 0) "reset"))
	"Misc"
	(("o" set-frame-font "frame font")
	 ("f" make-frame-command "new frame")
	 ("d" delete-frame "delete frame")
	 ("z" winner-undo "winner undo")
	 ("Z" winner-redo "winner redo"))
	)
       )
     (bind-key "w" #'ace-window-hydra/body leader-key)
     ))
 'ace-window
 'winner
 )

;; Enforce rules for popups
(use-package popper)
(use-package popper
  :custom
  (popper-group-function #'popper-group-by-directory)
  (popper-echo-dispatch-actions t)
  :bind (:map popper-mode-map
              ("C-h z"       . popper-toggle)
              ("C-<tab>"     . popper-cycle)
              ("C-M-<tab>"   . popper-toggle-type))
  :hook (emacs-startup . popper-echo-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*$"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "^\\*eldoc.*\\*$"
          "\\*Compile-Log\\*$"
          "\\*Completions\\*$"
          "\\*Warnings\\*$"
          "\\*Async Shell Command\\*$"
          "\\*Apropos\\*$"
          "\\*Backtrace\\*$"
          "\\*Calendar\\*$"
          "\\*Fd\\*$" "\\*Find\\*$" "\\*Finder\\*$"
          "\\*Kill Ring\\*$"
          "\\*Embark \\(Collect\\|Live\\):.*\\*$"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          flymake-diagnostics-buffer-mode
          flycheck-error-list-mode flycheck-verify-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*Process List\\*$" process-menu-mode
          list-environment-mode cargo-process-mode

          "^\\*.*eshell.*\\*.*$"
          "^\\*.*shell.*\\*.*$"
          "^\\*.*terminal.*\\*.*$"
          "^\\*.*vterm[inal]*.*\\*.*$"

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\**"
          "\\*diff-hl\\**"
          "^\\*macro expansion\\**"

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-.+\\*"
          "\\*prolog\\*" inferior-python-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-run-mode rustic-cargo-test-mode
          ))

  (with-eval-after-load 'doom-modeline
    (setq popper-mode-line
          '(:eval (let ((face (if (doom-modeline--active)
                                  'doom-modeline-emphasis
                                'doom-modeline)))
                    (if (and (icons-displayable-p)
                             (bound-and-true-p doom-modeline-icon)
                             (bound-and-true-p doom-modeline-mode))
                        (format " %s "
                                (nerd-icons-octicon "nf-oct-pin" :face face))
                      (propertize " POP " 'face face))))))
  :config
  (with-no-warnings
    (defun my-popper-fit-window-height (win)
      "Determine the height of popup window WIN by fitting it to the buffer's content."
      (fit-window-to-buffer
       win
       (floor (frame-height) 3)
       (floor (frame-height) 3)))
    (setq popper-window-height #'my-popper-fit-window-height)

    (defun popper-close-window-hack (&rest _)
      "Close popper window via `C-g'."
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p))
                 popper-open-popup-alist)
        (let ((window (caar popper-open-popup-alist)))
          (when (window-live-p window)
            (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack)))

(require 'my-bookmark)

;; C-x r l to list bookmarks

(require 'init-search)

(require 'init-org)

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

(use-package session)
(require 'auto-save)
;; todo dont load
(require 'basic-toolkit)

(setq desktop-load-locked-desktop t) ; don't popup dialog ask user, load anyway
(setq desktop-restore-frames nil)    ; don't restore any frame

(defun emacs-session-restore ()
  "Restore emacs session."
  (interactive)
  (ignore-errors
    ;; Kill other windows.
    (delete-other-windows)
    ;; Kill unused buffers.
    (kill-unused-buffers)
    ;; Restore session.
    (desktop-read "~/.emacs.d/var/")
    ))

(defun emacs-session-save (&optional arg)
  "Save emacs session."
  (interactive "p")
  (ignore-errors
    (if (equal arg 4)
	;; Kill all buffers if with prefix argument.
	(mapc 'kill-buffer (buffer-list))
      ;; Kill unused buffers.
      (kill-unused-buffers)
      ;; Save all buffers before exit.
      (auto-save-buffers))
    ;; Save session.
    (make-directory "~/.emacs.d/var/" t)
    (desktop-save "~/.emacs.d/var/")
    ;; Exit emacs.
    (kill-emacs)))

(global-set-key (kbd "S-<f9>") 'emacs-session-save)
(global-set-key (kbd "C-<f9>") 'emacs-session-save)

(emacs-session-restore)

(require 'init-completion)

(require 'init-tools)

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

;; ===== 按环境加载 =====
(if (display-graphic-p)
    (load "~/.emacs.d/gui.el")
  (load "~/.emacs.d/tui.el"))

;; Hanlde minified code
(if emacs/>=27p (add-hook 'after-init-hook #'global-so-long-mode))
(when sys/linuxp
  (load-if-exists (expand-file-name "linux.el" suk-emacs-root-dir)))
(when sys/win32p
  (load-if-exists (expand-file-name "windows.el" suk-emacs-root-dir)))
(when sys/macp
  (load-if-exists (expand-file-name "mac.el" suk-emacs-root-dir)))
