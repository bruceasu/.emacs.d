(require 'basic-function)

(setq user-full-name "Suk")
(setq user-mail-address "bruceasu@gmail.com")

;; basic settings
(setq-default
 major-mode 'text-mode ; 默认使用text模式
 cursor-type 'bar      ; 设置光标样式
 tab-width 4           ; tab 的宽度为 4 空格
 indent-tabs-mode nil  ; 永久使用空格縮排，唔好用 TAB 只係用空格代替
                       ; TAB，使用 C-q TAB 來輸入 TAB 字符
 )
(tooltip-mode -1)                          ;不要显示任何 tooltips
(delete-selection-mode 1)                  ;选中文本后输入会覆盖
(size-indication-mode 1)
(server-mode 1)
(global-hl-line-mode 1)                    ;高亮当前行
(put 'narrow-to-region 'disabled nil)      ;开启变窄区域
(auto-compression-mode 1)                  ;打开压缩文件时自动解压缩
(show-paren-mode t)                        ;显示括号匹配
;;(blink-cursor-mode -1)
(setq inhibit-startup-message t)           ; 关闭启动欢迎界面
;; (setq initial-scratch-message nil)      ; 清空 *scratch* 缓冲区信息
(setq inhibit-startup-echo-area-message t) ; 关闭启动时回显区的提示信息


(setq read-process-output-max #x10000)  ; 64kb.  Increase how much is read from processes in a single chunk (default is 4kb)
(setq vc-follow-symlinks t)
(setq font-lock-maximum-decoration t)

(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq set-mark-command-repeat-pop t) ; Repeating C-SPC after popping mark pops it again
(setq sentence-end "\\([。！？￥%×（）—]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*") ; 测定句子结束识别同埋标点，不用在 `fill` 时，再句号后插入 2 个空行。
(setq sentence-end-double-space nil)
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
;;(setq blink-matching-paren nil)         ;当插入右括号时不显示匹配的左括号
(setq message-log-max t)                  ;设置message记录全部消息, 而不用截去
(setq require-final-newline nil)          ;不自动添加换行符到末尾, 有些情况会出现错误
(setq ediff-window-setup-function
      (quote ediff-setup-windows-plain))  ;比较窗口设置在同一个frame里
(setq x-stretch-cursor t)                 ;光标在 TAB 字符上会显示为一个大方块
(setq print-escape-newlines t)            ;显示字符窗中的换行符为 \n
(setq tramp-default-method "ssh")         ;设置传送文件默认的方法
(setq void-text-area-pointer nil)         ;禁止显示鼠标指针
(setq auto-window-vscroll nil)            ;关闭自动调节行高
(setq mouse-yank-at-point nil)            ;让光标无法离开视线
(setq kill-whole-line t)                  ; C-k deletes the end of line
(setq delete-by-moving-to-trash t)        ; Deleting files go to OS's trash folder
(setq track-eol t)                        ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)
(setq save-interprogram-paste-before-kill t) ; Save clipboard contents into kill-ring before replace them
;;(setq auto-save-default nil)            ; Disable auto save
(setq echo-keystrokes 0.1)                ;加快快捷键提示的速度

;; Hanlde minified code
(if emacs/>=27p
    (add-hook 'after-init-hook #'global-so-long-mode))

;; 如果有两个重名buffer, 则再前面加上路径区别
(require 'uniquify)
(with-eval-after-load 'uniquify
  ;; (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  )
;; Misc
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

;; backup settings
(setq make-backup-files t)
(setq version-control t)     ; 允许多次备份
(setq kept-old-versions 2)   ; 保留最早的2个备份文件
(setq kept-new-version 100)  ; 保留最近的100个备份文件
(setq delete-old-versions t) ; 自动删除旧的备份文件

(setq enable-recursive-minibuffers t ; Allow commands in minibuffers
      history-length 1000
      savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history)
      savehist-autosave-interval 300
      savehist-file (expand-file-name "history" suk-emacs-var-dir) ; "~/.emacs.d/var/history"
      )
(savehist-mode 1)

;; Set UTF-8 as the default coding system
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)            ;缓存文件编码
(setq default-file-name-coding-system 'utf-8)              ;文件名编码
(setq default-keyboard-coding-system 'utf-8)               ;键盘输入编码
(setq default-process-coding-system '(utf-8 . utf-8))      ;进程输出输入编码
(setq default-sendmail-coding-system 'utf-8)               ;发送邮件编码
(setq default-terminal-coding-system 'utf-8)               ;终端编码


(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq buffer-file-coding-system 'utf-8)
(setq session-save-file-coding-system 'utf-8)

(set-language-environment "UTF-8")

;; 重要提示:写在最后一行的，实际上最优先使用; 最前面一行，反而放到最后才识别。
;; utf-16le-with-signature 相当于 Windows 下的 Unicode 编码，这里也可写成
;; utf-16 (utf-16 ham:  utf-16le, utf-16be, utf-16le-with-signature dang)
;; Unicode
;; (prefer-coding-system 'utf-16le-with-signature)
;; (prefer-coding-system 'utf-16)
;; (prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8)

(require 'init-key)

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

;; 一啲方便嘅函数
(global-set-key (kbd "C-x M-a") 'align-regexp)  ;; 快捷键 C-x M-a 用于对齐正则表达式
(global-set-key (kbd "C-(") 'backward-sexp)     ;; 快捷键 C-( 用于向后跳跃到上一个 sexp
(global-set-key (kbd "C-)") 'forward-sexp)         ;; 快捷键 C-) 用于向前跳跃到下一个 sexp
(global-set-key (kbd "C-x R") 'recentf-open)     ;; 快捷键 C-x R 用于打开最近文件
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(when emacs/>=29p
  ;; (keymap-global-set <key> <cmmd>)
  (keymap-set global-map "C-<f11>" #'toggle-frame-fullscreen)            ;; 快捷键 C-<f11> 用于切换全屏模式
  (keymap-set global-map "M-s-<return>" #'toggle-frame-fullscreen)  ;; 快捷键 M-S-<return> 也用于切换全屏模式
  (keymap-set global-map "RET" #'newline-and-indent)                             ;; 回车键 RET 用于创建新行并对齐
  (keymap-set global-map "S-<return>" #'comment-indent-new-line)  ;; Shift + 回车键用于取消对齐创建的新行
  ) 
(unless emacs/>=29p
  (global-set-key (kbd "C-<f11>") 'toggle-frame-fullscreen)  ;; 快捷键 C-<f11> 用于切换全屏模式
  (global-set-key (kbd "M-s<return>") 'toggle-frame-fullscreen)
  (global-set-key (kbd "RET") #'newline-and-indent)  ;; 回车键 RET 用于创建新行并对齐
  (global-set-key (kbd "S-<return>") #'comment-indent-new-line)  ;; Shift + 回车键用于取消对齐创建的新行
  )

(with-eval-after-load 'pretty-hydra
  ;; Global toggles
  (with-no-warnings
    (pretty-hydra-define+ toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "nf-fa-toggle_on") :color amaranth :quit-key ("q" "C-g"))
      ("Basic"
       (("n" (cond ((fboundp 'display-line-numbers-mode)
                    (display-line-numbers-mode (if display-line-numbers-mode -1 1)))
                   ((fboundp 'gblobal-linum-mode)
                    (global-linum-mode (if global-linum-mode -1 1))))
         "line number"
         :toggle (or (bound-and-true-p display-line-numbers-mode)
                     (bound-and-true-p global-linum-mode)))
        ("i" global-aggressive-indent-mode "aggressive indent" :toggle t)
        ("d" global-hungry-delete-mode "hungry delete" :toggle t)
        ("e" electric-pair-mode "electric pair" :toggle t)
        ("c" flyspell-mode "spell check" :toggle t)
        ("s" prettify-symbols-mode "pretty symbol" :toggle t)
        ("l" global-page-break-lines-mode "page break lines" :toggle t)
        ("B" display-battery-mode "battery" :toggle t)
        ("T" display-time-mode "time" :toggle t)
        ("a" abbrev-mode "abrev" :toggle t)
        ("F" auto-fill-mode "auto fill" :toggle t)
        ("m" doom-modeline-mode "modern mode-line" :toggle t)
        ("t" toggle-truncate-lines "truncate lines" :toggle t)
        ("u" toggle-company-ispell "Company Ispell" :toggle t))
       "Highlight"
       (("h l" global-hl-line-mode "line" :toggle t)
        ("h p" show-paren-mode "paren" :toggle t)
        ("h s" symbol-overlay-mode "symbol" :toggle t)
        ("h r" rainbow-mode "rainbow" :toggle t)
        ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
         "whitespace" :toggle show-trailing-whitespace)
        ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
        ("h i" highlight-indent-guides-mode "indent" :toggle t)
        ("h t" global-hl-todo-mode "todo" :toggle t))
       "Program"
       (("f" flymake-mode "flymake" :toggle t)
        ("O" hs-minor-mode "hideshow" :toggle t)
        ("U" subword-mode "subword" :toggle t)
        ("w" whitespace-mode "whitespace" :toggle t)
        ("W" which-function-mode "which function" :toggle t)
        ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
        ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
        ("v" global-diff-hl-mode "gutter" :toggle t)
        ("V" diff-hql-flydiff-mode "live gutter" :toggle t)
        ("M" diff-hl-margin-mode "margin gutter" :toggle t)
        ("D" diff-hl-dired-mode "dired gutter" :toggle t))
       ))
    (keymap-global-set "C-x M-t"  #'toggles-hydra/body)
))

;;; ### goto-line-preview ###
(lazy-load-global-keys
 '(
   ("M-g p" . goto-line-preview))
 "goto-line-preview")

 ;;; ### basic-toolkit ###
(lazy-load-global-keys
 '(
   ("M-G" . goto-column)                ;到指定列
   ("C->" . remember-init)              ;记忆初始函数
   ("C-<" . remember-jump)              ;记忆跳转函数
   ("M-s-," . point-stack-pop)          ;buffer索引跳转
   ("M-s-." . point-stack-push)         ;buffer索引标记
   ("s-g" . goto-percent)               ;跳转到当前Buffer的文本百分比, 单位为字符
   ("s-J" . scroll-up-one-line)         ;向上滚动一行
   ("s-K" . scroll-down-one-line)       ;向下滚动一行
   ("C-S-i" . scroll-up-one-line)       ;向上滚动一行
   ("C-S-k" . scroll-down-one-line)     ;向下滚动一行

   )
 "basic-toolkit")
;; ;;; ### Ace jump ### 比较旧了。使用avy替代
;; (lazy-load-global-keys
;;  '(
;;    ("C-w" . ace-jump-word-mode)
;;    ("C-c" . ace-jump-char-mode)
;;    ("C-l" . ace-jump-line-mode)
;;    )
;;  "ace-jump-mode"
;;  "C-z"
;;  )

;; Jump to Chinese characters
(run-with-idle-timer
 1
 nil
 #'(lambda()     
     (use-package ace-pinyin
       :diminish
       :hook (after-init . ace-pinyin-global-mode))
     (require 'goto-chg)
     ))

;;;###autoload
(defun insert-hash-template ()
  "插入HASH模板：
#=================================================
# <cursor>
#-------------------------------------------------
光标位于第二行的 # 后面。"
  (interactive)
  (beginning-of-line) 
  (insert "#=================================================\n")
  (insert "# \n")
  (insert "#-------------------------------------------------\n")
  ;; 将光标移动到第二行的 # 后面
  (forward-line -2)          ; 移动到上一行（即第二行）
  (end-of-line)              ; 移动到行尾

  )

;;;###autoload
(defun insert-slash-template ()
  "插入Slash comment模板：
#=================================================
# <cursor>
#-------------------------------------------------
光标位于第二行的 // 后面。"
  (interactive)
  (beginning-of-line) 
  (insert "//=================================================\n")
  (insert "// \n")
  (insert "//-------------------------------------------------\n")
  ;; 将光标移动到第二行的 # 后面
  (forward-line -2)          ; 移动到上一行（即第二行）
  (end-of-line)              ; 移动到行尾
  )


;;;###autoload
(defun insert-star-template ()
  "插入Slash comment模板：
#=================================================
# <cursor>
#-------------------------------------------------
光标位于第二行的 # 后面。"
  (interactive)
  (beginning-of-line) 
  (insert "/*=================================================*/\n")
  (insert "/*  */\n")
  (insert "/*-------------------------------------------------*/\n")
  (forward-line -2)         
  (end-of-line)          
  (backward-char 3)      
  )

;;;###autoload
(defun insert-javadoc-template ()
  "插入Javadoc模板：
#=================================================
# <cursor>
#-------------------------------------------------
光标位于第二行的 * 后面。"
  (interactive)
  (beginning-of-line) 
  (insert "/**\n")
  (insert " * \n")
  (insert " */\n")
  (forward-line -2)       
  (end-of-line)         
  )

;; 绑定快捷键 C-c t 到插入自定义模板的函数
(global-set-key (kbd "C-c t h") 'insert-hash-template)
(global-set-key (kbd "C-c t c") 'insert-slash-template)
(global-set-key (kbd "C-c t s") 'insert-star-template)
(global-set-key (kbd "C-c t j") 'insert-javadoc-template)

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
  (global-set-key (kbd "C-c C-h") 'my-hydra-describe/body))

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
     (use-package expand-region ; I prefer stable version
        :load-path "~/.emacs.d/extensions/expand-region"
     )
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
     (use-package tagedit) ;; useful for html
     (use-package cliphist)
     (use-package iedit)
     (use-package wgrep) ;; eidt the grep / rg result then apply to the origin buffer. Cancel is supportted.
     (use-package writeroom-mode)
     ))

(global-set-key  (kbd "C-S-SPC") 'set-mark-command)

(define-prefix-command 'leader-key)
(global-set-key (kbd "M-s-SPC") 'leader-key)

;;; ### Toolkit ###
;;; --- 工具函数
(lazy-load-set-keys
 '(
   ("C-," . bury-buffer)                ;隐藏当前buffer
   ("C-." . unbury-buffer)              ;反隐藏当前buffer
   ("s-[" . eval-expression)            ;执行表达式
   ("s-1" . sort-lines)                 ;排序
   ("s-2" . hanconvert-region)          ;转换简体或繁体中文
   ("s-3" . uniquify-all-lines-buffer)  ;删除重复的行
   ("s-<f12>" . calendar)
   ("C-<f12>" . lazycat-theme-toggle)
   ;;([c-t] . transpose-chars)
   ([S-f5] . toggle-truncate-lines)
   ("C-x M-a" . align-regexp)
   )
 )

;; C-c TAB indent-region
;; C-u C-c TAB => (un)indent-region

;;(global-set-key (kbd "C-(") 'backward-sexp)
;;(global-set-key (kbd "C-)") 'forward-sexp)
;;(global-set-key (kbd "C-x t f") 'global-flycheck-mode)
;;(global-set-key (kbd "C-x R") 'recentf)
;; M-x global-set-key RET 交互式的绑定你的键。
;; C-x Esc Esc 调出上一条“复杂命令”

;;Emacs 自动排版
;;很简单：C-x h C-M-\
;;其中C-x h 是全选
;;C-M-\ 是排版

;; C-x C-q set/unset readonly
;; 大小写转换： M-u, M-l, M-c

;; M-x align-regexp 可以方便的对齐一些文字

(require-package 'buffer-move)

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
      :init (setq nerd-icons-ibuffer-icon suk-icon)))
  )

;; Persistent the scratch buffer
(run-with-idle-timer
 10 nil
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
     (require-package 'autorevert)
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
(global-set-key (kbd "s-b") 'quick-switch-buffer)
(global-set-key (kbd "C-z b") 'quick-switch-buffer)

;; Directional window-selection routines
(lazy-load-global-keys
 '(
   ("<M-up>"    .  windmove-up)   
   ("<M-down>"  .  windmove-down)
   ("<M-left>"  .  windmove-left)
   ("<M-right>" .  windmove-right)
   )
 "windmove")

;; Frame transparence
(lazy-load-global-keys
 '(
   ("C-M-9" . transwin-inc)
   ("C-M-8" . transwin-dec)
   ("C-M-7" . transwin-toggle)
   )
 "transwin"
)

(with-eval-after-load 'transwin
  (setq transwin-parameter-alpha 'alpha-background))

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
  (with-eval-after-load 'pretty-hydra
    (pretty-hydra-define+ ace-window-hydra
      (:title (pretty-hydra-title "Window Management" 'faicon "nf-fa-th")
              :foreign-keys warn :quit-key ("q" "C-g"))
      ("Actions"
       (("TAB" other-window "switch")
        ("x" ace-delete-window "delete")
        ("X" ace-delete-other-windows "delete other" :exit t)
        ("s" ace-swap-window "swap")
        ("a" ace-select-window "select" :exit t)
        ("m" toggle-frame-maximized "maximize" :exit t)
        ("u" toggle-frame-fullscreen "fullscreen" :exit t))
       "Movement"
       (("i" windmove-up "move ↑")
        ("k" windmove-down "move ↓")
        ("j" windmove-left "move ←")
        ("l" windmove-right "move →")
        ("f" follow-mode "follow"))
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
        ("Z" winner-redo "winner redo")))
      )
    (global-set-key (kbd "C-c w") #'ace-window-hydra/body)
    )
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

;; Enforce rules for popups
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

;;; ### watch other window ###
;;; --- 滚动其他窗口
(lazy-load-global-keys
 '(
   ("M-S-n" . other-window-move-up)   ;向下滚动其他窗口
   ("M-S-p" . other-window-move-down) ;向上滚动其他窗口
   ("M-n"   . window-move-up)         ;向下滚动当前窗口
   ("M-p"   . window-move-down)       ;向上滚动当前窗口
   )
 "win-move")

;; (lazy-load-set-keys
;;  '(
;;    ("C-c :" . split-window-vertically)   ;纵向分割窗口
;;    ("C-c |" . split-window-horizontally) ;横向分割窗口
;;    ("C-x ;" . delete-other-windows)      ;关闭其它窗口
;;    ))

(lazy-load-global-keys
 '(
   ("C-c V" . delete-other-windows-vertically+)   ;关闭上下的其他窗口
   ("C-c H" . delete-other-windows-horizontally+) ;关闭左右的其他窗口
   ("C-'" . delete-current-buffer-and-window)     ;关闭当前buffer, 并关闭窗口
   ("C-\"" . delete-current-buffer-window)        ;删除当前buffer的窗口
   ("M-s-o" . toggle-one-window)                  ;切换一个窗口
   ("C-x O" . toggle-window-split)
   )
 "window-extension")

(lazy-load-global-keys
 '(
   ("C-<f7>"   . suk/bookmark-launcher/body)
   )
 "my-bookmark") ;; 有的函数跟basic-toolkit重复

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

(require 'init-ui)

(require 'init-completion)

(require 'init-tools)

;; 空闲时加载
(run-with-idle-timer
 1
 nil
 #'(lambda()
     (require 'load-abbrev)
     ;; chmod +x
     ;; ref. http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#chmod
     (add-hook 'after-save-hook'executable-make-buffer-file-executable-if-script-p)
     (autoload 'calendar "init-calendar" "Config Chinese calendar " t)
     ))

;; Hanlde minified code
(if emacs/>=27p (add-hook 'after-init-hook #'global-so-long-mode))
(when sys/linuxp
  (load-if-exists (expand-file-name "linux.el" suk-emacs-root-dir)))
(when sys/win32p
  (load-if-exists (expand-file-name "windows.el" suk-emacs-root-dir)))
(when sys/macp
  (load-if-exists (expand-file-name "mac.el" suk-emacs-root-dir)))
