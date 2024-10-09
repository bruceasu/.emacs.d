;;; init-basic --- set the basic configurations.
;;; Commentary:
(provide 'init-basic)


;;(tooltip-mode -1)                       ;不要显示任何 tooltips
(delete-selection-mode 1)               ; 选中文本后输入会覆盖
(size-indication-mode 1)
;;(blink-cursor-mode -1)
(setq inhibit-startup-message t)          ; 关闭启动欢迎界面
(setq initial-scratch-message nil)        ; 清空 *scratch* 缓冲区信息
(setq inhibit-startup-echo-area-message t) ; 关闭启动时回显区的提示信息

(setq-default
 major-mode 'text-mode
 cursor-type 'bar ; 设置光标样式
 tab-width 4
 indent-tabs-mode nil)     ;; Permanently indent with spaces, never with TABs
;; only use spaces instead of TAB, use C-q TAB to input the TAB char


(setq read-process-output-max #x10000)  ; 64kb.  Increase how much is read from processes in a single chunk (default is 4kb)
(setq vc-follow-symlinks t)
(setq font-lock-maximum-decoration t)

(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq set-mark-command-repeat-pop t) ; Repeating C-SPC after popping mark pops it again
(setq sentence-end "\\([。！？￥%×（）—]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*") ;; citding sentence-end sikbit tsungman biudim, bat yungzoi `fill` shi, zoi gêihòu hau cápyap 2 go hung gák.

(add-hook 'after-change-major-mode-hook (lambda ()(modify-syntax-entry ?_ "w"))) ;; yöng `_` bèi shiwai dánci ge zòusing bòufan
(add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?- "w"))) ;; `-` fuhòu tungsöng
(setq sentence-end-double-space nil)


;;====================================================
;; Encoding begin
;;====================================================
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


(setq suggest-key-bindings 1)             ;当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
(setq browse-kill-ring-quit-action        ;设置退出动作
      (quote save-and-restore))           ;保存还原窗口设置

(setq max-lisp-eval-depth 40000)          ;lisp最大执行深度
(setq max-specpdl-size 10000)             ;最大容量
(setq kill-ring-max 1024)                 ;用一个很大的 kill ring. 这样防止我不小心删掉重要的东西
(setq mark-ring-max 1024)                 ;设置的mark ring容量
(setq eval-expression-print-length nil)   ;设置执行表达式的长度没有限制
(setq eval-expression-print-level nil)    ;设置执行表达式的深度没有限制
(auto-compression-mode 1)                 ;打开压缩文件时自动解压缩
(setq read-quoted-char-radix 16)          ;设置 引用字符 的基数
(setq global-mark-ring-max 1024)          ;设置最大的全局标记容量
(global-hl-line-mode 1)                   ;高亮当前行
(setq isearch-allow-scroll t)             ;isearch搜索时是可以滚动屏幕的
(setq enable-recursive-minibuffers t)     ;minibuffer 递归调用命令
(setq history-delete-duplicates t)        ;删除minibuffer的重复历史
(setq minibuffer-message-timeout 2)       ;显示消息超时的时间
(setq auto-revert-mode 1)                 ;自动更新buffer
(show-paren-mode t)                       ;显示括号匹配
(setq show-paren-style 'parentheses)      ;括号匹配显示但不是烦人的跳到另一个括号。
(setq blink-matching-paren nil)           ;当插入右括号时不显示匹配的左括号
(setq message-log-max t)                  ;设置message记录全部消息, 而不用截去
(setq require-final-newline nil)          ;不自动添加换行符到末尾, 有些情况会出现错误
(setq ediff-window-setup-function
      (quote ediff-setup-windows-plain))  ;比较窗口设置在同一个frame里
(setq x-stretch-cursor t)                 ;光标在 TAB 字符上会显示为一个大方块
(put 'narrow-to-region 'disabled nil)     ;开启变窄区域
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
;;(setq auto-save-default nil)              ; Disable auto save
(setq echo-keystrokes 0.1)              ;加快快捷键提示的速度
(setq byte-compile-warnings
      (quote (
              ;; 显示的警告
              free-vars                 ;不在当前范围的引用变量
              unresolved                ;不知道的函数
              callargs                  ;函数调用的参数和定义的不匹配
              obsolete                  ;荒废的变量和函数
              noruntime                 ;函数没有定义在运行时期
              interactive-only          ;正常不被调用的命令
              make-local                ;调用 `make-variable-buffer-local' 可能会不正确的
              mapcar                    ;`mapcar' 调用
              ;;
              ;; 抑制的警告
              (not redefine)            ;重新定义的函数 (比如参数数量改变)
              (not cl-functions)        ;`CL' 包中的运行时调用的函数
              )))



;; 如果有两个重名buffer, 则再前面加上路径区别
(require 'uniquify)
;; (setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)


;; =========================================================
;;备份策略
;; =========================================================
(setq make-backup-files t)
(setq version-control t)     ; 允许多次备份
(setq kept-old-versions 2)   ; 保留最早的2个备份文件
(setq kept-new-version 100)  ; 保留最近的100个备份文件
(setq delete-old-versions t) ; 自动删除旧的备份文件

;; Misc
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

;;; ### Advice ###
;;; --- 各种emacs行为建议
;; 在特定地模式下粘贴时自动缩进
(defadvice yank (after indent-region activate)
  "To make yank content indent automatically."
  (if (member major-mode '(emacs-lisp-mode
                           java-mode
                           web-mode
                           c-mode
                           c++-mode
                           js-mode
                           latex-mode
                           plain-tex-mode))
      (indent-region (region-beginning) (region-end) nil)))

;;; Windows
;; spcial coding settings for Windows
(unless (memq system-type '(cygwin windows-nt ms-dos))
  (setq selection-coding-system 'utf-8))
