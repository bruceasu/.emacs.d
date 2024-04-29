;; init-basic.el --- Initialize basic configurations.	-*- lexical-binding: t -*-

(provide 'init-basic)

(eval-when-compile
  (require '+const)
  (require '+custom)
  (require '+func)
  (require 'subr-x)
  )

;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen t) ; hèicèi makying ge kaidung gaimin
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; ģánbèi gunggêi lán.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;ģánbèi coidán lán
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;ģánbèi ģandung tiu

;; Goyan sönsik
(setq user-full-name suk-full-name)
(setq user-mail-address suk-mail-address)

(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              c-basic-offset 4
              indent-tabs-mode nil)     ;; Permanently indent with spaces, never with TABs
(setq read-process-output-max #x10000)  ; 64kb.  Increase how much is read from processes in a single chunk (default is 4kb)
(setq vc-follow-symlinks t)
(setq font-lock-maximum-decoration t)
(setq initial-scratch-message nil)
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq set-mark-command-repeat-pop t) ; Repeating C-SPC after popping mark pops it again
(setq sentence-end "\\([。！？￥%×（）—]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*") ;; citding sentence-end sikbit tsungman biudim, bat yungzoi `fill` shi, zoi gêihòu hau cápyap 2 go hung gák.

(add-hook 'after-change-major-mode-hook (lambda ()(modify-syntax-entry ?_ "w"))) ;; yöng `_` bèi shiwai dánci ge zòusing bòufan
(add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?- "w"))) ;; `-` fuhòu tungsöng
(setq sentence-end-double-space nil)

(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)

(when sys/win32p
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

  ;; Optimization
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-use-native-image-API t         ; use native w32 API
        w32-pipe-read-delay 0              ; faster IPC
        w32-pipe-buffer-size 65536)        ; read more at a time (64K, was 4K)
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

;; GUI Environment
(when (display-graphic-p)
  (progn
    ;; scroll-bar
    (set-scroll-bar-mode 'right)
    ;; 隐藏垂直滚动条。
    (modify-all-frames-parameters '((vertical-scroll-bars)))
    )
  )

;;====================================================
;; Encoding begin
;;====================================================
;; Set UTF-8 as the default coding system
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

(when sys/win32p
  (setq w32-unicode-filenames t)        ; kaiyung Unicode mangin ming jichi
  (setq file-name-coding-system 'utf-8) ; citji mangin ming pinmá wai UTF-8
  (setq locale-coding-system 'utf-8)    ; citji kêiwik pinmá wai UTF-8

  ;; gámtai
  ;;(prefer-coding-system 'gb2312)
  ;;(prefer-coding-system 'cp936)
  ;;(prefer-coding-system 'gb18030)
  ;;(setq file-name-coding-system 'gb18030)
  ;;(setq locale-coding-system 'gb18030)

  ;; jìngtai
  ;; (prefer-coding-system 'cp950)
  ;; (prefer-coding-system 'big5-hkscs)
  ;; (setq file-name-coding-system 'big5-hkscs) ; Hong Kong and Taiwan
  ;; (setq locale-coding-system 'big5-hkscs)

  ;; yatman
  ;; (setq file-name-coding-system 'cp932)
  ;; (setq locale-coding-system 'cp932)

)

(autoload 'calendar "init-calendar" "Config Chinese calendar " t)

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
  (global-set-key (kbd "C-c C-q") 'my-hydra-describe/body))
