(run-with-idle-timer
 10
 nil
 #'(lambda()
     ;; Display ugly ^L page breaks as tidy horizontal lines
     (use-package page-break-lines
       :diminish
       :hook (after-init . global-page-break-lines-mode))
     ))

(run-with-idle-timer
 1
 nil
 #'(lambda()
     ;; 切换buffer焦点时高亮动画
     (require-package 'beacon)
     (use-package beacon
       :ensure t
       :hook (after-init . beacon-mode))))

;; hungry-delete
(run-with-idle-timer
 2 nil
 #'(lambda()
     (require-package 'hungry-delete)
     (require 'hungry-delete)
     (with-eval-after-load 'hungry-delete
       (setq hungry-delete-chars-to-skip " \t\f\v"
             hungry-delete-except-modes
             '(help-mode
               minibuffer-mode
               minibuffer-inactive-mode
               calc-mode))
       ;; Delete
       (global-set-key (kbd "C-c <backspace>") #'hungry-delete-backward)
       (global-set-key (kbd "C-c <delete>") #'hungry-delete-forward)
       )))

;;; ### Insert translated name ###
;; youdao / google
(setq insert-translated-name-translate-engine "google")
(lazy-load-global-keys
 '(
   ("," . insert-translated-name-insert-with-underline)
   ("." . insert-translated-name-insert-with-camel)
   ("/" . insert-translated-name-insert)
   )
 "insert-translated-name"
 "C-z"
 )

;; (use-package crux)
(lazy-load-global-keys
 '(
   ;;文件操作:
   ;;("C-c r" . crux-rename-file) ; 重命名当前文件或目录。
   ("C-c k" . crux-rename-file-and-buffer)
   ;;("C-c r" . crux-recentf-find-file)
   ("C-c D"  . crux-delete-file-and-buffer) ;  删除当前文件并关闭相关缓冲区。
   ;; 行/区域操作:
   ;;crux-move-beginning-of-line: 将光标移动到行的开头。
   ;;crux-move-end-of-line: 将光标移动到行的末尾。
   ;;crux-top-join-line: 将当前行与上一行合并。

   ("C-S-k" . crux-kill-whole-line) ;; 剪切整行。
   ;;("C-J" .crux-kill-and-join-forward) ;;除当前行尾的空白字符，并与下一行合并。
   ;;复制/剪切/粘贴操作:
   ;;("C-l" . crux-smart-copy-line-above); 在当前行上方复制当前行。
   ;;("C-o" . crux-smart-copy-line-below);  在当前行下方复制当前行。
   ;;   缩进操作:

   ("C-c TAB" . crux-indent-defun) ;; 对当前函数或代码块重新缩进。
   ;; crux-cleanup-buffer-or-region ;; 清理缓冲区中选定区域或整个缓冲区中的尾随空格和空行。
   ;; 查找/替换操作:
   ;; crux-find-user-init-file ;; 快速打开 Emacs 用户配置文件。
   ;; crux-view-url ;; 在浏览器中查看当前 URL。
   ;; 其他实用功能:

   ("C-c ;" . crux-kill-other-buffers) ;;关闭所有除当前缓冲区外的其他缓冲区。
   ("C-M-k" . crux-kill-line-backwards) ;;向后删除整行内容（包括行尾换行符）。
   ;; crux-reopen-as-root-mode: 以 root 身份重新打开当前文件。

   )
 "crux"
 )

;; kèitá bongding
;; f3 start macro(kmacro-start-macro-or-insert-counter),
;; f4 done macro or run marcro (kmacro-end-or-call-macro).
;; C-x ( start macro (kmacro-start-macro),
;; C-x ) end done marco,
;; C-x e run marco(kmacro-end-macro)
;; 先定义一个宏
;; 然后 name-last-kbd-macro
;; 然后 insert-kbd-macro
;; 等到如下类似的配置
;; (fset 'delete-empty-lines (kbd "M-x flush-lines RET ^\s-*$ RET"))
;;
;;; ### Keyboard Macro ###
;;; --- 键盘宏
(lazy-load-global-keys
 '(
   ("M-s-s" . kmacro-start-macro-or-insert-counter) ;开始键盘宏或插入 F3
   ("M-s-d" . kmacro-end-or-call-macro)    ;结束键盘宏或调用 F4
   ("M-s-c" . kmacro-delete-ring-head)     ;删除当前的键盘宏
   ("M-s-w" . kmacro-cycle-ring-next)      ;下一个键盘宏
   ("M-s-e" . kmacro-cycle-ring-previous)  ;上一个键盘宏
   ("M-s-a" . kmacro-edit-macro)           ;编辑键盘宏
   ("M-s-v" . name-last-kbd-macro)         ;命令当前键盘宏
   ("M-s-f" . insert-kbd-macro)            ;插入键盘宏
   ("M-s-q" . apply-macro-to-region-lines) ;应用键盘宏到选择的区域
   )
 "macros+")

(require-package 'command-log-mode) ;; show the command you press the shortcuts. M-x command-log-mode, M-x clm/open-command-log-buffer

;;(require-package 'lsp-mode)
;;(require-package 'lsp-ui)
;;(require-package 'dap-mode)

;; Tree-sitter support
(when sys/linuxp
 (when (suk-treesit-available-p)
   (require-package 'treesit-auto)
   (use-package treesit-auto
     :ensure t
     :hook (after-init . global-treesit-auto-mode)
     :init (setq treesit-auto-install 'prompt))
   (global-treesit-auto-mode)
   )
(require 'fingertip) ;; M-x treesit-install-language-grammar RET
(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'c++-mode-hook
               'java-mode-hook
               'haskell-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'maxima-mode-hook
               'ielm-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'php-mode-hook
               'python-mode-hook
               'js-mode-hook
               'go-mode-hook
               'qml-mode-hook
               'jade-mode-hook
               'css-mode-hook
               'ruby-mode-hook
               'coffee-mode-hook
               'rust-mode-hook
               'rust-ts-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'web-mode-hook
               'markdown-mode-hook
               'llvm-mode-hook
               'conf-toml-mode-hook
               'nim-mode-hook
               'typescript-mode-hook
               'c-ts-mode-hook
               'c++-ts-mode-hook
               'cmake-ts-mode-hook
               'toml-ts-mode-hook
               'css-ts-mode-hook
               'js-ts-mode-hook
               'json-ts-mode-hook
               'python-ts-mode-hook
               'bash-ts-mode-hook
               'typescript-ts-mode-hook
               ))
  (add-hook hook #'(lambda () (fingertip-mode 1))))

(define-key fingertip-mode-map (kbd "(") 'fingertip-open-round)
(define-key fingertip-mode-map (kbd "[") 'fingertip-open-bracket)
(define-key fingertip-mode-map (kbd "{") 'fingertip-open-curly)
(define-key fingertip-mode-map (kbd ")") 'fingertip-close-round)
(define-key fingertip-mode-map (kbd "]") 'fingertip-close-bracket)
(define-key fingertip-mode-map (kbd "}") 'fingertip-close-curly)
(define-key fingertip-mode-map (kbd "=") 'fingertip-equal)
(define-key fingertip-mode-map (kbd "（") 'fingertip-open-chinese-round)
(define-key fingertip-mode-map (kbd "「") 'fingertip-open-chinese-bracket)
(define-key fingertip-mode-map (kbd "【") 'fingertip-open-chinese-curly)
(define-key fingertip-mode-map (kbd "）") 'fingertip-close-chinese-round)
(define-key fingertip-mode-map (kbd "」") 'fingertip-close-chinese-bracket)
(define-key fingertip-mode-map (kbd "】") 'fingertip-close-chinese-curly)

(define-key fingertip-mode-map (kbd "%") 'fingertip-match-paren)
(define-key fingertip-mode-map (kbd "\"") 'fingertip-double-quote)
(define-key fingertip-mode-map (kbd "'") 'fingertip-single-quote)

;;(define-key fingertip-mode-map (kbd "SPC") 'fingertip-space)
;;(define-key fingertip-mode-map (kbd "RET") 'fingertip-newline)

(define-key fingertip-mode-map (kbd "M-o") 'fingertip-backward-delete)
(define-key fingertip-mode-map (kbd "C-d") 'fingertip-forward-delete)
(define-key fingertip-mode-map (kbd "C-k") 'fingertip-kill)

(define-key fingertip-mode-map (kbd "M-\"") 'fingertip-wrap-double-quote)
(define-key fingertip-mode-map (kbd "M-'") 'fingertip-wrap-single-quote)
(define-key fingertip-mode-map (kbd "M-[") 'fingertip-wrap-bracket)
(define-key fingertip-mode-map (kbd "M-{") 'fingertip-wrap-curly)
(define-key fingertip-mode-map (kbd "M-(") 'fingertip-wrap-round)
(define-key fingertip-mode-map (kbd "M-)") 'fingertip-unwrap)

(define-key fingertip-mode-map (kbd "M-p") 'fingertip-jump-right)
(define-key fingertip-mode-map (kbd "M-n") 'fingertip-jump-left)
(define-key fingertip-mode-map (kbd "M-:") 'fingertip-jump-out-pair-and-newline)

(define-key fingertip-mode-map (kbd "C-j") 'fingertip-jump-up)


)
