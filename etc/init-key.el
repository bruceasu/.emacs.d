;;; init-key --- initial the shortcut
;; -*- lexical-binding: t -*-

(provide 'init-key)
(eval-when-compile
  (require '+const)
  (require '+custom)
  (require 'lazy-load))

;;; 全局 lazy-load-global-keys
;;; 模式 lazy-load-local-keys
;;; 支持最后一个参数传递前缀按键，
;;;
;;; 如果Emacs默认就加载了某个插件，而不需要在运行时动态加载，也可
;;; 以使用 lazy-load-set-keys
;;;
;;; 很多全局按键默认已经被 Emacs占用了，必须先卸载以后才能重新绑
;;; 定这些全局按键，比如 Ctrl + x, 下面这段代码就是用
;;; lazy-load-unset-keys 卸载默认绑定的全局按键

;; Mac平台下交换 Option 和 Command 键。
;;(when (featurep 'cocoa)
;;  (setq mac-option-modifier 'super)
;;  (setq mac-command-modifier 'meta))
;;

;;; ### Unset key ###
 ;;; --- 卸载按键
 (lazy-load-unset-keys                   ;全局按键的卸载
  ;; '("C-z"  "s-W" "s-z" "M-h" "C-\\" "s-c" "s-x" "s-v"))
  '("C-z" ))

;;; ### sudo ###
(lazy-load-global-keys
 '(("C-z C-s" . suk/sudo/body))
 "init-sudo"
 )
;;; ### Sdcv ###
;;; --- 星际译王命令行
(when  (eq system-type 'gnu/linux)
(lazy-load-global-keys
 '(("p" . sdcv-search-pointer)		   ;光标处的单词, buffer显示
   ("P" . sdcv-search-pointer+)		   ;光标处的单词, tooltip显示
   ("i" . sdcv-search-input)		   ;输入的单词, buffer显示
   (";" . sdcv-search-input+)
   ("y" . my-youdao-dictionary-search-at-point)
   ("Y" . youdao-dictionary-search-at-point)
   ("g" . google-translate-at-point)
   ("G" . google-translate-query-translate)
   ("s" . google-translate-smooth-translate)
   ("f" . fanyi-dwim)
   ("d" . fanyi-dwim2)
   ("h" . fanyi-from-history)
   )
 "init-translate"
 "C-z"))

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
   ([S-f6] . hs-minor-mode)
   ([S-f5] . toggle-truncate-lines)
   ("C-x M-a" . align-regexp)
   ("C-c ." . hs-toggle-hiding)
   ("C-c ," . hs-show-all)
   )
 )
   ;;; ### basic-toolkit ###
(lazy-load-global-keys
 '(
   ("M-s-n" . comment-part-move-down)   ;向下移动注释
   ("M-s-p" . comment-part-move-up)     ;向上移动注释
   ("C-s-n" . comment-dwim-next-line)   ;移动到上一行并注释
   ("C-s-p" . comment-dwim-prev-line)   ;移动到下一行并注释
   ("M-2" . indent-buffer)              ;自动格式化当前Buffer
   ("M-z" . upcase-char)                ;Upcase char handly with capitalize-word
   ;;("C-x u" . mark-line)              ;选中整行
   ("s-k" . kill-and-join-forward)      ;在缩进的行之间删除
   ("M-G" . goto-column)                ;到指定列
   ("C->" . remember-init)              ;记忆初始函数
   ("C-<" . remember-jump)              ;记忆跳转函数
   ("M-s-," . point-stack-pop)          ;buffer索引跳转
   ("M-s-." . point-stack-push)         ;buffer索引标记
   ("s-g" . goto-percent) ;跳转到当前Buffer的文本百分比, 单位为字符
   ("M-I" . backward-indent)            ;向后移动4个字符
                                        ;   ("s-J" . scroll-up-one-line)         ;向上滚动一行
                                        ;   ("s-K" . scroll-down-one-line)       ;向下滚动一行
   ("<f2>" . refresh-file)              ;自动刷新文件
   ("s-f" . find-file-root)             ;用root打开文件
   ("s-r" . find-file-smb)              ;访问sambao
   ("C-J" . join-lines)                ;连接行
   )
 "basic-toolkit")


(lazy-load-global-keys
 '(
   ("<f7>" . olivetti-mode))
 "olivetti")

;;; ### goto-line-preview ###
(lazy-load-global-keys
 '(
   ("M-g p" . goto-line-preview))
 "goto-line-preview")

    ;;; ### Delete block ###
    ;;; --- 快速删除光标左右的内容
(lazy-load-global-keys
 '(
   ("M-," . delete-block-backward)
   ("M-." . delete-block-forward))
 "delete-block")


    ;;; ### watch other window ###
    ;;; --- 滚动其他窗口
(lazy-load-global-keys
 '(
   ("C-P" . other-window-move-up)       ;向下滚动其他窗口
   ("C-N" . other-window-move-down) ;向上滚动其他窗口
   ("M-p" . window-move-up)         ;向下滚动当前窗口
   ("M-n" . window-move-down)           ;向上滚动当前窗口
   )
 "win-move")

;; ### move text ###
(lazy-load-global-keys
 '(
   ("M-N" . move-text-down) ;把光标所在的整行文字(或标记)下移一行
   ("M-P" . move-text-up)   ;把光标所在的整行文字(或标记)上移一行
   ("M-<DOWN>" . move-text-down)	;把光标所在的整行文字(或标记)下移一行
   ("M-<UP>"   . move-text-up)	;把光标所在的整行文字(或标记)上移一行
   )
 "move-text")

;; ### duplicate-line ###

(lazy-load-global-keys
 '(
   ("C-S-o" . duplicate-line-or-region-above) ;向上复制当前行或区域
   ("C-S-l" . duplicate-line-or-region-below) ;向下复制当前行或区域
   ("C-S-s-o" . duplicate-line-above-comment) ;复制当前行到上一行, 并注释当前行
   ("C-S-s-l" . duplicate-line-below-comment) ;复制当前行到下一行, 并注释当前行
   ("C-:" . comment-or-uncomment-region+)     ;注释当前行

   )
 "duplicate-line")

;;; ### open new line ###
(lazy-load-global-keys
  '(
    ("C-o" . open-newline-above)         ;在上面一行新建一行
    ("C-l" . open-newline-below)         ;在下面一行新建一行
    )
  "open-newline")


;; ### Rect ###
;; --- 矩形操作
(lazy-load-global-keys
 '(
   ("s-M" . rm-set-mark)                ;矩形标记
   ("s-X" . rm-exchange-point-and-mark) ;矩形对角交换
   ("s-D" . rm-kill-region)             ;矩形删除
   ("s-S" . rm-kill-ring-save)          ;矩形保存
   ("s-Y" . yank-rectangle)             ;粘帖矩形
   ("s-O" . open-rectangle)       ;用空白填充矩形, 并向右移动文本
   ("s-C" . clear-rectangle)      ;清空矩形
   ("s-T" . string-rectangle)     ;用字符串替代矩形的每一行
   ("s-I" . string-insert-rectangle)    ;插入字符串在矩形的每一行
   ("s-F" . delete-whitespace-rectangle) ;删除矩形中空格
   ("s-\"" . copy-rectangle-to-register) ;拷贝矩形到寄存器
   ("s-:" . mark-rectangle-to-end)       ;标记矩形到行末
   )
 "rect-extension")

;; ### Buffer Move ###
;; --- 缓存移动
(lazy-load-set-keys
 '(
   ;;("C-z i" . beginning-of-buffer)      ;缓存开始 M-<
   ;;("C-z k" . end-of-buffer)            ;缓存结尾 M->
   ("C-M-f" . forward-paragraph)        ;下一个段落
   ("C-M-b" . backward-paragraph)       ;上一个段落
   ("C-M-y" . backward-up-list)         ;向左跳出 LIST
   ("C-M-o" . up-list)                  ;向右跳出 LIST
   ("C-M-u" . backward-down-list)       ;向左跳进 LIST
   ("C-M-i" . down-list)                ;向右跳进 LIST
   ("C-M-a" . beginning-of-defun)       ;函数开头
   ("C-M-e" . end-of-defun)             ;函数末尾
   ))

;; ### Buffer Edit ###
;; --- 缓存编辑
(lazy-load-set-keys
 '(
   ("C-x C-x" . exchange-point-and-mark)   ;交换当前点和标记点
   ("M-o" . backward-delete-char-untabify) ;向前删除字符
   ("C-M-S-h" . mark-paragraph)            ;选中段落
   ("M-SPC" . just-one-space)              ;只有一个空格在光标处
   ))
(lazy-load-global-keys
 '(
    ("C-c b"   . my-hydra-buffers/body)
    ("M-<f7>"  . suk-read-mode)
    ("C-;" . suk/close-current-buffer)            ;关闭当前buffer
   )
 "init-buffers")
;; ### Font ###
;; --- 字体命令
(lazy-load-set-keys
 '(
   ("C--" . text-scale-decrease)        ;减小字体大小
   ("C-=" . text-scale-increase)        ;增加字体大小
   ))


;; ### Window Operation ###
;; --- 窗口操作
(lazy-load-set-keys
 '(
   ;;("C-c :" . split-window-vertically)   ;纵向分割窗口
   ;;("C-c |" . split-window-horizontally) ;横向分割窗口

   ;;("C-x ;" . delete-other-windows)      ;关闭其它窗口
   ))
(lazy-load-global-keys
 '(
   ("C-c V" . delete-other-windows-vertically+) ;关闭上下的其他窗口
   ("C-c H" . delete-other-windows-horizontally+) ;关闭左右的其他窗口
   ("C-'" . delete-current-buffer-and-window) ;关闭当前buffer, 并关闭窗口
   ("C-\"" . delete-current-buffer-window) ;删除当前buffer的窗口
   ("M-s-o" . toggle-one-window)           ;切换一个窗口
   ("C-x O" . toggle-window-split)
   )
 "window-extension")

  ;;; ### Functin key ###
  ;;; --- 功能函数
(lazy-load-set-keys
 '(
   ("<f9>" . emacs-session-save )
   ("C-4" . insert-changelog-date)      ;插入日志时间 (%Y/%m/%d)
   ("C-&" . switch-to-messages)         ;跳转到 *Messages* buffer
   ))

  ;;; ### Awesome-Pair ###
  ;;; --- 结构化编程

  ;;; ### Thingh-edit ###
  ;;; --- 增强式编辑当前光标的对象
(lazy-load-global-keys
 '(
   ("C-c w" . thing-copy-word)
   ("C-c s" . thing-copy-symbol)
   ("C-c m" . thing-copy-email)
   ("C-c f" . thing-copy-filename)
   ("C-c u" . thing-copy-url)
   ("C-c x" . thing-copy-sexp)
   ("C-c g" . thing-copy-page)
   ("C-c t" . thing-copy-sentence)
   ("C-c o" . thing-copy-witespace)
   ("C-c i" . thing-copy-list)
   ("C-c c" . thing-copy-comment)
   ("C-c h" . thing-copy-defun)
   ("C-c p" . thing-copy-parentheses)
   ("C-c l" . thing-copy-line)
   ("C-c a" . thing-copy-to-line-begining)
   ("C-c e" . thing-copy-to-line-end)
   ("C-c W" . thing-cut-word)
   ("C-c S" . thing-cut-symbol)
   ("C-c M" . thing-cut-email)
   ("C-c F" . thing-cut-filename)
   ("C-c G" . thing-cut-page)
   ("C-c T" . thing-cut-sentence)
   ("C-c O" . thing-cut-whitespace)
   ("C-c I" . thing-cut-list)
   ("C-c C" . thing-cut-comment)
   ("C-c H" . thing-cut-defun)
   ("C-c P" . thing-cut-parentheses)
   ("C-c L" . thing-cut-line)
   ("C-c A" . thing-cut-to-line-beginning)
   ("C-c E" . thing-cut-to-line-end)
   )
 "thing-edit"
 "C-z"
 )

  ;;; ### Isearch ###
  ;;; --- 交互式搜索
(lazy-load-set-keys
 '(
   ("TAB" . isearch-complete)               ;isearch补全
   ("C-s" . isearch-repeat-forward)         ;重复向前搜索, 第一次可以用来搜索上一次的历史哟
   ("C-r" . isearch-repeat-backward)        ;重复向后搜索
   ("C-g" . isearch-abort)                  ;中止搜索
   ("C-w" . isearch-yank-word-or-char)      ;粘帖光标后的词或字符作为搜索对象
   ("C-y" . isearch-yank-line)              ;粘帖光标后的行作为搜索对象
   ("M-o" . isearch-delete-char)            ;删除
   ("M-p" . isearch-ring-retreat)           ;搜索历史向后
   ("M-n" . isearch-ring-adjust)            ;搜索历史向前
   ("M-y" . isearch-yank-kill)              ;从 kill ring 中粘帖最后一项到搜索对象后
   ("M-h" . isearch-yank-char)              ;粘帖光标后的字符到搜索对象
   ("M-e" . isearch-edit-string)            ;编辑搜索对象
   ("M-c" . isearch-toggle-case-fold)       ;切换大小写
   ("M-r" . isearch-toggle-regexp)          ;切换正则表达式
   ("M-w" . isearch-toggle-word)            ;切换词
   ("M->" . isearch-beginning-of-buffer)    ;跳转到buffer开头并重新搜索, 搜索最前面一个
   ("M-<" . isearch-end-of-buffer)          ;跳转到buffer末尾并重新搜索, 搜索最后面一个
   ("M-%" . isearch-query-replace)          ;替换
   ("M-d" . isearch-find-duplicate-word)    ;查找重复的单词
   ("M-z" . isearch-find-duplicate-line)    ;查找重复的行
   ("C-M-%" . isearch-query-replace-regexp) ;正则表达式替换
   )
 isearch-mode-map
 )


  ;;; ### expand-region ###
(lazy-load-global-keys
 '(
   ("C-+" . er/expand-region))
 "expand-region")

;; ### vdiff ###
(lazy-load-global-keys
 '(
   ("M-s-u" . vdiff-buffers))
 "vdiff")

  ;;; ### Ace jump ###
(lazy-load-global-keys
 '(
   ("C-c w" . ace-jump-word-mode)
   ("C-c c" . ace-jump-char-mode)
   ("C-c l" . ace-jump-line-mode)
   )
 "ace-jump-mode"
 "C-z"
 )


  ;;; ### Company en words ###
  ;;; --- 英文助手
(lazy-load-global-keys
 '(
   ("M-r" . toggle-company-english-helper) ;英文助手
   )
 "company-english-helper")



(lazy-load-set-keys
 '(
   ("C-z S g" . suk/google-search)
   ("C-z S c" .  suk/github-code-search)
   )
 )

;;; --- 笔记管理和组织
(define-prefix-command 'F9-map)
(global-set-key (kbd "<f9>") 'F9-map)
(lazy-load-global-keys
	'(("a" . org-agenda)
	   	("A" . org-attach)
	   	("s" . show-org-agenda)
	   	("c" . org-capture)
	   	("i" . org-toggle-inline-images)
	   	("l" . org-toggle-link-display)
	   	("d" . calendar)
	   	("F" . boxquote-insert-file)
	   	("R" . boxquote-region)
	   	("v" . visible-mode)
	   	("f" . suk/file-shortcuts/body)
	   	("<f9>" . emacs-session-save )
   		("C-4" . insert-changelog-date)      ;插入日志时间 (%Y/%m/%d)
   		("C-&" . switch-to-messages)         ;跳转到 *Messages* buffer
	)
  "init-org"
  "<f9>")
(lazy-load-global-keys
	'(("C-c C-u" . string-inflection-hydra/body)
	)
"init-string-inflection"
	)

;;; Dash.
;;(lazy-load-global-keys
;; '(("y" . dash-at-point)
;;   )
;; "dash-at-point"
;; "C-x"
;; )

;; ### String Inflection ###
;; --- 单词语法风格快速转换
(lazy-load-global-keys
 '(
   ("C-c C-u" . one-key-string-inflection)
   )
 "init-string-inflection")

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

;;(global-set-key  [C-f7] 'suk/point-to-register)
;;(global-set-key  [f7] 'suk/jump-to-register)
(lazy-load-global-keys
 '(
    ("C-<f7>"   . suk/bookmark-launcher/body)
   )
 "init-bookmark")

(lazy-load-global-keys
'(
  ;;文件操作:
  ;;("C-c r" . crux-rename-file) ; 重命名当前文件或目录。
  ("C-c D"  . crux-delete-file-and-buffer) ;  删除当前文件并关闭相关缓冲区。
  ;; 行/区域操作:
  ;;crux-move-beginning-of-line: 将光标移动到行的开头。
  ;;crux-move-end-of-line: 将光标移动到行的末尾。
  ;;crux-top-join-line: 将当前行与上一行合并。

  ("C-K" . crux-kill-whole-line) ;; 剪切整行。
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
  ("C-k" . crux-kill-line-backwards) ;;向后删除整行内容（包括行尾换行符）。
  ;; crux-reopen-as-root-mode: 以 root 身份重新打开当前文件。

  )
"init-crux"
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




;; global-set-key examples:
;; (global-set-key (kbd "C-x C-\\") 'next-line)
;; (global-set-key [?\C-x ?\C-\\] 'next-line)
;; (global-set-key [(control ?x) (control ?\\)] 'next-line)

;;(global-set-key (kbd "C-(") 'backward-sexp)
;;(global-set-key (kbd "C-)") 'forward-sexp)

;;(global-set-key (kbd "C-x t T") 'suk/toggle-transparency)
;;(global-set-key (kbd "C-x t p") 'suk/toggle-toggle-proxy)
;;(global-set-key (kbd "C-x t f") 'global-flycheck-mode)
;;(global-set-key (kbd "C-x R") 'recentf)
;; M-x global-set-key RET 交互式的绑定你的键。
;; C-x Esc Esc 调出上一条“复杂命令”
;; 使用方式
;; (suk-set-key-bindings 'global-set-key
;;   (list
;;      '([f2]              calendar)
;;      '([(shift f2)]      remember)
;;      '([f5]              revert-buffer)
;;      (list (kbd "C-c l") 'copy-line)))



;; 设置绑定
(defun suk-set-key-bindings (ACTION BINDINGLIST)
  "Map keys.
  ACTION usually is 'global-set-key', and BINDINGLIST is key and command LIST."

  (mapcar (lambda(lst)
            ""
            (let ((x (car lst))
                  (y (car (last lst))))
              (funcall ACTION x y))) BINDINGLIST ))


(suk-set-key-bindings 'global-set-key
                      (list
                       (list (kbd "C-x M-a") 'align-regexp)
                       ;; '([S-f11]          insert-translated-name-insert) ;; Chinese to English
                       ;; '([S-f12]          toggle-company-english-helper) ;; popup English tips
                       ;; '([S-f2]           suk/new-empty-buffer)
                       ;; '([f2]                hs-toggle-hiding)
                       ;;'([M-f12]             vterm)
                       ;; '([S-f1]              snails)
                       (list (kbd "C-(") 'backward-sexp)
                       (list (kbd "C-)") 'forward-sexp)
                       (list (kbd "C-x t T") 'suk/toggle-transparency)
                       (list (kbd "C-x t p") 'suk/toggle-toggle-proxy)
                       (list (kbd "C-x t f") 'global-flycheck-mode)
                       (list (kbd "C-x R") 'recentf-open)
                       (list (kbd "C-<f11>")  'toggle-frame-fullscreen)
                       ;; (list (kbd "C-S-f")  'toggle-frame-fullscreen) ; Compatible with macOS
                       (list (kbd "M-S-<return>")  'toggle-frame-fullscreen)
                       ;; 创建新行的动作
                       (list (kbd "RET") 'newline-and-indent) ;; 回车时创建新行并且对齐
                       (list (kbd "S-<return>") 'comment-indent-new-line) ;; 取消对齐创建的新行

                       ))



;; bind-keys 是由 use-package 宏提供的一个功能，允许在一个声明中绑定多个键。虽然
;; bind-keys 可以独立于 use-package 使用，但它通常与 use-package 结合使用，以提
;; 供更清晰和模块化的键绑定配置。
;; (use-package bind-key)
;; (bind-key "C-c x" #'some-function some-package-mode-map)
;; (bind-key "C-c y" #'another-function)

;; Toggle fullscreen <F11> also bind to fullscreen
;; (bind-keys ("C-<f11>" . toggle-frame-fullscreen)
;;            ("C-S-f" . toggle-frame-fullscreen) ; Compatible with macOS
;;            ("M-S-<return>" . toggle-frame-fullscreen) ; Compatible with Windos
;;            )

(unless sys/win32p
  (global-set-key  (kbd "C-S-SPC") 'set-mark-command))

;; C-c TAB indent-region
;; C-u C-c TAB => (un)indent-region



;; has set to f7, c-f7
;;(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
;;(global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))

;; default keys: C-x LEFT/RIGHT C-, C-.
(global-set-key (kbd "<C-S-iso-lefttab>") 'previous-buffer)
(global-set-key (kbd "<C-tab>") 'next-buffer)
;;(global-set-key (kbd "C-x C-b") 'buffer-menu)
;;(global-set-key (kbd "C-x C-b") 'ibuffer)

;; (define-prefix-command 'my-leader) ;设定leader
;; (define-key keymap "keystrok" 'command-name)   ;将快捷键绑定到 leader按键后，即和键位图绑定。
;; (global-set-key "keystroke" 'command-name) ; 定义全局快捷键
;; (local-set-key  "keystroke" 'command-name) ; 定义局部快捷键
;;注意：keystroke中的Control 和 Alternative使用\C, \M表示。
;;如果是kbd函数，可以使用C和M表示

;; 方式一：
;;(define-prefix-command 'SPC-map)
;;(global-set-key (kbd "SPC") 'SPC-map)
;;(global-set-key (kbd "SPC f") 'find-file)

;; 方式二：
;;(define-prefix-command 'SPC-map)
;;(global-set-key (kbd "SPC") #'SPC-map)
;;(define-key SPC-map (kbd "f") #'find-file)


;; 演示了如何定义一个新的按键前缀. 这里定义了M-c作为按键前缀.
;; (define-prefix-command 'comma-map)
;; (global-set-key (kbd ",") 'comma-map)
;; (global-set-key [(meta c)] 'meta-c-map)
;; 演示了如何在一个模式下(这里是isearch模式), 定义快捷键. 退出isearch-mode, 所有按键失效.
;; (add-hook 'isearch-mode-hook
;;        '(lambda ()
;;           ;; 搜索下一个结果
;;           (define-key isearch-mode-map [(meta n)] 'isearch-repeat-forward)
;;           ;; 搜索前一个结果
;;           (define-key isearch-mode-map [(meta p)] 'isearch-repeat-backward)
;;           ;; 替换
;;           (define-key isearch-mode-map [(control r)] 'isearch-query-replace)
;;           ;; 正则替换
;;           (define-key isearch-mode-map [(meta 5)] 'isearch-query-replace-regexp)
;;           (define-key isearch-mode-map [(meta f)] 'isearch-yank-word-or-char)
;;           ;; 剪切板作为搜索内容
;;           (define-key isearch-mode-map [(meta y)] 'isearch-yank-kill)
;;           ;; 将光标到行尾作为搜索内容
;;           (define-key isearch-mode-map [(meta k)] 'isearch-yank-line)
;;           (define-key isearch-mode-map [(hyper l)] 'isearch-yank-char)
;;           ;; 向左或向右(选择/取消)单个字符作为搜索内容
;;           (define-key isearch-mode-map [(hyper j)] 'isearch-delete-char)
;;           ;; 显示occur视图
;;           (define-key isearch-mode-map [(meta o)] 'isearch-occur)
;;           ;; 单词搜索
;;           (define-key isearch-mode-map [(meta w)] 'isearch-forward-word)
;;           (define-key isearch-mode-map [(meta s)] 'isearch-repeat-forward)
;;           ))



;;Emacs 自动排版
;;很简单：C-x h C-M-\
;;其中C-x h 是全选
;;C-M-\ 是排版

;; C-x C-q set/unset readonly
;; 大小写转换： M-u, M-l, M-c

;; M-x align-regexp 可以方便的对齐一些文字

;;; rectangle
;; C-x r k
;; Kill the text of the region-rectangle, saving its contents as the last killed rectangle (kill-rectangle).
;; C-x r M-w
;; Save the text of the region-rectangle as the last killed rectangle (copy-rectangle-as-kill).
;; C-x r d
;; Delete the text of the region-rectangle (delete-rectangle).
;; C-x r y
;; Yank the last killed rectangle with its upper left corner at point (yank-rectangle).
;; C-x r o
;; Insert blank space to fill the space of the region-rectangle (open-rectangle). This pushes the previous contents of the region-rectangle to the right.
;; C-x r N
;; Insert line numbers along the left edge of the region-rectangle (rectangle-number-lines). This pushes the previous contents of the region-rectangle to the right.
;; C-x r c
;; Clear the region-rectangle by replacing all of its contents with spaces (clear-rectangle).
;; M-x delete-whitespace-rectangle
;; Delete whitespace in each of the lines on the specified rectangle, starting from the left edge column of the rectangle.
;; C-x r t string <RET>
;; Replace rectangle contents with string on each line (string-rectangle).
;; M-x string-insert-rectangle <RET> string <RET>
;; Insert string on each line of the rectangle.
;; C-x <SPC>
;; Toggle Rectangle Mark mode (rectangle-mark-mode). When this mode is active, the region-rectangle is highlighted and can be shrunk/grown, and the standard kill and yank commands operate on it.
;; The rectangle operations fall into two classes: commands to erase or insert rectangles, and comm

;; f3 start macro(kmacro-start-macro-or-insert-counter),
;; f4 done macro or run marcro (kmacro-end-or-call-macro).
;; C-x ( start macro (kmacro-start-macro),
;; C-x ) end done marco,
;; C-x e run marco(kmacro-end-macro)
;; 先定义一个宏
;; 然后 name-last-kbd-macro
;; 然后 insert-kbd-macro
;; 等到如下类似的配置
(fset 'delete-empty-lines (kbd "M-x flush-lines RET ^\s-*$ RET"))

(define-prefix-command 'leader-key)
(global-set-key (kbd "M-s-SPC") 'leader-key)

;; Delete
(global-set-key (kbd "C-c <backspace>") #'hungry-delete-backward)
(global-set-key (kbd "C-c <delete>") #'hungry-delete-forward)

(global-set-key (kbd "C-c C-j") #'yas-expand)
