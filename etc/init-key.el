;; Mac平台下交换 Option 和 Command 键。
;;(when (featurep 'cocoa)
;;  (setq mac-option-modifier 'super)
;;  (setq mac-command-modifier 'meta))

;;; ### Unset key ###
;;; --- 卸载按键
(lazy-load-unset-keys                   ;全局按键的卸载
 '("C-z" "s-W"))

;;; ### Sdcv ###
;;; --- 星际译王命令行
(lazy-load-global-keys
 '(("p" . sdcv-search-pointer)          ;光标处的单词, buffer显示
   ("P" . sdcv-search-pointer+)         ;光标处的单词, tooltip显示
   ("i" . sdcv-search-input)            ;输入的单词, buffer显示
   (";" . sdcv-search-input+)
   ("y" . youdao-dictionary-search-at-point)
   ("Y" . youdao-dictionary-search-at-point-tooltip)
   )
 "init-translate"
 "C-z")

;;; ### Insert translated name ###
(lazy-load-global-keys
 '(
   ("," . insert-translated-name-insert-with-underline)
   ("." . insert-translated-name-insert-with-camel)
   )
 "insert-translated-name"
 "C-z"
 )
 
(lazy-load-global-keys
 '(
    ("s-i" . insert-translated-name-insert)
 )
 "init-insert-translated-name")


;;; ### Toolkit ###
;;; --- 工具函数
(lazy-load-set-keys
 '(
;;   ("s-c o" . one-key-menu-directory)   ;目录打开菜单
   ("C-<" . bury-buffer)                ;隐藏当前buffer
   ("C->" . unbury-buffer)              ;反隐藏当前buffer
   ("s-[" . eval-expression)            ;执行表达式
   ("C-S-q" . quoted-insert)            ;读取系一个输入字符并插入
   ("M-h" . set-mark-command) ;Instead C-Space for Chinese input method
   ("M-H" . set-mark-command) ;Instead C-Space for Chinese input method
   ))
 
;;; ### Color-Rg ###
;;; --- 搜索重构
(lazy-load-global-keys
 '(
   ("C-z g" . color-rg-search-symbol)
   ("C-z h" . color-rg-search-input)
   ("C-z j" . color-rg-search-symbol-in-project)
   ("C-z k" . color-rg-search-input-in-project)
   ("C-z ," . color-rg-search-symbol-in-current-file)
   ("C-z ." . color-rg-search-input-in-current-file)
   )
 "color-rg")
 

;;; ### Watch other window ###
;;; --- 滚动其他窗口
(lazy-load-global-keys
 '(
   ("M-J" . other-window-move-up)        ;向下滚动其他窗口
   ("M-K" . other-window-move-down)      ;向上滚动其他窗口
   ("M-<" . window-move-up)              ;向下滚动当前窗口
   ("M->" . window-move-down)            ;向上滚动当前窗口
   )
"win-move")

;;; ### Buffer Move ###
;;; --- 缓存移动
(lazy-load-set-keys
 '(
   ("C-z k" . beginning-of-buffer)      ;缓存开始
   ("C-z j" . end-of-buffer)            ;缓存结尾
   ("C-M-f" . forward-paragraph)        ;下一个段落
   ("C-M-b" . backward-paragraph)       ;上一个段落
   ("C-M-y" . backward-up-list)         ;向左跳出 LIST
   ("C-M-o" . up-list)                  ;向右跳出 LIST
   ("C-M-u" . backward-down-list)       ;向左跳进 LIST
   ("C-M-i" . down-list)                ;向右跳进 LIST
   ("C-M-a" . beginning-of-defun)       ;函数开头
   ("C-M-e" . end-of-defun)             ;函数末尾
   ))

 
(lazy-load-global-keys
 '(
   ("s-n" . move-text-down)      ;把光标所在的整行文字(或标记)下移一行
   ("s-p" . move-text-up)        ;把光标所在的整行文字(或标记)上移一行
   )
 "move-text")
 
(lazy-load-global-keys
 '(
   ("C-S-o" . duplicate-line-or-region-above) ;向上复制当前行或区域
   ("C-S-l" . duplicate-line-or-region-below) ;向下复制当前行或区域
   ("C-S-s-o" . duplicate-line-above-comment) ;复制当前行到上一行, 并注释当前行
   ("C-S-s-l" . duplicate-line-below-comment) ;复制当前行到下一行, 并注释当前行
   ("C-:" . comment-or-uncomment-region+)     ;注释当前行
   )
 "duplicate-line")
 
(lazy-load-global-keys
 '(
   ("C-o" . open-newline-above)         ;在上面一行新建一行
   ("C-l" . open-newline-below)         ;在下面一行新建一行
   )
 "open-newline")
 

 
;;; ### Buffer Edit ###
;;; --- 缓存编辑
(lazy-load-set-keys
 '(
   ("C-x C-x" . exchange-point-and-mark)   ;交换当前点和标记点
   ("M-o" . backward-delete-char-untabify) ;向前删除字符
   ("C-M-S-h" . mark-paragraph)            ;选中段落
   ("M-SPC" . just-one-space)              ;只有一个空格在光标处
   ))
   
(lazy-load-global-keys
 '(
   ("C-/" . undo-tree-undo)             ;撤销
   ("C-?" . undo-tree-redo)             ;重做)
   )
 "undo-tree")
 


;;; ### Font ###
;;; --- 字体命令
(lazy-load-set-keys
 '(
   ("s-_" . text-scale-decrease)        ;减小字体大小
   ("s-+" . text-scale-increase)        ;增加字体大小
   ))
   


;;; ### Window Operation ###
;;; --- 窗口操作
(lazy-load-set-keys
 '(
   ("C-x :" . split-window-vertically)   ;纵向分割窗口
   ("C-x |" . split-window-horizontally) ;横向分割窗口
   ("C-;" . kill-this-buffer)            ;关闭当前buffer
   ("C-x ;" . delete-other-windows)      ;关闭其它窗口
   ))
   


;;; ### Awesome-Tab ###
;;; --- 多标签浏览
(lazy-load-set-keys
 '(
   ("s-j" . awesome-tab-ace-jump)        ;Ace jump
   ("M-7" . awesome-tab-backward-tab)    ;移动到后一个标签
   ("M-8" . awesome-tab-forward-tab)     ;移动到前一个标签
   ("M-9" . awesome-tab-backward-group)  ;移动到后一个标签组
   ("M-0" . awesome-tab-forward-group)   ;移动到前一个标签组
   ("<C-tab>" . awesome-tab-forward-tab) ;移动到后一个标签
   ("<C-S-iso-lefttab>" . awesome-tab-backward-tab) ;移动到前一个标签
   ))

(lazy-load-global-keys
 '(
   ("M-&" . awesome-tab-backward-tab-other-window)
   ("M-*" . awesome-tab-forward-tab-other-window)
   ("M-s-7" . awesome-tab-select-beg-tab)
   ("M-s-8" . awesome-tab-select-end-tab)
   ("M-s-9" . awesome-tab-move-current-tab-to-beg)
   ("s-q" . awesome-tab-kill-other-buffers-in-current-group)
   ("s-Q" . awesome-tab-kill-all-buffers-in-current-group)
   ("s-w" . awesome-tab-keep-match-buffers-in-current-group)
   ("s-W" . awesome-tab-kill-match-buffers-in-current-group)
   )
 "awesome-tab")

   
;;; ### Awesome-Pair ###
;;; --- 结构化编程
;;(lazy-load-unset-keys
;; '("M-J" "M-r" "M-s" "M-;" "C-M-f" "C-M-b" "M-)")
;; awesome-pair-mode-map)                 ;卸载按键
;;(defvar awesome-pair-key-alist nil)
;;(setq awesome-pair-key-alist
;;      '(
;;        ;; 移动
;;        ("M-n" . awesome-pair-jump-left)
;;        ("M-p" . awesome-pair-jump-right)
;;        ;; 符号插入
;;        ("%" . awesome-pair-match-paren)       ;括号跳转
;;        ("(" . awesome-pair-open-round)        ;智能 (
;;        ("[" . awesome-pair-open-bracket)      ;智能 [
;;        ("{" . awesome-pair-open-curly)        ;智能 {
;;        (")" . awesome-pair-close-round)       ;智能 )
;;        ("]" . awesome-pair-close-bracket)     ;智能 ]
;;        ("}" . awesome-pair-close-curly)       ;智能 }
;;        ("\"" . awesome-pair-double-quote)     ;智能 "
;;        ("=" . awesome-pair-equal)             ;智能 =
;;        ("SPC" . awesome-pair-space)           ;智能 Space
;;        ;; 删除
;;        ("M-o" . awesome-pair-backward-delete) ;向后删除
;;        ("C-d" . awesome-pair-forward-delete)  ;向前删除
;;        ("C-k" . awesome-pair-kill)            ;向前kill
;;        ;; 包围
;;        ("M-\"" . awesome-pair-wrap-double-quote) ;用 " " 包围对象, 或跳出字符串
;;        ("M-[" . awesome-pair-wrap-bracket)       ;用 [ ] 包围对象
;;        ("M-{" . awesome-pair-wrap-curly)         ;用 { } 包围对象
;;        ("M-(" . awesome-pair-wrap-round)         ;用 ( ) 包围对象
;;        ("M-)" . awesome-pair-unwrap)             ;去掉包围对象
;;        ;; 跳出并换行缩进
;;        ("M-:" . awesome-pair-jump-out-pair-and-newline) ;跳出括号并换行
;;        ))
;;        
;;(lazy-load-set-keys awesome-pair-key-alist awesome-pair-mode-map)

;;; ### Dired ###
;;; --- Dired
(lazy-load-global-keys
 '(
   ("<f8>" . dired-jump)
   ("C-x C-f" . find-file)
   )
 "init-dired")


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
 
;;; ### Flycheck ###
;;; --- 及时拼写检查
(lazy-load-global-keys
 '(
   ("M-s-j" . flycheck-next-error)      ;显示下一个错误
   ("M-s-k" . flycheck-previous-error)  ;显示上一个错误
   )
 "init-flycheck"
 )



;;; ### Ace jump ###
(lazy-load-global-keys
 '(
   ("s-<" . ace-jump-word-mode)
   ("s->" . ace-jump-char-mode)
   ("s-?" . ace-jump-line-mode)
   )
 "ace-jump-mode")
 

;;; ### Company en words ###
;;; --- 英文助手
(lazy-load-global-keys
 '(
   ("M-r" . toggle-company-english-helper) ;英文助手
   )
 "company-english-helper")
;;; ### Ido ###
;;; --- 交互式管理文件和缓存
(lazy-load-set-keys
 '(
   ("C-x C-f" . ido-find-file)          ;交互式查找文件
   ("C-x b" . ido-switch-buffer)        ;交互式切换buffer
   ("C-x i" . ido-insert-buffer)        ;插入缓存
   ("C-x I" . ido-insert-file)          ;插入文件
   ))
(add-hook 'ido-setup-hook
          '(lambda ()
             (interactive)
             (ido-my-keys ido-completion-map)))
(defun ido-my-keys (keymap)
  "Add my keybindings for ido."
  (lazy-load-set-keys
   '(
     ("M-s-p" . ido-prev-match)              ;上一个匹配
     ("M-s-n" . ido-next-match)              ;下一个匹配
     ("M-s-h" . ido-next-work-directory)     ;下一个工作目录
     ("M-s-l" . ido-prev-work-directory)     ;上一个工作目录
     ("M-o" . backward-delete-char-untabify) ;向前删除字符
     ("M-O" . ido-delete-backward-updir)     ;删除字符或进入上一级目录
     )
   keymap
   ))


;;; Elisp
(lazy-load-set-keys
 '(
   ("RET" . comment-indent-new-line)    ;自动换行并注释
   )
 emacs-lisp-mode-map
 )


 
;;; ### Magit ###
;;
(lazy-load-global-keys
 '(
   ("C-x g" . magit-status)
   ("C-x M-g" . magit-dispatch)
   ("C-c M-g" . magit-file-popup))
 "init-vcs")


(lazy-load-global-keys
 '(
   ("s-y" . snails)
   ("s-u" . snails-search-point)
   )
 "snails")

(lazy-load-global-keys
 '(
   ("C-7" . xref-pop-marker-stack)
   ("C-8" . xref-find-definitions)
   ("C-9" . xref-find-definitions-other-window)
   ("M-k" . xref-find-references)
   ("M-," . nox-rename)
   ("M-." . nox-show-doc)
   )
 "init-nox.el")

 
 
;;; ### Org ###
;;; --- 笔记管理和组织
(define-prefix-command 'F9-map)
(global-set-key (kbd "<f9>") 'F9-map)

(lazy-load-global-keys
 '(("a" . org-agenda)
   ("s" . show-org-agenda)
   ("c" . org-capture)
   ("i" . org-toggle-inline-images)
   ("l" . org-toggle-link-display)
   ("d" . calendar)
   ("f" . boxquote-insert-file)
   ("r" . boxquote-region)
   ("v" . visible-mode))
 "init-org"
 "<f9>")



;;; ### Keyboard Macro ###
;;; --- 键盘宏
;;(lazy-load-global-keys
;; '(
;;   ("M-s-s" . kmacro-start-macro-or-insert-counter) ;开始键盘宏或插入
;;   ("M-s-d" . kmacro-end-or-call-macro)             ;结束键盘宏或调用
;;   ("M-s-c" . kmacro-delete-ring-head)              ;删除当前的键盘宏
;;   ("M-s-w" . kmacro-cycle-ring-next)               ;下一个键盘宏
;;   ("M-s-e" . kmacro-cycle-ring-previous)           ;上一个键盘宏
;;   ("M-s-a" . kmacro-edit-macro)                    ;编辑键盘宏
;;   ("M-s-v" . name-last-kbd-macro)                  ;命令当前键盘宏
;;   ("M-s-f" . insert-kbd-macro)                     ;插入键盘宏
;;   ("M-s-q" . apply-macro-to-region-lines) ;应用键盘宏到选择的区域
;;   )
;; "macros+")

;;; ### EAF ###
;;; EAF
;;(unless (featurep 'cocoa)
;;  (lazy-load-global-keys
;;   '(
;;     ("s-'" . eaf-open)
;;     ("s-\"" . eaf-open-browser)
;;     ("s-/" . eaf-open-terminal)
;;     )
;;   "init-eaf")
;;  (lazy-load-local-keys
;;   '(
;;     ("H" . eaf-open-this-from-dired)
;;     )
;;   dired-mode-map
;;   "init-eaf"))

;;; ### Aweshell ###
;;; --- 多标签式的shell
;;(lazy-load-global-keys
;; '(
;;   ("s-n" . aweshell-new)
;;   ("s-h" . aweshell-toggle)
;;   ("s-x s-x" . aweshell-dedicated-toggle)
;;   )
;; "aweshell")
;;; ### IRC ###
;;; --- 聊天
;;(lazy-load-global-keys
;; '(
;;   ("C-c i" . switch-to-erc)            ;切换到IRC或自动登录IRC
;;   ("C-c I" . erc-nick-notify-jump-last-channel) ;自动跳转到最后收到消息的频道
;;   )
;; "init-erc")

;;(lazy-load-global-keys
;; '(
;;   ("C-z l" . display-line-numbers-mode) ;行号模式切换
;;   ("M-s-n" . comment-part-move-down)    ;向下移动注释
;;   ("M-s-p" . comment-part-move-up)      ;向上移动注释
;;   ("C-s-n" . comment-dwim-next-line)    ;移动到上一行并注释
;;   ("C-s-p" . comment-dwim-prev-line)    ;移动到下一行并注释
;;   ("M-2" . indent-buffer)               ;自动格式化当前Buffer
;;   ("M-z" . upcase-char)      ;Upcase char handly with capitalize-word
;;   ("C-x u" . mark-line)      ;选中整行
;;   ("s-k" . kill-and-join-forward)      ;在缩进的行之间删除
;;   ("M-G" . goto-column)                ;到指定列
;;   ("C->" . remember-init)              ;记忆初始函数
;;   ("C-<" . remember-jump)              ;记忆跳转函数
;;   ("M-s-," . point-stack-pop)          ;buffer索引跳转
;;   ("M-s-." . point-stack-push)         ;buffer索引标记
;;   ("s-g" . goto-percent)    ;跳转到当前Buffer的文本百分比, 单位为字符
;;   ("M-I" . backward-indent) ;向后移动4个字符
;;   ("s-J" . scroll-up-one-line)         ;向上滚动一行
;;   ("s-K" . scroll-down-one-line)       ;向下滚动一行
;;   ("<f2>" . refresh-file)              ;自动刷新文件
;;   ("s-f" . find-file-root)             ;用root打开文件
;;   ("s-r" . find-file-smb)              ;访问sambao
;;   )
;; "basic-toolkit")

;;; Dash.
;;(lazy-load-global-keys
;; '(("y" . dash-at-point)
;;   )
;; "dash-at-point"
;; "C-x"
;; )
 
(provide 'init-key)
