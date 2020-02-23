(eval-when-compile
  (require 'init-custom))

(setq system-time-locale "C")

;; 编码设置 begin
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; 23.2 之后废弃，用buffer-file-coding-system
;(setq default-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;; for windows
;; (setq-default pathname-coding-system 'euc-cn)
;; (set-language-environment 'Chinese-GB)
;; 据说设置为UTF-8不会卡顿
(set-language-environment "UTF-8")
;;(setq file-name-coding-system 'euc-cn)
;;(setq file-name-coding-system 'gb18030)
;;(if is-os-windows
;;	(setq file-name-coding-system 'gb18030))

;; 重要提示:写在最后一行的，实际上最优先使用; 最前面一行，反而放到最后才识别。
;; utf-16le-with-signature 相当于 Windows 下的 Unicode 编码，这里也可写成
;; utf-16 (utf-16 实际上还细分为 utf-16le, utf-16be, utf-16le-with-signature等多种)
;; 繁体
(prefer-coding-system 'cp950)
;; 简体
(prefer-coding-system 'gb18030)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb2312)
;; Unicode
;(prefer-coding-system 'utf-16le-with-signature)
(prefer-coding-system 'utf-16)
;; 新建文件使用utf-8-unix方式
;; 如果不写下面两句，只写
;; (prefer-coding-system 'utf-8)
;; 这一句的话，新建文件以utf-8编码，行末结束符平台相关
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)
(setq session-save-file-coding-system 'utf-8)
(set-charset-priority 'unicode)

(auto-compression-mode 1)
(setq default-major-mode 'text-mode)
(setq show-paren-style 'parentheses)
;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
(setq suggest-key-bindings 1)
;;只渲染当前屏幕语法高亮，加快显示速度
(setq font-lock-maximum-decoration t)
;; 书签文件的路径及文件名
(setq bookmark-default-file "~/tmp/emacs/.emacs.bmk")
;; 同步更新书签文件 ;; 或者退出时保存
(setq bookmark-save-flag 1)
;;C-x r m (name)  M-x bookmark-set  设置书签
;;C-x r b (name)  M-x bookmark-jump  跳转到书签
;;C-x r l         M-x bookmark-bmenu-list  书签列表
;;                M-x bookmark-delete  删除书签
;;                M-x bookmark-load  读取存储书签文件
;;备份策略
(setq backup-directory-alist '(("" . "~/tmp/emacs/backup")))
(setq-default make-backup-file t)
(setq make-backup-file t)
(setq make-backup-files t)
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 10)
(setq delete-old-versions t)
;; 设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插 入两个空格。
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq adaptive-fill-regexp "[ \t]+\\|[ \t]*\\([0-9]+\\.\\|\\*+\\)[ \t]*")
(setq adaptive-fill-first-line-regexp "^\\* *$")

;; 开启行号显示
;; (global-linum-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
;;(display-time-mode 1)

;; 简写模式
;;(setq-default abbrev-mode t)
;;(setq save-abbrevs nil)

;; Emacs可以做为一个server, 然后用emacsclient连接这个server,
;; 无需再打开两个Emacs，windows下还不支持daemon的方式。
;;(server-force-delete)
;; (server-start)

(provide 'init-settings)
