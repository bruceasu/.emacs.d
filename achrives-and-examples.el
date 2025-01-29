;; global-set-key examples:
(global-set-key (kbd "C-x C-\\") 'next-line)
(global-set-key [?\C-x ?\C-\\] 'next-line)
(global-set-key [(control ?x) (control ?\\)] 'next-line)

;; remove a keybinding

;; for emacs 29 or after
(keymap-global-set "C-t" nil)
;; or use
(keymap-global-unset "C-t")
;; before emacs 28 or before
(global-set-key (kbd "C-t") nil)
;; or
(global-unset-key (kbd "C-t"))


(define-prefix-command 'my-leader) ;设定leader
(define-key keymap "keystrok" 'command-name)   ;将快捷键绑定到 leader按键后，即和键位图绑定。
(global-set-key "keystroke" 'command-name) ; 定义全局快捷键
(local-set-key  "keystroke" 'command-name) ; 定义局部快捷键
;;注意：keystroke中的Control 和 Alternative使用\C, \M表示。
;;如果是kbd函数，可以使用C和M表示

;; 方式一：
(define-prefix-command 'SPC-map)
(global-set-key (kbd "SPC") 'SPC-map)
(global-set-key (kbd "SPC f") 'find-file)

;; 方式二：
(define-prefix-command 'SPC-map)
(global-set-key (kbd "SPC") #'SPC-map)
(define-key SPC-map (kbd "f") #'find-file)


;; 演示了如何定义一个新的按键前缀. 这里定义了M-c作为按键前缀.
(define-prefix-command 'comma-map)
(global-set-key (kbd ",") 'comma-map)
(global-set-key [(meta c)] 'meta-c-map)
;; 演示了如何在一个模式下(这里是isearch模式), 定义快捷键.
;; 退出isearch-mode, 所有按键失效.
(add-hook
  'isearch-mode-hook
  '(lambda ()
    ;; 搜索下一个结果
    (define-key isearch-mode-map [(meta n)] 'isearch-repeat-forward)
    ;; 搜索前一个结果
    (define-key isearch-mode-map [(meta p)] 'isearch-repeat-backward)
    ;; 替换
    (define-key isearch-mode-map [(control r)] 'isearch-query-replace)
    ;; 正则替换
    (define-key isearch-mode-map [(meta 5)] 'isearch-query-replace-regexp)
    (define-key isearch-mode-map [(meta f)] 'isearch-yank-word-or-char)
    ;; 剪切板作为搜索内容
    (define-key isearch-mode-map [(meta y)] 'isearch-yank-kill)
    ;; 将光标到行尾作为搜索内容
    (define-key isearch-mode-map [(meta k)] 'isearch-yank-line)
    (define-key isearch-mode-map [(hyper l)] 'isearch-yank-char)
    ;; 向左或向右(选择/取消)单个字符作为搜索内容
    (define-key isearch-mode-map [(hyper j)] 'isearch-delete-char)
    ;; 显示occur视图
    (define-key isearch-mode-map [(meta o)] 'isearch-occur)
    ;; 单词搜索
    (define-key isearch-mode-map [(meta w)] 'isearch-forward-word)
    (define-key isearch-mode-map [(meta s)] 'isearch-repeat-forward)
    ))

;;; ### Sdcv ###
;;; --- 星际译王命令行
(when  (eq system-type 'gnu/linux)
    (lazy-load-global-keys
     '(("p" . sdcv-search-pointer)           ;光标处的单词, buffer显示
       ("P" . sdcv-search-pointer+)          ;光标处的单词, tooltip显示
       ("i" . sdcv-search-input)             ;输入的单词, buffer显示
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
;; async-shell-command
