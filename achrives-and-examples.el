;;注意：keystroke中的Control 和 Alternative使用\C, \M表示。
;;如果是kbd函数，可以使用C和M表示

;; global examples:
(global-set-key (kbd "C-x C-\\") #'next-line)
(global-set-key [?\C-x ?\C-\\] #'next-line)
(global-set-key [(control ?x) (control ?\\)] #'next-line)
(keymap-global-set [(control ?x) (control ?\\)] #'next-line)
(keymap-set global-map "C-x" #'next-line)

;; remove a keybinding for emacs 29 or after
(keymap-global-set "C-t" nil)

;; or use
(keymap-global-unset "C-t")

;; before emacs 28 or before
(global-set-key (kbd "C-t") nil)

;; or
(global-unset-key (kbd "C-t"))

;; 自己定义 keymap 其实很简单的，使用 defvar-keymap ，可以从上面的配置中
;; 抽取 C-c n 作为一个 prefix key ，定义如下：
(defvar-keymap hsk/notetaking-map
  :doc "My notetaking keymap"
  "d" #'denote
  "t" #'denote-keywords-add
  "T" #'denote-keywords-remove)
(global-set-key (kbd "C-c n") hsk/notetaking-map)

;; 来将上面定义的 keymap 绑定到 C-c n 上。不过从 emacs-29 开始，更推荐用
;; keymap-set ，上面的代码可以改成：
(keymap-set global-map "C-c n" hsk/notetaking-map)
(keymap-global-set "C-c n" hsk/notetaking-map)

;; 或者使用旧方法
;; (define-prefix-command hsk/notetaking-map)
(defvar hsk/notetaking-map (make-sparse-keymap)
  "Keymap for leader bindings.")

(global-set-key (kbd "C-c n") hsk/notetaking-map)
(define-key hsk/notetaking-map (kbd "d") #'denote)
(define-key hsk/notetaking-map (kbd "t") #'isearch-repeat-forward)


;; 新方法省去了 (kbd ...) 这一层，比较方便。

;; 比起原先每个快捷键单独定义，使用 keymap 好处是便于管理，比如之后我想更
;; 换快捷键前缀，从 C-c n 换成 C-c C-n ，只要改一处 keymap-set 就可以了。

;; 另一个优点是可以嵌套定义，比如我想要将 org-journal 相关的操作也挂到
;; hsk/notetaking-map 下面，可以先定义一个 hsk/org-journal-map ：

(defvar-keymap hsk/org-journal-map
  :doc "My org-journal keymap"
  "n" #'org-journal-new-entry
  "s" #'org-journal-search
  "o" #'org-journal-open-current-journal-file)

;; 然后将它嵌套进另一个 map 下面：
(defvar-keymap hsk/notetaking-map
  :doc "My notetaking keymap"
  ;;...
  "j" hsk/org-journal-map)

;; 这样 org-journal-new-entry 就映射到 C-c n j n 上了，非常优雅。


(let ((my-keymap (make-sparse-keymap))) ; 创建一个空的 keymap
  (keymap-set my-keymap "C-c f" 'find-file)) ; 设置 C-c f 来执行 'find-file 命令

(keymap-local-set "C-p" ctl-x-map)

;; 定义leader key 方式 （旧）
(define-prefix-command 'my-leader) 
(define-key keymap "keystrok" 'command-name)
(global-set-key "keystroke" 'command-name)
(local-set-key  "keystroke" 'command-name) 

;; 方式一：
(define-prefix-command 'SPC-map)
(global-set-key (kbd "SPC") 'SPC-map)
(global-set-key (kbd "SPC f") 'find-file)

;; 方式二：
(define-prefix-command 'SPC-map)
(global-set-key (kbd "SPC") #'SPC-map)
(define-key SPC-map (kbd "f") #'find-file)

;; 演示了如何定义一个新的按键前缀. 这里定义了M-c作为按键前缀.
(define-prefix-command 'comm-map)
(global-set-key (kbd "M-c") 'comm-map)
(global-set-key [(meta c)]  'comm-map)

;; 新方式更加直观明白
(defvar leader-key-map (make-sparse-keymap) "Keymap for leader key bindings.")
(global-set-key (kbd "M-m") leader-key-map)
(define-key leader-key-map (kbd "f") #'find-file)

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
