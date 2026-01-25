;;注意：keystroke中的Control 和 Alternative使用\C, \M表示。
;;如果是kbd函数，可以使用C和M表示

;; global examples:
;; --------------------------------------------------------
;; (global-set-key (kbd "C-x C-\\") #'next-line)
;; (global-set-key [?\C-x ?\C-\\] #'next-line)
;; (global-set-key [(control ?x) (control ?\\)] #'next-line)
;; (keymap-global-set [(control ?x) (control ?\\)] #'next-line)
;; (keymap-set global-map "C-x" #'next-line)

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
