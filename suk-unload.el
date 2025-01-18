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

(when (display-graphic-p)
  (use-package centaur-tabs
    :demand
    :init
    ;; Set the style to rounded with icons
    (setq centaur-tabs-style "bar")
    (setq centaur-tabs-set-icons t)
    :config
    (centaur-tabs-mode t)
    :bind
    ("C-<prior>" . centaur-tabs-backward)  ;; Ctrl PgUp
    ("C-<next>"  . centaur-tabs-forward))  ;; Ctrl PgDn
)

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

(require-package 'command-log-mode) ;; show the command you press the shortcuts. M-x command-log-mode, M-x clm/open-command-log-buffer

(require 'eglot)
;;(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-10"))
;;(add-hook 'c-mode-hook 'eglot-ensure)
;;(add-hook 'c++-mode-hook 'eglot-ensure)

;; Java 需要JDK17+
(setenv "JAVA_HOME" "~/apps/jdk-21.0.5")
(setenv "PATH" (concat "$PATH:~/apps/jdk-21.0.5/bin"))
(defconst my-eclipse-jdt-home "/home/suk/.local/share/eclipse.jdt.ls/plugins/org.eclipse.equinox.launcher_1.6.700.v20231214-2017.jar")

(defun my-eglot-eclipse-jdt-contact (interactive)
  "Contact with the jdt server input INTERACTIVE."
  (let ((cp (getenv "CLASSPATH")))
    (setenv "CLASSPATH" (concat cp ":" my-eclipse-jdt-home))
    (unwind-protect (eglot--eclipse-jdt-contact nil)
      (setenv "CLASSPATH" cp))))
(setcdr (assq 'java-mode eglot-server-programs) #'my-eglot-eclipse-jdt-contact)
(add-hook 'java-mode-hook 'eglot-java-mode)

;;Show function arglist or variable docstring
(run-with-idle-timer
 1
 nil
 #'(lambda()
     (use-package eldoc
       :ensure nil
       :diminish
       :config
       (use-package eldoc-box
           :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
           :custom
           (eldoc-box-lighter nil)
           (eldoc-box-only-multi-line t)
           (eldoc-box-clear-with-C-g t)
           :custom-face
           (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
           (eldoc-box-body ((t (:inherit tooltip))))
           :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
           :config
           ;; Prettify `eldoc-box' frame
           (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
                 (alist-get 'right-fringe eldoc-box-frame-parameters) 8)))
     ))
