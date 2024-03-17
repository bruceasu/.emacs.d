;;; init-idle.el --- Configure file that can load when emacs idle.

;;; Commentary:
;;
;; Configure file that can load when emacs idle.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require


;;; Code:


(provide 'init-idle)

(eval-when-compile
  (require '+const)
  (require '+custom)
  (require 'init-package)
  )
;; (require 'pretty-lambdada)
;; (pretty-lambda-for-modes)


(setq suggest-key-bindings 1)             ;当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
(setq browse-kill-ring-quit-action        ;设置退出动作
      (quote save-and-restore))           ;保存还原窗口设置
(autoload 'hanconvert-region "hanconvert" ;简繁中文互相转换
  "Convert a region from simple chinese to tradition chinese or
from tradition chinese to simple chinese" t)
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
(setq minibuffer-message-timeout 1)       ;显示消息超时的时间
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
(setq inhibit-compacting-font-caches t)   ; Don’t compact font caches during GC.
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

(setq-default cursor-type 'box) ; 设置光标样式

;;; ### Advice ###
;;; --- 各种emacs行为建议
;; 在特定地模式下粘贴时自动缩进
(defadvice yank (after indent-region activate)
  "To make yank content indent automatically."
  (if (member major-mode '(emacs-lisp-mode
                           scheme-mode
                           lisp-mode
                           lisp-interaction-mode
                           c-mode
                           c++-mode
                           objc-mode
                           latex-mode
                           plain-tex-mode))
      (indent-region (region-beginning) (region-end) nil)))

;;; ### Speedbar ###
(setq speedbar-show-unknown-files t)    ;显示文件

;;; ### Modeline-posn-column-limit ###
(setq modelinepos-column-limit 80)      ;设置列数限制, 并在mode-line上显示

;;; enable winner mode globally for undo/redo window layout changes
(winner-mode t)
(show-paren-mode t)
(tooltip-mode -1)                       ;不要显示任何 tooltips
(delete-selection-mode 1)               ; 选中文本后输入会覆盖
(auto-compression-mode 1)
(size-indication-mode 1)
(blink-cursor-mode -1)

;; 如果有两个重名buffer, 则再前面加上路径区别
(require 'uniquify)
;; (setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; chmod +x
;; ref. http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#chmod
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; =========================================================
;;备份策略
;; =========================================================
(setq make-backup-files t)
(setq version-control t)     ; 允许多次备份
(setq kept-old-versions 2)   ; 保留最早的2个备份文件
(setq kept-new-version 100)  ; 保留最近的100个备份文件
(setq delete-old-versions t) ; 自动删除旧的备份文件

;; 回到关闭文件前光标的位置
(use-package saveplace
  :ensure nil
  :defer 1
  :hook (after-init . save-place-mode)
  :init (setq save-place-file (expand-file-name "saveplace" suk-emacs-var-dir) ; "~/.emacs.d/var/saveplace"
  ))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300
              savehist-file (expand-file-name "history" suk-emacs-var-dir) ; "~/.emacs.d/var/history"
))

;; =========================================================
;; Start server
;; =========================================================
(use-package server
   :ensure t
   :hook (after-init . server-mode))
;; Emacs可以做为一个server, 然后用emacsclient连接这个server,
;; 无需再打开两个Emacs，windows下还不支持daemon的方式。
;;(server-force-delete)
;;(server-start)

;; Misc.
(use-package simple
  :ensure nil
  :hook ((after-init . size-indication-mode)
         (text-mode . visual-line-mode)
         ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        kill-whole-line t               ; Kill line including '\n'
        line-move-visual nil
        track-eol t                     ; Keep cursor at end of lines. Require line-move-visual is nil.
        set-mark-command-repeat-pop t)  ; Repeating C-SPC after popping mark pops it again

  ;; Visualize TAB, (HARD) SPACE, NEWLINE
  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace ()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

  ;; Prettify the process list
  (with-no-warnings
    (defun my-list-processes--prettify ()
      "Prettify process list."
      (when-let ((entries tabulated-list-entries))
        (setq tabulated-list-entries nil)
        (dolist (p (process-list))
          (when-let* ((val (cadr (assoc p entries)))
                      (name (aref val 0))
                      (pid (aref val 1))
                      (status (aref val 2))
                      (status (list status
                                    'face
                                    (if (memq status '(stop exit closed failed))
                                        'error
                                      'success)))
                      (buf-label (aref val 3))
                      (tty (list (aref val 4) 'face 'font-lock-doc-face))
                      (thread (list (aref val 5) 'face 'font-lock-doc-face))
                      (cmd (list (aref val 6) 'face 'completions-annotations)))
            (push (list p (vector name pid status buf-label tty thread cmd))
		          tabulated-list-entries)))))
    (advice-add #'list-processes--refresh :after #'my-list-processes--prettify)))

(when (or sys/mac-x-p sys/linux-x-p (daemonp))
  (use-package exec-path-from-shell
    :custom (exec-path-from-shell-arguments '("-l"))
    :init (exec-path-from-shell-initialize)))

;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000)) ; 16MB

;; Sqlite
(when (fboundp 'sqlite-open)
  (use-package emacsql-sqlite-builtin))
;;===================================================
;; Proxy settings
;;===================================================
;;;###autoload
(defun suk/proxy-http-show ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is \"%s\"" suk-proxy)
    (message "No proxy")))

;;;###autoload
(defun suk/proxy-http-enable ()
  "Enable http/https proxy."
  (interactive)
  (setq url-proxy-services `(("http" . suk-proxy)
                             ("https" . suk-proxy)
                             ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (suk/proxy-http-show))

;;;###autoload
(defun suk/proxy-http-disable ()
  "Disable http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (suk/proxy-http-show))

;;;###autoload
(defun suk/proxy-http-toggle ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
      (suk/proxy-http-disable)
    (suk/proxy-http-enable)))

;;;###autoload
(defun suk/proxy-socks-enable ()
  "Enable Socks proxy."
  (interactive)
  (setq url-gateway-method 'socks)
  (setq socks-noproxy '("localhost"))
  (setq socks-server '("Default server" "127.0.0.1" 1080 5))
  (message "Enable socks proxy."))

;;;###autoload
(defun suk/proxy-socks-disable ()
  "Disable Socks proxy."
  (interactive)
  (setq url-gateway-method 'native)
  (setq socks-noproxy nil)
  (message "Disable socks proxy."))

;;===================================================
;; Byte Compile
;;===================================================
;;;###autoload
(defun byte-compile-elpa ()
  "Compile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))

;;;###autoload
(defun byte-compile-extensions ()
  "Compile packages in extensions directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "extensions")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))

;;;###autoload
(defun native-compile-elpa ()
  "Native-compile packages in elpa directory."
  (interactive)
  (if (fboundp 'native-compile-async)
      (native-compile-async package-user-dir t)))

;;;###autoload
(defun native-compile-extensions ()
  "Native compile packages in extensions directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "extensions")))
    (if (fboundp 'native-compile-async)
        (native-compile-async dir t))))

;;;###autoload
(defun suk-set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.

  Save to option `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save)
             (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward
              (format "^[\t ]*[;]*[\t ]*(setq %s .*)" variable)
              nil t)
        (replace-match (format "(setq %s '%s)" variable value) nil nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))



;;===================================================
;; Update
;;===================================================
;;;###autoload
(defun update-config ()
  "Update Suk's Emacs configurations to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (unless (file-exists-p dir)
      (user-error "\"%s\" doesn't exist" dir))

    (message "Updating configurations...")
    (cd dir)
    (shell-command "git pull")
    (message "Updating configurations...done")))
(defalias 'suk-update-config #'update-config)

;;;###autoload
(defun update-packages ()
  "Refresh package contents and update all packages."
  (interactive)
  (message "Updating packages...")
  (package-upgrade-all)
  (message "Updating packages...done"))
(defalias 'suk-update-packages #'update-packages)

;;;###autoload
(defun update-config-and-packages()
  "Update confgiurations and packages."
  (interactive)
  (update-config)
  (update-packages))
(defalias 'suk-update #'update-config-and-packages)

;;;###autoload
(defun update-dotfiles ()
  "Update the dotfiles to the latest version."
  (interactive)
  (let ((dir (or (getenv "DOTFILES")
                 (expand-file-name "~/.dotfiles/"))))
    (if (file-exists-p dir)
        (progn
          (message "Updating dotfiles...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating dotfiles...done"))
      (message "\"%s\" doesn't exist" dir))))
(defalias 'suk-update-dotfiles #'update-dotfiles)

;;;###autoload
(defun update-org ()
  "Update Org files to the latest version."
  (interactive)
  (let ((dir (expand-file-name "~/org/")))
    (if (file-exists-p dir)
        (progn
          (message "Updating org files...")
          (cd dir)
          (shell-command "git pull")
          (message "Updating org files...done"))
      (message "\"%s\" doesn't exist" dir))))
(defalias 'suk-update-org #'update-org)

;;;###autoload
(defun update-all()
  "Update dotfiles, org files, configurations and packages to the latest."
  (interactive)
  (update-org)
  (update-dotfiles)
  (update-config-and-packages))
(defalias 'suk-update-all #'update-all)

;; ==================================================
;; Terminate
;; ==================================================
;;;###autoload
(defun term()
  "Use Bash in windows."
  (interactive)
  (if sys/win32p
	  (let (
			(shell-file-name windows-bash-path)
			)
		(call-interactively 'shell))
    (let ((explicit-shell-file-name "/bin/bash"))
	  (call-interactively 'shell)))
  )


;; Misc
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))
