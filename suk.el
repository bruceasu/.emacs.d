(setq user-full-name "Suk")
(setq user-mail-address "bruceasu@gmail.com")

;; set const
(defconst custom-template-file
  (expand-file-name "custom-template.el" user-emacs-directory)
  "Custom template file of Suk's Emacs.")

(defconst suk-homepage
  "https://github.com/bruceasu/.emacs.d"
  "The Github page of this Emacs config.")


(defgroup suk nil
  "suk Emacs customizations."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/bruceasu/.emacs.d"))

(defcustom suk-icon t
  "Display icons or not."
  :group 'suk
  :type 'boolean)

(defcustom org-roam-directory (expand-file-name "RoamNotes" user-home-dir)
  "The org roam directory."
  :group 'suk
  :type 'string)

(defcustom org-files-directory (expand-file-name "org" user-home-dir)
  "The org roam directory."
  :group 'suk
  :type 'string)

(defcustom my-org-inline-css-file "~/.emacs.d/share/my-org-style-min.css"
  "The org css style file."
  :group 'suk
  :type 'string)

;;minibuffer childframe
(setq suk-completion-style "childframe")

(defcustom suk-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-"     . ?←)
    ("->"     . ?→)
    ("->>"    . ?↠)
    ("=>"     . ?⇒)
    ("map"    . ?↦)
    ("/="     . ?≠)
    ("!="     . ?≠)
    ("=="     . ?≡)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("=<<"    . (?= (Br . Bl) ?≪))
    (">>="    . (?≫ (Br . Bl) ?=))
    ("<=<"    . ?↢)
    (">=>"    . ?↣)
    ("&&"     . ?∧)
    ("||"     . ?∨)
    ("not"    . ?¬))
  "A list of symbol prettifications. Nil to use font supports ligatures."
  :group 'suk
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom suk-prettify-org-symbols-alist
  '(("[ ]"            . ?)
    ("[-]"            . ?)
    ("[X]"            . ?)

    ;; (":PROPERTIES:"   . ?)
    ;; (":ID:"           . ?🪪)
    ;; (":END:"          . ?🔚)

    ;; ("#+ARCHIVE:"     . ?📦)
    ;; ("#+AUTHOR:"      . ?👤)
    ;; ("#+CREATOR:"     . ?💁)
    ;; ("#+DATE:"        . ?📆)
    ;; ("#+DESCRIPTION:" . ?⸙)
    ;; ("#+EMAIL:"       . ?📧)
    ;; ("#+HEADERS"      . ?☰)
    ;; ("#+OPTIONS:"     . ?⚙)
    ;; ("#+SETUPFILE:"   . ?⚒)
    ("#+TAGS:"        . ?🏷)
    ("#+TITLE:"       . ?📓)

    ("#+BEGIN_SRC"    . ?✎)
    ("#+END_SRC"      . ?□)
    ("#+BEGIN_QUOTE"  . ?«)
    ("#+END_QUOTE"    . ?»)
    ("#+RESULTS:"     . ?💻)
    )
  "A list of symbol prettifications for `org-mode'."
  :group 'suk
  :type '(alist :key-type string :value-type (choice character sexp)))


;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-template-file
       (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
           (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))

(if (file-exists-p custom-file)
    (load custom-file))

;; Load `custom-post.el'
;; Put personal configurations to override defaults here.
(add-hook 'after-init-hook
          (lambda ()
            (let ((file
                   (expand-file-name "custom-post.el" user-emacs-directory)))
              (if (file-exists-p file)
                  (load file)))))

(setq-default
 major-mode 'text-mode ; 默认使用text模式
 cursor-type 'bar      ; 设置光标样式
 tab-width 4           ; tab 的宽度为 4 空格
 indent-tabs-mode nil  ; 永久使用空格縮排，唔好用 TAB 只係用空格代替 TAB，使用 C-q TAB 來輸入 TAB 字符
 )
(tooltip-mode -1)                          ;不要显示任何 tooltips
(delete-selection-mode 1)                  ; 选中文本后输入会覆盖
(size-indication-mode 1)
(server-mode 1)
(global-hl-line-mode 1)                    ;高亮当前行
(put 'narrow-to-region 'disabled nil)      ;开启变窄区域
(auto-compression-mode 1)                  ;打开压缩文件时自动解压缩
(show-paren-mode t)                        ;显示括号匹配
;;(blink-cursor-mode -1)
(setq inhibit-startup-message t)           ; 关闭启动欢迎界面
;; (setq initial-scratch-message nil)      ; 清空 *scratch* 缓冲区信息
(setq inhibit-startup-echo-area-message t) ; 关闭启动时回显区的提示信息


(setq read-process-output-max #x10000)  ; 64kb.  Increase how much is read from processes in a single chunk (default is 4kb)
(setq vc-follow-symlinks t)
(setq font-lock-maximum-decoration t)

(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq set-mark-command-repeat-pop t) ; Repeating C-SPC after popping mark pops it again
(setq sentence-end "\\([。！？￥%×（）—]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*") ; 测定句子结束识别同埋标点，不用在 `fill` 时，再句号后插入 2 个空行。
(setq sentence-end-double-space nil)
(add-hook 'after-change-major-mode-hook (lambda ()(modify-syntax-entry ?_ "w"))) ;; 让 `_` 被视为单词嘅组成部分
(add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?- "w"))) ;; `-` 符号同样
(setq suggest-key-bindings 1)             ;当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
(setq browse-kill-ring-quit-action        ;设置退出动作
      (quote save-and-restore))           ;保存还原窗口设置
(setq max-lisp-eval-depth 40000)          ;lisp最大执行深度
(setq max-specpdl-size 10000)             ;最大容量
(setq kill-ring-max 1024)                 ;用一个很大的 kill ring. 这样防止我不小心删掉重要的东西
(setq mark-ring-max 1024)                 ;设置的mark ring容量
(setq eval-expression-print-length nil)   ;设置执行表达式的长度没有限制
(setq eval-expression-print-level nil)    ;设置执行表达式的深度没有限制
(setq read-quoted-char-radix 16)          ;设置 引用字符 的基数
(setq global-mark-ring-max 1024)          ;设置最大的全局标记容量
(setq isearch-allow-scroll t)             ;isearch搜索时是可以滚动屏幕的
(setq enable-recursive-minibuffers t)     ;minibuffer 递归调用命令
(setq history-delete-duplicates t)        ;删除minibuffer的重复历史
(setq minibuffer-message-timeout 2)       ;显示消息超时的时间
(setq auto-revert-mode 1)                 ;自动更新buffer
(setq show-paren-style 'parentheses)      ;括号匹配显示但不是烦人的跳到另一个括号。
(setq blink-matching-paren nil)           ;当插入右括号时不显示匹配的左括号
(setq message-log-max t)                  ;设置message记录全部消息, 而不用截去
(setq require-final-newline nil)          ;不自动添加换行符到末尾, 有些情况会出现错误
(setq ediff-window-setup-function
      (quote ediff-setup-windows-plain))  ;比较窗口设置在同一个frame里
(setq x-stretch-cursor t)                 ;光标在 TAB 字符上会显示为一个大方块
(setq print-escape-newlines t)            ;显示字符窗中的换行符为 \n
(setq tramp-default-method "ssh")         ;设置传送文件默认的方法
(setq void-text-area-pointer nil)         ;禁止显示鼠标指针
(setq auto-window-vscroll nil)            ;关闭自动调节行高
(setq mouse-yank-at-point nil)            ;让光标无法离开视线
(setq kill-whole-line t)                  ; C-k deletes the end of line
(setq delete-by-moving-to-trash t)        ; Deleting files go to OS's trash folder
(setq track-eol t)                        ; Keep cursor at end of lines. Require line-move-visual is nil.
(setq line-move-visual nil)
(setq save-interprogram-paste-before-kill t) ; Save clipboard contents into kill-ring before replace them
;;(setq auto-save-default nil)            ; Disable auto save
(setq echo-keystrokes 0.1)                ;加快快捷键提示的速度

;; Hanlde minified code
(if emacs/>=27p
    (add-hook 'after-init-hook #'global-so-long-mode))

;; 如果有两个重名buffer, 则再前面加上路径区别
(require 'uniquify)
(with-eval-after-load 'uniquify
  ;; (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  )
;; Misc
(if (boundp 'use-short-answers)
    (setq use-short-answers t)
  (fset 'yes-or-no-p 'y-or-n-p))

(setq make-backup-files t)
(setq version-control t)     ; 允许多次备份
(setq kept-old-versions 2)   ; 保留最早的2个备份文件
(setq kept-new-version 100)  ; 保留最近的100个备份文件
(setq delete-old-versions t) ; 自动删除旧的备份文件

;;saveplace
(setq save-place-file (expand-file-name "saveplace" suk-emacs-var-dir)) ; "~/.emacs.d/var/saveplace"
(save-place-mode 1)
;;If emacs is slow to exit after enabling saveplace, you may be
;;running afoul of save-place-forget-unreadable-files. On exit,
;;it checks that every loaded file is readable before saving its
;;buffer position - potentially very slow if you use NFS.
(setq save-place-forget-unreadable-files nil)

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(setq enable-recursive-minibuffers t ; Allow commands in minibuffers
      history-length 1000
      savehist-additional-variables '(mark-ring
                                      global-mark-ring
                                      search-ring
                                      regexp-search-ring
                                      extended-command-history)
      savehist-autosave-interval 300
      savehist-file (expand-file-name "history" suk-emacs-var-dir) ; "~/.emacs.d/var/history"
      )
(savehist-mode 1)

;; Set UTF-8 as the default coding system
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)            ;缓存文件编码
(setq default-file-name-coding-system 'utf-8)              ;文件名编码
(setq default-keyboard-coding-system 'utf-8)               ;键盘输入编码
(setq default-process-coding-system '(utf-8 . utf-8))      ;进程输出输入编码
(setq default-sendmail-coding-system 'utf-8)               ;发送邮件编码
(setq default-terminal-coding-system 'utf-8)               ;终端编码


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

;; -*- coding: utf-8; lexical-binding: t; -*-
(defun suk/wait-for-modules (callback &rest modules)
  "Wait for MODULES to be loaded and then call CALLBACK."
  (let ((all-loaded nil))
    (dolist (module modules)
      (with-eval-after-load module
        (setq all-loaded t)))
    (if all-loaded
        (funcall callback)
      (add-hook 'after-load-functions
                (lambda ()
                  (when (cl-every #'featurep modules)
                    (funcall callback)))))))

;;;###autoload
(defun run-cmd-and-replace-region (cmd)
  "Run CMD in shell on selected region or current buffer.
Then replace the region or buffer with cli output."
  (let* ((orig-point (point))
         (b (if (region-active-p) (region-beginning) (point-min)))
         (e (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region b e cmd nil t)
    (goto-char orig-point)))


;;;###autoload
(defun my-buffer-str ()
  (buffer-substring-no-properties (point-min) (point-max)))

;; 使用示例
;;(wait-for-modules
;; 'my-callback-function
;; 'module1
;; 'module2
;; 'module3)

(defmacro suk/timer (&rest body)
  "Measure the time of code BODY running."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))


;;;###autoload
(defun childframe-workable-p ()
  "Whether childframe is workable."
  (not (or noninteractive
           emacs-basic-display
           (not (display-graphic-p)))))
;;;###autoload
(defun childframe-completion-workable-p ()
  "Whether childframe completion is workable."
  (and (eq suk-completion-style 'childframe)
       (childframe-workable-p)))
;;;###autoload
(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and suk-icon
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))
;;;###autoload
(defun suk-treesit-available-p ()
  "Check whether tree-sitter is available.
Native tree-sitter is introduced since 29.1."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))
;;;###autoload
(defun too-long-file-p ()
  "Check whether the file is too long."
  (or (> (buffer-size) 100000)
      (and (fboundp 'buffer-line-statistics)
           (> (car (buffer-line-statistics)) 10000))))

;; {{ copied from http://ergoemacs.org/emacs/elisp_read_file_content.html
;;;###autoload
(defun my-get-string-from-file (file)
  "Return FILE's content."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;;###autoload
(defun my-read-lines (file)
  "Return a list of lines of FILE."
  (split-string (my-get-string-from-file file) "\n" t))
;; }}

;;;###autoload
(defun path-in-directory-p (file directory)
  "FILE is in DIRECTORY."
  (let* ((pattern (concat "^" (file-name-as-directory directory))))
    (if (string-match pattern file) file)))


;;;###autoload
(defun my-send-string-to-cli-stdin (string program)
  "Send STRING to cli PROGRAM's stdin."
  (with-temp-buffer
    (insert string)
    (call-process-region (point-min) (point-max) program)))

;;;###autoload
(defun my-write-string-to-file (string file)
  "Write STRING to FILE."
  (with-temp-buffer
    (insert string)
    (write-region (point-min) (point-max) file)))

;;;###autoload
(defun my-async-shell-command (command)
  "Execute string COMMAND asynchronously."
  (let* ((proc (start-process "Shell"
                              nil
                              shell-file-name
                              shell-command-switch command)))
    (set-process-sentinel proc `(lambda (process signal)
                                  (let* ((status (process-status process)))
                                    (when (memq status '(exit signal))
                                      (unless (string= (substring signal 0 -1) "finished")
                                        (message "Failed to run \"%s\"." ,command))))))))

(defvar my-disable-idle-timer (daemonp)
  "Function passed to `my-run-with-idle-timer' is run immediately.")
(defun my-run-with-idle-timer (seconds func)
  "After SECONDS, run function FUNC once."
  (cond
   (my-disable-idle-timer
    (funcall func))
   (t
    (run-with-idle-timer seconds nil func))))

(require 'init-key)

;;(require 'lazycat-theme)
;;(lazycat-theme-load-dark)
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  ;; 加载一个主题，DOOM One 是 DOOM Emacs 的默认主题，非常美观
  :init
  (load-theme 'doom-one t)
  )

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

;;;###autoload
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(use-package doom-modeline
  ;;  :load-path "~/.emacs.d/extensions/doom-modeline"
  :hook (after-init . doom-modeline-mode)
  :init
  ;;(doom-modeline-mode 1)
  (setq doom-modeline-icon suk-icon
        doom-modeline-minor-modes t)
  :config
  (column-number-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-window-width-limit nil)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-time t)
  (doom-modeline-vcs-max-leghth 50)
  ;; Windows下记得加上
  (if sys/win32p (setq inhibit-compacting-font-caches t))
  )

(use-package hide-mode-line
  :hook (((treemacs-mode
           eshell-mode shell-mode
           term-mode vterm-mode
           embark-collect-mode
           lsp-ui-imenu-mode
           pdf-annot-list-mode) . turn-on-hide-mode-line-mode)
         (dired-mode . (lambda()
                         (and (bound-and-true-p hide-mode-line-mode)
                              (turn-off-hide-mode-line-mode))))))

;; A minor-mode menu for mode-line
(use-package minions
  :hook (doom-modeline-mode . minions-mode))

;; 字体
  (lazy-load-set-keys
   '(
     ("C--" . text-scale-decrease)        ;减小字体大小
     ("C-=" . text-scale-increase)        ;增加字体大小
     ("C-x C-0" . text-scale-adjust)
     ))


  (defun font-installed-p (font-name)
    "Check if font with FONT-NAME is available."
    (find-font (font-spec :name font-name)))


;; Use fixed pitch where it's sensible
;;  (use-package mixed-pitch :diminish)
(require 'load-set-font)

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
    ("C-<prior>" . centaur-tabs-backward) ;; Ctrl PgUp
    ("C-<next>"  . centaur-tabs-forward))  ;; Ctrl PgDn
)

(when (display-graphic-p)
   ;; Icons
  (use-package nerd-icons
    :config
    (when (and (display-graphic-p)
               (not (font-installed-p nerd-icons-font-family)))
      (nerd-icons-install-fonts t)))

  ;; 图标支持
  (use-package all-the-icons
    ;; :ensure t
    :load-path "~/.emacs.d/extensions/all-the-icons"
    :if (display-graphic-p))
  )

(run-with-idle-timer
 9
 nil
 #'(lambda()
     ;;(require-package 'highlight-symbol)
     ;; Highlight the current line
     (use-package hl-line
       :ensure nil
       :hook ((after-init . global-hl-line-mode)
              ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
               (lambda () (setq-local global-hl-line-mode nil)))))



     (use-package rainbow-mode
       :diminish
       :hook ((prog-mode html-mode) . rainbow-mode)
       )

     ))

(require-package 'hydra)

(use-package hydra
  :hook (emacs-lisp-mode . hydra-add-imenu)
  :init
  (when (childframe-completion-workable-p)
    (setq hydra-hint-display-type 'posframe)
    (with-eval-after-load 'posframe
      (defun hydra-set-posframe-show-params ()
        "Set hydra-posframe style."
        (setq hydra-posframe-show-params
              `(:left-fringe 8
                             :right-fringe 8
                             :internal-border-width 2
                             :internal-border-color ,(face-background 'posframe-border nil t)
                             :background-color ,(face-background 'tooltip nil t)
                             :foreground-color ,(face-foreground 'tooltip nil t)
                             :lines-truncate t
                             )))
      (hydra-set-posframe-show-params)
      (add-hook 'after-load-theme-hook #'hydra-set-posframe-show-params t)))
  )

(with-eval-after-load 'hydra
    (use-package pretty-hydra
      :custom (pretty-hydra-default-title-body-format-spec " %s%s")
      :bind ("<f6>" . toggles-hydra/body)
      :hook (emacs-lisp-mode . (lambda ()
                                 (add-to-list
                                  'imenu-generic-expression
                                  '("Hydras" "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)" 2))))
      :init
      (cl-defun pretty-hydra-title (title &optional icon-type icon-name &key face height v-adjust)
        "Add an icon in the hydra title."
        (let ((face (or face `(:inherit highlight :reverse-video t)))
              (height (or height 1.2))
              (v-adjust (or v-adjust 0.0)))
          (concat
           (when (and (icons-displayable-p) icon-type icon-name)
             (let ((f (intern (format "nerd-icons-%s" icon-type))))
               (when (fboundp f)
                 (concat (apply f (list icon-name :face face :height height :v-adjust v-adjust)) " "))))
           (propertize title 'face face))))

      ;; Global toggles
      (with-no-warnings
        (pretty-hydra-define+ toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "nf-fa-toggle_on") :color amaranth :quit-key ("q" "C-g"))
          ("Basic"
           (("n" (cond ((fboundp 'display-line-numbers-mode)
                        (display-line-numbers-mode (if display-line-numbers-mode -1 1)))
                       ((fboundp 'gblobal-linum-mode)
                        (global-linum-mode (if global-linum-mode -1 1))))
             "line number"
             :toggle (or (bound-and-true-p display-line-numbers-mode)
                         (bound-and-true-p global-linum-mode)))
            ("i" global-aggressive-indent-mode "aggressive indent" :toggle t)
            ("d" global-hungry-delete-mode "hungry delete" :toggle t)
            ("e" electric-pair-mode "electric pair" :toggle t)
            ("c" flyspell-mode "spell check" :toggle t)
            ("s" prettify-symbols-mode "pretty symbol" :toggle t)
            ("l" global-page-break-lines-mode "page break lines" :toggle t)
            ("B" display-battery-mode "battery" :toggle t)
            ("T" display-time-mode "time" :toggle t)
            ("a" abbrev-mode "abrev" :toggle t)
            ("F" auto-fill-mode "auto fill" :toggle t)
            ("m" doom-modeline-mode "modern mode-line" :toggle t)
            ("t" toggle-truncate-lines "truncate lines" :toggle t)
            ("u" toggle-company-ispell "Company Ispell" :toggle t))
           "Highlight"
           (("h l" global-hl-line-mode "line" :toggle t)
            ("h p" show-paren-mode "paren" :toggle t)
            ("h s" symbol-overlay-mode "symbol" :toggle t)
            ("h r" rainbow-mode "rainbow" :toggle t)
            ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
             "whitespace" :toggle show-trailing-whitespace)
            ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
            ("h i" highlight-indent-guides-mode "indent" :toggle t)
            ("h t" global-hl-todo-mode "todo" :toggle t))
           "Program"
           (("f" flymake-mode "flymake" :toggle t)
            ("O" hs-minor-mode "hideshow" :toggle t)
            ("U" subword-mode "subword" :toggle t)
            ("w" whitespace-mode "whitespace" :toggle t)
            ("W" which-function-mode "which function" :toggle t)
            ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
            ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
            ("v" global-diff-hl-mode "gutter" :toggle t)
            ("V" diff-hql-flydiff-mode "live gutter" :toggle t)
            ("M" diff-hl-margin-mode "margin gutter" :toggle t)
            ("D" diff-hl-dired-mode "dired gutter" :toggle t))
           ))))
    )

;; @see https://github.com/abo-abo/hydra
;; color could: red, blue, amaranth, pink, teal
(with-eval-after-load 'hydra
  (with-eval-after-load 'ivy
    (use-package ivy-hydra)))

(when (display-graphic-p)
  (use-package vertico
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word))
    :hook ((after-init . vertico-mode)
           (rfn-eshadow-update-overlay . vertico-directory-tidy))
    :config
    (with-eval-after-load 'posframe
      (when (childframe-completion-workable-p)
        (use-package vertico-posframe
          :ensure t
          :hook (vertico-mode . vertico-posframe-mode)
          :init (setq vertico-posframe-parameters '((left-fringe  . 8) (right-fringe . 8)))
          )))
    )
  )

(when (display-graphic-p)
  ;; Child frame
  (when (childframe-workable-p)
    (use-package posframe
      :hook (after-load-theme . posframe-delete-all)
      :init
      (defface posframe-border `((t (:inherit region)))
        "Face used by the `posframe' border."
        :group 'posframe)
      (defvar posframe-border-width 2
        "Default posframe border width.")
      )))

;; Optimization
(setq idle-update-delay 1.0)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; GUI Environment
(when (display-graphic-p)
  ;; Don't use GTK+ tooltip
  (when (boundp 'x-gtk-use-system-tooltips)
    (setq x-gtk-use-system-tooltips nil))
  ;; scroll-bar
  (set-scroll-bar-mode 'right)
  ;; 隐藏垂直滚动条。
  ;;(modify-all-frames-parameters '((vertical-scroll-bars)))
  )

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))
;;(global-set-key (kbd "C-x C-b") 'ibuffer)

(with-eval-after-load 'ibuffer
  ;; Display icons for buffers
  (when (display-graphic-p)
    (use-package nerd-icons-ibuffer
      :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
      :init (setq nerd-icons-ibuffer-icon suk-icon)))
  )

;; Persistent the scratch buffer
(run-with-idle-timer
 10 nil
 #'(lambda()
     (use-package persistent-scratch
       :diminish
       :bind (:map persistent-scratch-mode-map
                   ([remap kill-buffer] . (lambda (&rest _)
                                            (interactive)
                                            (user-error "Scratch buffer cannot be killed")))
                   ([remap revert-buffer] . persistent-scratch-restore)
                   ([remap revert-this-buffer] . persistent-scratch-restore))
       :hook ((after-init . persistent-scratch-autosave-mode)iu
              (lisp-interaction-mode . persistent-scratch-mode))
       :init
       ;; 创建 var 文件夹
       (make-directory (expand-file-name "var" user-emacs-directory) t)

       (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
             persistent-scratch-backup-directory (expand-file-name "var/persistent-scratch" user-emacs-directory)
             persistent-scratch-save-file (expand-file-name "var/.persistent-scratch" user-emacs-directory))
       (persistent-scratch-setup-default)

       )))

;; Automatically reload files was modified by external program
(run-with-idle-timer
 1 nil
 #'(lambda()
     (require-package 'autorevert)
     (use-package autorevert
       :ensure nil
       :diminish
       :defer 2
       :hook (after-init . global-auto-revert-mode))))

(require 'auto-save)
(auto-save-enable)
(setq auto-save-silent t)
;;(setq auto-save-delete-trailing-whitespace t)

;; ### vdiff ###
(lazy-load-global-keys
 '(
   ("M-s-u" . vdiff-buffers))
 "vdiff")

;; Frame transparence
(require-package 'transwin)
(use-package transwin
  :bind (("C-M-9" . transwin-inc)
         ("C-M-8" . transwin-dec)
         ("C-M-7" . transwin-toggle))
  :init
  (when sys/linux-x-p
    (setq transwin-parameter-alpha 'alpha-background)))

;; Directional window-selection routines
(require-package 'windmove)
(use-package windmove
  :ensure nil
  :bind*
  (("M-<left>" . (lambda ()
                   (interactive)
                   (message "Moving left!")
                   (windmove-left)))
   ("M-<right>" . (lambda ()
                    (interactive)
                    (message "Moving right")
                    (windmove-right)))
   ("M-<up>" . (lambda ()
                 (interactive)
                 (message "Moving up")
                 (windmove-up)))
   ("M-<down>" . (lambda ()
                   (interactive)
                   (message "Moving down")
                   (windmove-down)))))

;; Restore old window configurations
(require-package 'winner)
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo) ;; C-c <Left>/C-c <Right>
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*"))
  )

;; Quickly switch windows
 (require-package 'ace-window)
 (use-package ace-window
   :pretty-hydra
   ((:title (pretty-hydra-title "Window Management" 'faicon "nf-fa-th")
            :foreign-keys warn :quit-key ("q" "C-g"))
    ("Actions"
     (("TAB" other-window "switch")
      ("x" ace-delete-window "delete")
      ("X" ace-delete-other-windows "delete other" :exit t)
      ("s" ace-swap-window "swap")
      ("a" ace-select-window "select" :exit t)
      ("m" toggle-frame-maximized "maximize" :exit t)
      ("u" toggle-frame-fullscreen "fullscreen" :exit t))
     "Movement"
     (("i" windmove-up "move ↑")
      ("k" windmove-down "move ↓")
      ("j" windmove-left "move ←")
      ("l" windmove-right "move →")
      ("f" follow-mode "follow"))
     "Resize"
     (("<left>" shrink-window-horizontally "shrink H")
      ("<right>" enlarge-window-horizontally "enlarge H")
      ("<up>" shrink-window "shrink V")
      ("<down>" enlarge-window "enlarge V")
      ("n" balance-windows "balance"))
     "Split"
     (("r" split-window-right "horizontally")
      ("R" split-window-horizontally-instead "horizontally instead")
      ("v" split-window-below "vertically")
      ("V" split-window-vertically-instead "vertically instead")
      ("t" toggle-window-split "toggle")
      ("o" delete-other-windows "only this")
      )
     "Zoom"
     (("+" text-scale-increase "in")
      ("=" text-scale-increase "in")
      ("-" text-scale-decrease "out")
      ("0" (text-scale-increase 0) "reset"))
     "Misc"
     (("o" set-frame-font "frame font")
      ("f" make-frame-command "new frame")
      ("d" delete-frame "delete frame")
      ("z" winner-undo "winner undo")
      ("Z" winner-redo "winner redo"))))
   :custom-face
   (aw-leading-char-face ((t (:inherit font-lock-keyword-face :foreground unspecified :bold t :height 3.0))))
   (aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
   (aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
   :bind (([remap other-window] . ace-window)
          ("C-c w" . ace-window-hydra/body))
   :hook (emacs-startup . ace-window-display-mode)
   :config
   (defun toggle-window-split ()
     (interactive)
     (if (= (count-windows) 2)
         (let* ((this-win-buffer (window-buffer))
                (next-win-buffer (window-buffer (next-window)))
                (this-win-edges (window-edges (selected-window)))
                (next-win-edges (window-edges (next-window)))
                (this-win-2nd (not (and (<= (car this-win-edges)
                                            (car next-win-edges))
                                        (<= (cadr this-win-edges)
                                            (cadr next-win-edges)))))
                (splitter
                 (if (= (car this-win-edges)
                        (car (window-edges (next-window))))
                     'split-window-horizontally
                   'split-window-vertically)))
           (delete-other-windows)
           (let ((first-win (selected-window)))
             (funcall splitter)
             (if this-win-2nd (other-window 1))
             (set-window-buffer (selected-window) this-win-buffer)
             (set-window-buffer (next-window) next-win-buffer)
             (select-window first-win)
             (if this-win-2nd (other-window 1))))
       (user-error "`toggle-window-split' only supports two windows")))

   ;; Bind hydra to dispatch list
   (add-to-list 'aw-dispatch-alist '(?w ace-window-hydra/body) t)

   ;; Select widnow via `M-1'...`M-9'
   (defun aw--select-window (number)
     "Slecet the specified window."
     (when (numberp number)
       (let ((found nil))
         (dolist (win (aw-window-list))
           (when (and (window-live-p win)
                      (eq number
                          (string-to-number
                           (window-parameter win 'ace-window-path))))
             (setq found t)
             (aw-switch-to-window win)))
         (unless found
           (message "No specified window: %d" number)))))
   (dotimes (n 9)
     (bind-key (format "M-%d" (1+ n))
               (lambda ()
                 (interactive)
                 (aw--select-window (1+ n))))))

;; Enforce rules for popups
(require-package 'popper)
(use-package popper
  :custom
  (popper-group-function #'popper-group-by-directory)
  (popper-echo-dispatch-actions t)
  :bind (:map popper-mode-map
              ("C-h z"       . popper-toggle)
              ("C-<tab>"     . popper-cycle)
              ("C-M-<tab>"   . popper-toggle-type))
  :hook (emacs-startup . popper-echo-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*$"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "^\\*eldoc.*\\*$"
          "\\*Compile-Log\\*$"
          "\\*Completions\\*$"
          "\\*Warnings\\*$"
          "\\*Async Shell Command\\*$"
          "\\*Apropos\\*$"
          "\\*Backtrace\\*$"
          "\\*Calendar\\*$"
          "\\*Fd\\*$" "\\*Find\\*$" "\\*Finder\\*$"
          "\\*Kill Ring\\*$"
          "\\*Embark \\(Collect\\|Live\\):.*\\*$"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          flymake-diagnostics-buffer-mode
          flycheck-error-list-mode flycheck-verify-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*Process List\\*$" process-menu-mode
          list-environment-mode cargo-process-mode

          "^\\*.*eshell.*\\*.*$"
          "^\\*.*shell.*\\*.*$"
          "^\\*.*terminal.*\\*.*$"
          "^\\*.*vterm[inal]*.*\\*.*$"

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\**"
          "\\*diff-hl\\**"
          "^\\*macro expansion\\**"

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-.+\\*"
          "\\*prolog\\*" inferior-python-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-run-mode rustic-cargo-test-mode
          ))

  (with-eval-after-load 'doom-modeline
    (setq popper-mode-line
          '(:eval (let ((face (if (doom-modeline--active)
                                  'doom-modeline-emphasis
                                'doom-modeline)))
                    (if (and (icons-displayable-p)
                             (bound-and-true-p doom-modeline-icon)
                             (bound-and-true-p doom-modeline-mode))
                        (format " %s "
                                (nerd-icons-octicon "nf-oct-pin" :face face))
                      (propertize " POP " 'face face))))))
  :config
  (with-no-warnings
    (defun my-popper-fit-window-height (win)
      "Determine the height of popup window WIN by fitting it to the buffer's content."
      (fit-window-to-buffer
       win
       (floor (frame-height) 3)
       (floor (frame-height) 3)))
    (setq popper-window-height #'my-popper-fit-window-height)

    (defun popper-close-window-hack (&rest _)
      "Close popper window via `C-g'."
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p))
                 popper-open-popup-alist)
        (let ((window (caar popper-open-popup-alist)))
          (when (window-live-p window)
            (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack)))

;;; ### watch other window ###
;;; --- 滚动其他窗口
(lazy-load-global-keys
 '(
   ("C-S-n" . other-window-move-up)       ;向下滚动其他窗口
   ("C-S-p" . other-window-move-down) ;向上滚动其他窗口
   ("M-n" . window-move-up)         ;向下滚动当前窗口
   ("M-p" . window-move-down)           ;向上滚动当前窗口
   )
 "win-move")
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

(when sys/linuxp
  (run-with-idle-timer
   2 nil
   #'(lambda()
       (require-package 'projectile)
       (use-package projectile
         :ensure t
         :when (< emacs-major-version 28)
         :diminish " Proj."
         :init (add-hook 'after-init-hook 'projectile-mode)
         :config
         ;;(setq projectile-completion-system 'ido)
         ;;(setq ido-enable-flex-matching t)
         (setq projectile-completion-system 'ivy)
         ;; Eanble Projectile globally
         ;;(projectile-mode 1)
         ;; Set akeybinding for projectile commands
         ;;(global-set-key (kbd "C-c p") 'projectile-commander)
         (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
         )
       ))
  )

;; 一啲方便嘅函数
(global-set-key (kbd "C-x M-a") 'align-regexp)  ;; 快捷键 C-x M-a 用于对齐正则表达式
(global-set-key (kbd "C-(") 'backward-sexp)     ;; 快捷键 C-( 用于向后跳跃到上一个 sexp
(global-set-key (kbd "C-)") 'forward-sexp)      ;; 快捷键 C-) 用于向前跳跃到下一个 sexp
(global-set-key (kbd "C-x t T") 'suk/toggle-transparency)  ;; 快捷键 C-x t T 用于切换透明度
(global-set-key (kbd "C-x t p") 'suk/toggle-toggle-proxy)  ;; 快捷键 C-x t p 用于切换代理
(global-set-key (kbd "C-x t f") 'global-flycheck-mode)     ;; 快捷键 C-x t f 用于开启全局语法检查
(global-set-key (kbd "C-x R") 'recentf-open)   ;; 快捷键 C-x R 用于打开最近文件
(global-set-key (kbd "C-<f11>") 'toggle-frame-fullscreen)  ;; 快捷键 C-<f11> 用于切换全屏模式
(global-set-key (kbd "M-S-<return>") 'toggle-frame-fullscreen)  ;; 快捷键 M-S-<return> 也用于切换全屏模式
(global-set-key (kbd "RET") 'newline-and-indent)  ;; 回车键 RET 用于创建新行并对齐
(global-set-key (kbd "S-<return>") 'comment-indent-new-line)  ;; Shift + 回车键用于取消对齐创建的新行

(use-package bind-key)
;;(bind-key "C-c x" #'some-function some-package-mode-map)
;;(bind-key "C-c y" #'another-function)

;; Toggle fullscreen <F11> also bind to fullscreen
(bind-keys ("C-<f11>" . toggle-frame-fullscreen)
           ("C-S-f" . toggle-frame-fullscreen) ; Compatible with macOS
           ("M-S-<return>" . toggle-frame-fullscreen) ; Compatible with Windos
           )

;;; ### goto-line-preview ###
(lazy-load-global-keys
 '(
   ("M-g p" . goto-line-preview))
 "goto-line-preview")

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

;;; ### sudo ###
(when sys/linuxp
  (lazy-load-global-keys
   '(("C-z C-s" . suk/sudo/body))
   "my-sudo"
   ))

;; vi like key binds
;; (require-package 'evil)
;; (require-package 'evil-escape)
;; (require-package 'evil-exchange)
;; (require-package 'evil-find-char-pinyin)
;; (require-package 'evil-mark-replace)
;; (require-package 'evil-matchit)
;; (require-package 'evil-nerd-commenter)
;; (require-package 'evil-surround)
;; (require-package 'evil-visualstar)

;;;###autoload
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
  (global-set-key (kbd "C-c C-h") 'my-hydra-describe/body))

;; expand-region
(run-with-idle-timer
 2 nil
 #'(lambda()
     ;; (use-package expand-region ; I prefer stable version
     ;;   :load-path "~/.emacs.d/extensions/expand-region"
     ;;   )
     (with-eval-after-load 'expand-region
       (when (suk-treesit-available-p)
         (defun treesit-mark-bigger-node ()
           "Use tree-sitter to mark regions."
           (let* ((root (treesit-buffer-root-node))
                  (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
                  (node-start (treesit-node-start node))
                  (node-end (treesit-node-end node)))
             ;; Node fits the region exactly. Try its parent node instead.
             (when (and (= (region-beginning) node-start) (= (region-end) node-end))
               (when-let ((node (treesit-node-parent node)))
                 (setq node-start (treesit-node-start node)
                       node-end (treesit-node-end node))))
             (set-mark node-end)
             (goto-char node-start)))
         ))))

;; Treat undo history as a tree, ^x u
(run-with-idle-timer
 2 nil
 #'(lambda()
       (if emacs/>=28p
           (progn
             ;; vundo :load-path "~/.emacs.d/extensions/vundo"
             (with-eval-after-load 'vundo
               (setq vundo-glyph-alist vundo-unicode-symbols)))
         (progn
           ;; use undo-tree
           ;; (unless emacs/>=28p
           ;;   (require-package 'undo-tree))
           (setq undo-tree-visualizer-timestamps t
                 undo-tree-visualizer-diff t
                 undo-tree-enable-undo-in-region nil
                 undo-tree-auto-save-history nil)
           ;; HACK: keep the diff window
           (with-no-warnings
             (make-variable-buffer-local 'undo-tree-visualizer-diff)
             (setq-default undo-tree-visualizer-diff t))
           (with-eval-after-load 'undo-tree
             (add-hook 'after-init-hook #'global-undo-tree-mode))
           ))
       ))

;;; ### Advice ###
;;; --- 各种emacs行为建议
;; 在特定地模式下粘贴时自动缩进
(defadvice yank (after indent-region activate)
  "To make yank content indent automatically."
  (if (member major-mode
              '(emacs-lisp-mode
                java-mode
                web-mode
                c-mode
                c++-mode
                js-mode
                latex-mode
                plain-tex-mode))
      (indent-region (region-beginning) (region-end) nil)))

(require-package 'flyspell)
(require-package 'langtool)
;; my own patched version is better an open-source grammar, spelling,
;; and style checker, directly into Emacs. LanguageTool supports
;; multiple languages, including English, Spanish, French, German, and
;; many others, making it a versatile tool for checking the quality of
;; your writing.

(run-with-idle-timer
 2 nil
 #'(lambda()
     (require-package 'paredit) ;; useful for lisp
     (require-package 'tagedit) ;; useful for html
     (require-package 'cliphist)
     (require-package 'iedit)
     (require-package 'wgrep) ;; eidt the grep / rg result then apply to the origin buffer. Cancel is supportted.
     ;;(require-package 'textile-mode)
     ;;(require-package 'vimrc-mode)
     ;;(require-package 'qrencode)
     (use-package writeroom-mode)

     ))

(global-set-key  (kbd "C-S-SPC") 'set-mark-command)

(define-prefix-command 'leader-key)
(global-set-key (kbd "M-s-SPC") 'leader-key)
(global-set-key (kbd "C-c C-j") #'yas-expand)
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
   ([S-f5] . toggle-truncate-lines)
   ("C-x M-a" . align-regexp)
   )
 )

;; C-c TAB indent-region
;; C-u C-c TAB => (un)indent-region

;;(global-set-key (kbd "C-(") 'backward-sexp)
;;(global-set-key (kbd "C-)") 'forward-sexp)
;;(global-set-key (kbd "C-x t T") 'suk/toggle-transparency)
;;(global-set-key (kbd "C-x t p") 'suk/toggle-toggle-proxy)
;;(global-set-key (kbd "C-x t f") 'global-flycheck-mode)
;;(global-set-key (kbd "C-x R") 'recentf)
;; M-x global-set-key RET 交互式的绑定你的键。
;; C-x Esc Esc 调出上一条“复杂命令”

;;Emacs 自动排版
;;很简单：C-x h C-M-\
;;其中C-x h 是全选
;;C-M-\ 是排版

;; C-x C-q set/unset readonly
;; 大小写转换： M-u, M-l, M-c

;; M-x align-regexp 可以方便的对齐一些文字

;;(global-set-key  [C-f7] 'suk/point-to-register)
;;(global-set-key  [f7] 'suk/jump-to-register)

;; has set to f7, c-f7
;;(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
;;(global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))

(lazy-load-global-keys
 '(
   ("C-<f7>"   . suk/bookmark-launcher/body)
   )
 "my-bookmark")

;; C-x r l to list bookmarks

(when  (eq system-type 'gnu/linux)
  ;;; Dash.
  (lazy-load-global-keys
   '(("y" . dash-at-point)
     )
   "dash-at-point"
   "C-x"
   )
  )

;;; ### Isearch ###
;;; ---
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

;; search tools

;;(require-package 'git-timemachine)
(require-package 'exec-path-from-shell)
(require-package 'findr) ;; a light file search tools.
(require-package 'find-by-pinyin-dired)
(require-package 'jump)

;;(require-package 'counsel) ; counsel => swiper => ivy
;;(require-package 'counsel-bbdb)
;;(require-package 'counsel-gtags)
;;(require-package 'counsel-css)
;;(require-package 'bbdb)


(require-package 'ivy)
(require-package 'find-file-in-project)
(require-package 'swiper)

(with-eval-after-load 'ivy
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (setq ivy-re-builders-alist
        '((counsel-rg . ivy--regex-plus)
          (swiper . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          (t . ivy--regex-ignore-order)))

  (when (display-graphic-p)
    (require-package 'ivy-posframe)
    (use-package ivy-posframe))
  )

(use-package swiper
  :bind
  (
   ("C-x M-s" . swiper)
   ("C-x C-f" . counsel-find-file)
   ("M-x"     . counsel-M-x)
   )
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    ;;(define-key read-expression-map (kbd "C-r") 'counsel-expression-history))
    )
  )



;; Jump to Chinese characters
(my-run-with-idle-timer
 1
 #'(lambda()
     (require-package 'pinyinlib)
     (require-package 'ace-pinyin)
     (use-package ace-pinyin
       :diminish
       :hook (after-init . ace-pinyin-global-mode))
     (require-package 'goto-chg)
     (require 'goto-chg)
     (require-package 'avy)
     (with-eval-after-load 'avy
       (setq avy-all-windows nil
             avy-all-windows-alt t
             avy-background t
             avy-style 'pre)
       (add-hook 'after-init-hook #'avy-setup-default)
       (lazy-load-global-keys
        ' (("C-:"   . avy-goto-char)
           ("C-M-;" . avy-goto-char-2)
           ("M-g l" . avy-goto-line)
           ("M-g w" . avy-goto-word-1)
           ("M-g e" . avy-goto-word-0))
        "avy")
       (require-package 'avy-zap)
       )

     (with-eval-after-load 'avy-zap
       ;; Kill text between the point and the character CHAR
       (lazy-load-global-keys
        '(("M-z" . avy-zap-to-char-dwim)
          ("M-Z" . avy-zap-up-to-char-dwim))
        "avy-zap")
       )
     (require-package 'anzu)
     (use-package anzu
       :diminish
       :bind (([remap query-replace] . anzu-query-replace)
              ([remap query-replace-regexp] . anzu-query-replace-regexp)
              :map isearch-mode-map
              ([remap isearch-query-replace] . anzu-isearch-query-replace)
              ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
       :hook (after-init . global-anzu-mode))

     ))


  ;;;###autoload
  (defun github-code-search ()
    "Search code on github for a given language."
    (interactive)
    (let ((language (completing-read
                     "Language: "
                     '("Java" "C/C++" "Emacs Javascript" "Lisp"  "Python" "Rust")))
          (code (read-string "Code: ")))
      (browse-url
       (concat "https://github.com/search?l=" language
               "&type=code&q=" code))))

  ;;;###autoload
  (defun google-search-str (str)
    (browse-url
     (concat "https://www.google.com/search?q=" str)))

  ;;;###autoload
  (defun google-search ()
    "Google search region, if active, or ask for search string."
    (interactive)
    (if (region-active-p)
        (google-search-str
         (buffer-substring-no-properties (region-beginning)
                                         (region-end)))
      (google-search-str (read-from-minibuffer "Search: "))))


  ;; Writable `grep' buffer
  (use-package wgrep
    :init
    (setq wgrep-auto-save-buffer t
          wgrep-change-readonly-file t))

  ;; Search tool
  (use-package grep
    :ensure nil
    :autoload grep-apply-setting
    :init
    (when (executable-find "rg")
      (grep-apply-setting
       'grep-command "rg --color=auto --null -nH --no-heading -e ")
      (grep-apply-setting
       'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
      (grep-apply-setting
       'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
      (grep-apply-setting
       'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")))


  ;; Fast search tool `ripgrep'
  (use-package rg
    :hook (after-init . rg-enable-default-bindings)
    :bind (:map rg-global-map
                ("c" . rg-dwim-current-dir)
                ("f" . rg-dwim-current-file)
                ("m" . rg-menu))
    :init (setq rg-group-result t
                rg-show-columns t)
    :config
    (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases))

(use-package webjump
:ensure nil
:bind ("C-c /" . webjump)
:custom
(webjump-sites '(
                 ;; Emacs.
                 ("Emacs Home Page" .
                  "www.gnu.org/software/emacs/emacs.html")
                 ("Savannah Emacs page" .
                  "savannah.gnu.org/projects/emacs")

                 ;; Internet search engines.
                 ("DuckDuckGo" .
                  [simple-query "duckduckgo.com"
                                "duckduckgo.com/?q=" ""])
                 ("Google" .
                  [simple-query "www.google.com"
                                "www.google.com/search?q=" ""])
                 ("Google Groups" .
                  [simple-query "groups.google.com"
                                "groups.google.com/groups?q=" ""])
                 ("Wikipedia" .
                  [simple-query "wikipedia.org" "wikipedia.org/wiki/" ""]))))

(lazy-load-set-keys
 '(
   ("C-z S g" . suk/google-search)
   ("C-z S c" . suk/github-code-search)
   )
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

;;(message org-files-directory)
;; 创建 var 文件夹
(make-directory (expand-file-name "var" user-emacs-directory) t)

;; 设置 org-persist 目录
(setq org-persist-directory (expand-file-name "var/org-persist" user-emacs-directory))
;; 创建新的 org-persist 目录（如果不存在）
(unless (file-exists-p org-persist-directory)
  (make-directory org-persist-directory t))
(require 'org)
;; To speed up startup, don't put to init section
(setq org-modules nil)
;;(setq org-startup-indented t)
(setq org-startup-folded nil)
(setq org-ellipsis  "... → ")
(setq org-pretty-entities t)
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars nil)
(setq org-blank-before-new-entry '((heading) (plain-list-item . auto)))
(setq org-insert-heading-respect-content t)
(setq org-yank-adjusted-subtrees t)
;; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)
;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

(setq org-tags-column -80)

(setq org-catch-invisible-edits 'smart)

;; 设置打开某种文件类型
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.mm\\'" . system)
        ("\\.x?html?\\'" . system)
        ("\\.pdf\\'" . system)))

  ;; 运行 Org Babel Tangle 命令：`M-x org-babel-tangle`。
  ;; 从 org 文件中生成 el 配置文件
  ;; 保存 user-emacs-directory(~/.emacs.d/) 文件下的 org 时，
  ;; 导出文件中 elisp 代码到文件中。
  (defun suk/org-babel-tangle-config ()
    (when (string-equal (file-name-directory (buffer-file-name))
                        (expand-file-name user-emacs-directory)) ; ~/.emacs.d
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook #'suk/org-babel-tangle-config)))


  ;;;###autoload
  (defun suk/load-theme-org()
    (interactive)
    (load-theme 'doom-solarized-light)
    )

  ;;;###autoload
  (defun suk/load-theme-default()
    (interactive)
    (load-theme 'doom-one)
    )

;; Registers allow you to jump to a file or other location quickly.
;; To jump to a register, use C-x r j followed by the letter of the register.
;; Using registers for all these file shortcuts is probably a bit of
;; a waste since I can easily define my own keymap, but since I rarely
;; go beyond register A anyway. Also, I might as well add shortcuts for refiling.
(require 'bookmark)
(defvar my-refile-map (make-sparse-keymap))
(defmacro my-defshortcut (key file)
  `(progn
     (set-register ,key (cons 'file ,file))
     (bookmark-store ,file (list (cons 'filename ,file)
                                 (cons 'position 1)
                                 (cons 'front-context-string "")) nil)
     (define-key my-refile-map
                 (char-to-string ,key)
                 (lambda (prefix)
                   (interactive "p")
                   (let ((org-refile-targets '(((,file) :maxlevel . 6)))
                         (current-prefix-arg (or current-prefix-arg '(4))))
                     (call-interactively 'org-refile))))))

(defvar my-org-last-refile-marker nil "Marker for last refile")
(defun my-org-refile-in-file (&optional prefix)
  "Refile to a target within the current file."
  (interactive)
  (let ((org-refile-targets (list (cons (list (buffer-file-name)) '(:maxlevel . 5)))))
    (call-interactively 'org-refile)
    (setq my-org-last-refile-marker (plist-get org-bookmark-names-plist :last-refile))))

(defun my-org-refile-to-previous ()
  "Refile subtree to last position from `my-org-refile-in-file'."
  (interactive)
  (save-selected-window
    (when (eq major-mode 'org-agenda-mode)
      (org-agenda-switch-to))
    (org-cut-subtree)
    (save-window-excursion
      (save-excursion
        (bookmark-jump (plist-get org-bookmark-names-plist :last-refile))
        (let ((level (org-current-level)))
          (org-end-of-subtree t t)
          (org-paste-subtree))))))


(define-key my-refile-map "," 'my-org-refile-to-previous)
(define-key my-refile-map "." 'my-org-refile-in-file)
;; (my-defshortcut ?i "~/cloud/orgzly/Inbox.org")
;; (my-defshortcut ?o "~/cloud/orgzly/organizer.org")
;; (my-defshortcut ?s "~/personal/sewing.org")
;; (my-defshortcut ?b "~/personal/business.org")
;; (my-defshortcut ?p "~/personal/google-inbox.org")
;; (my-defshortcut ?P "~/personal/google-ideas.org")
;; (my-defshortcut ?B "~/Dropbox/books")
;; (my-defshortcut ?n "~/notes")
;; (my-defshortcut ?N "~/sync/notes/QuickNote.md")
;; (my-defshortcut ?w "~/Dropbox/public/sharing/index.org")
;; (my-defshortcut ?W "~/Dropbox/public/sharing/blog.org")
;; (my-defshortcut ?j "~/personal/journal.org")
;; (my-defshortcut ?J "~/cloud/a/Journal.csv")
;; (my-defshortcut ?I "~/Dropbox/Inbox")
;; (my-defshortcut ?g "~/sachac.github.io/evil-plans/index.org")
;; (my-defshortcut ?c "~/code/dev/elisp-course.org")
;; (my-defshortcut ?C "~/personal/calendar.org")
;; (my-defshortcut ?l "~/dropbox/public/sharing/learning.org")
;; (my-defshortcut ?q "~/sync/notes/QuickNote.md")
;; (my-defshortcut ?Q "~/personal/questions.org")

(defmacro defshortcuts (name body &optional docstring &rest heads)
  (declare (indent defun) (doc-string 3))
  (cond ((stringp docstring))
        (t
         (setq heads (cons docstring heads))
         (setq docstring "")))
  (list
   'progn
   (append `(defhydra ,name (:exit t))
           (mapcar (lambda (h)
                     (list (elt h 0) (list 'find-file (elt h 1)) (elt h 2)))
                   heads))
   (cons 'progn
         (mapcar (lambda (h) (list 'my-defshortcut (string-to-char (elt h 0)) (elt h 1)))
                 heads))))

(defmacro defshortcuts+ (name body &optional docstring &rest heads)
  (declare (indent defun) (doc-string 3))
  (cond ((stringp docstring))
        (t
         (setq heads (cons docstring heads))
         (setq docstring "")))
  (list
   'progn
   (append `(defhydra+ ,name (:exit t))
           (mapcar (lambda (h)
                     (list (elt h 0) (list 'find-file (elt h 1)) (elt h 2)))
                   heads))
   (cons 'progn
         (mapcar (lambda (h) (list 'my-defshortcut (string-to-char (elt h 0)) (elt h 1)))
                 heads))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-use-fast-todo-selection t)
;; ! 的含义是记录某项更改为状态的时间。我不把这个添加到完成的状态，是因为它们已
;; 经被记录了。

;; @ 符号表示带理由的提示，所以当切换到 WAITTING 时，Org 模式会问我为什么，并将
;; 这个添加到笔记中。
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n!)"  "|" "DONE✔(d!)" "CANCELLED✘(c@/!)")
        (sequence "WAIT⚑(w@/!)" "HOLD(h@/!)" "|" "ABORT" "SOME" "PHONE" "MEETING")))
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "blue" :weight bold)
        ("DONE✔" :foreground "forest green" :weight bold)
        ("WAIT⚑" :foreground "orange" :weight bold)
        ("HOLD" :foreground "magenta" :weight bold)
        ;;("CANCELLED" :foreground "forest grey" :weight bold)
        ("ABORT" :foreground "yellow" :weight bold)
        ("SOME" :foreground "lightgreen" :weight bold)
        ("MEETING" :foreground "lightblue" :weight bold)
        ("PHONE" :foreground "pink" :weight bold) ))


(setq org-priority-faces
      '((?A . error)
        (?B . warning)
        (?C . success)))

;; The triggers break down to the following rules:
;;   Moving a task to CANCELLED adds a CANCELLED tag
;;   Moving a task to WAITTING adds a WAITTING tag
;;   Moving a task to HOLD adds WAITTING and HOLD tags
;;   Moving a task to a done state removes WAITTING and HOLD tags
;;   Moving a task to TODO removes WAITTING, CANCELLED, and HOLD tags
;;   Moving a task to NEXT removes WAITTING, CANCELLED, and HOLD tags
;;   Moving a task to DONE removes WAITTING, CANCELLED, and HOLD tags
(setq org-todo-state-tags-triggers
      '(("CANCELLED" ("CANCELLED" . t))
        ("WAIT⚑" ("WAITTING" . t))
        ("HOLD" ("WAITTING") ("HOLD" . t))
        ("DONE" ("WAITTING") ("CANCELLED") ("HOLD"))
        ("ABORT" ("WAITTING") ("CANCELLED") ("HOLD"))
        ("TODO" ("WAITTING") ("CANCELLED") ("HOLD"))
        ("NEXT" ("WAITTING") ("CANCELLED") ("HOLD"))
        ("SOME" ("WAITTING") ("CANCELLED") ("HOLD"))))
(setq org-log-done 'time)

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)
(setq org-agenda-diary-file (expand-file-name "diary.org" org-files-directory))
(setq org-agenda-block-separator ?─)
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
(setq org-agenda-current-time-string
      "⭠ now ─────────────────────────────────────────────────")
(setq org-agenda-diary-file (expand-file-name "diary.org" org-files-directory))
;; setup agenda files
;; org-mode manages the org-agenda-files variable automatically
;; using C-c [ and C-c ] to add and remove files respectively.
;; They can be files or directories.
(setq org-agenda-files
      `(,(expand-file-name "gtd.org" org-files-directory)
        ,(expand-file-name "work.org" org-files-directory)
        ,(expand-file-name "finished.org" org-files-directory)
        ,(expand-file-name "cancel.org" org-files-directory)
        ,(expand-file-name "journal.org" org-files-directory)
        ,(expand-file-name "trash.org" org-files-directory)
        ;;,(expand-file-name "folder" org-files-directory)
        ))

;; capture template
(setq org-default-notes-file (expand-file-name "notes.org" org-files-directory))
;; Capture templates for: TODO tasks, Notes,
;; appointments, phone calls, meetings, and (setq
;; org-protocol)
(setq org-capture-templates
      '(
        ("t" "Todo"
         entry (file+headline (expand-file-name "gtd.org" org-files-directory) "Tasks")
         "* TODO %?\n%U\n%a\n"
         :immediate-finishe t)
        ("n" "Note"
         entry (file (expand-file-name "notes.org" org-files-directory))
         "* %? :NOTE:\n%U\n%a\n"
         :immediate-finishe t)
        ("r" "respond"
         entry (file (expand-file-name "gtd.org" org-files-directory))
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
         :clock-in t
         :clock-resume t
         :immediate-finish t)
        ("j" "Journal"
         entry (file+datetree (expand-file-name "journal.org" org-files-directory))
         "* %?\nEntered on %U\n  %i\n  %a")
        ("w" "Review"
         entry (file (expand-file-name "gtd.org" org-files-directory))
         "* TODO Review %c\n%U\n"
         :immediate-finish t)
        ("m" "Meeting"
         entry (file (expand-file-name "gtd.org" org-files-directory))
         "* MEETING with %? :MEETING:\n%U"
         :clock-in t
         :clock-resume t)
        ("p" "Phone call"
         entry (file (expand-file-name "gtd.org" org-files-directory))
         "* PHONE %? :PHONE:\n%U"
         :clock-in t
         :clock-resume t)
        ("h" "Habit"
         entry (file (expand-file-name "gtd.org" org-files-directory))
         "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Refile settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exclude DONE state tasks from refile targets
    ;;;###autoload
(defun suk/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))))
;; Use full outline paths for refile targets
(setq org-refile-use-outline-path t)
(setq org-refile-target-verify-function 'suk/verify-refile-target)
;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPORTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
;; Inline images in HTML instead of producting links to the image
(setq org-html-inline-images t)
;; Use (setq org-manual.css from the norang website for export document stylesheets)
;; (setq org-html-head-extra "<link rel=\"stylesheet\" href=\"org-manual.css\" type=\"text/css\" />")
(setq org-html-head-include-default-style nil)
;; Do not generate internal css formatting for HTML exports
(setq org-export-htmlize-output-type (quote css))
;; Increase default number of headings to export
(setq org-export-headline-levels 6)
(setq org-export-coding-system 'utf-8)
(setq org-table-export-default-format "orgtbl-to-csv")
;; Do not generate internal css formatting for HTML exports
(setq org-export-htmlize-output-type 'css)
(setq org-export-with-timestamps nil)
;; _ 不转义，相当于#+OPTIONS: ^:{}
(setq org-export-with-sub-superscripts '{})
;; Embed inline CSS read from a file.
  ;;;###autoload
(defun null-or-unboundp (var)
  "Return t if VAR is either unbound or nil, otherwise return nil."
  (or (not (boundp var))
      (null (symbol-value var))))

    ;;;###autoload
(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           ;;(path  org-css-file)
           (homestyle (and (or (null dir) (null (file-exists-p path)))
                           (not (null-or-unboundp 'my-org-inline-css-file))))
           (final (if homestyle my-org-inline-css-file path)))
      (if (file-exists-p final)
          (progn
            (setq-local org-html-head-include-default-style nil)
            (setq-local org-html-head
                        (concat
                         "<style type=\"text/css\">\n"
                         "<!--/*--><![CDATA[/*><!--*/\n"
                         (with-temp-buffer
                           (insert-file-contents final)
                           (buffer-string))
                         "/*]]>*/-->\n"
                         "</style>\n")))))))

(add-hook 'org-export-before-processing-hook #'my-org-inline-css-hook)

;; https://github.com/marsmining/ox-twbs
;; M-x package-install [RET] ox-twbs [RET]
;; If the installation doesn’t work try refreshing the package list:
;; M-x package-refresh-contents [RET]
;; usage: org-twbs-export-to-html

(use-package ox-twbs)
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "s-\\") 'my-org-publish-buffer)))

(defun my-org-publish-buffer ()
  (interactive)
  (save-buffer)
  (org-twbs-export-to-html))
(add-hook 'org-mode-hook
          (lambda ()
            "Beautify org symbols."
            (when suk-prettify-org-symbols-alist
              (if prettify-symbols-alist
                  (push suk-prettify-org-symbols-alist prettify-symbols-alist)
                (setq prettify-symbols-alist suk-prettify-org-symbols-alist)))
            (prettify-symbols-mode 1)
            (abbrev-mode 1)
            (setq truncate-lines nil)
            (set-fill-column 70)
            (turn-on-font-lock)
            (load-org-font)
            ))

;; covert to html
(use-package htmlize :defer 2)
;;(require-package 'ob-sagemath)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;; Attachments
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-id-method (quote uuidgen))
;; Say you want to attach a file x.sql to your current task. Create
;; the file data in /tmp/x.sql and save it.
;;
;; Attach the file with C-c C-a a and enter the filename: x.sql.
;; This generates a unique ID for the task and adds the file in the
;; attachment directory.
;;
;; * Attachments                                     :ATTACH:
;;   :PROPERTIES:
;;   :Attachments: x.sql
;;   :ID:       f1d38e9a-ff70-4cc4-ab50-e8b58b2aaa7b
;;   :END:
;;
;; The attached file is saved in
;; data/f1/d38e9a-ff70-4cc4-ab50-e8b58b2aaa7b/. Where it goes
;; exactly isn't important for me 鈥?as long as it is saved and
;; retrievable easily. Org-mode copies the original file /tmp/x.sql
;; into the appropriate attachment directory.
;;
;; Tasks with attachments automatically get an ATTACH tag so you can
;; easily find tasks with attachments with a tag search.
;;
;; To open the attachment for a task use C-c C-a o. This prompts for
;; the attachment to open and TAB completion works here.

;;(setq plantuml-default-exec-mode 'server) ;default
;; ;; Sample jar configuration
;; (setq plantuml-jar-path "/path/to/your/copy/of/plantuml.jar")
;; (setq plantuml-default-exec-mode 'jar)

;; ;; Sample executable configuration
;; (setq plantuml-executable-path "/path/to/your/copy/of/plantuml.bin")
;; (setq plantuml-default-exec-mode 'executable)
(setq plantuml-default-exec-mode 'jar)
(setq org-plantuml-jar-path
      (expand-file-name "C:/green/plantuml-1.2024.3.jar"))
(setq org-plantuml-jar-args (list "-charset" "UTF-8"))
;; plantuml-java-args
;; plantuml-jar-args
(defun my-org-plantuml-execute (orig-fun &rest args)
  (let (
        (plantuml-java-args (list
                             "-Djava.awt.headless=true"
                             "-Dfile.encoding=UTT-8"
                             "-jar"
                             "--illegal-access=deny"
                             ))
        (plantuml-jar-args (list  "-charset" "UTF-8")) ;default value
        )
    (apply orig-fun args)))

(advice-add 'org-plantuml-execute :around #'my-org-plantuml-execute)
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (dot . t)
   ))
;;(setq process-environment (cons "PATH=D:/green/plantUML/bin;%PATH%" process-environment))

;; (defun my-org-export-before-processing-hook (backend)
;;   (let ((process-environment (cons "PATH=%PATH%" process-environment)))
;;     (org-babel-execute-src-block)
;;     (org-babel-execute-buffer)))

;; (add-hook 'org-export-before-processing-hook 'my-org-export-before-processing-hook)


(defun my-org-mode-refresh-images ()
  (when (derived-mode-p 'org-mode) ; 确保当前模式是 Org-mode
    (org-redisplay-inline-images))) ; 刷新内嵌图片

(add-hook 'org-babel-after-execute-hook 'my-org-mode-refresh-images)

;; org-mode
(require-package 'toc-org)

;; org => ppt
;;(require-package 'org-re-reveal)

;;;; Run commands in a popup frame
(defun prot-window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `prot-window-popup-frame'.
    Use this function via a hook."
  (when (frame-parameter nil 'prot-window-popup-frame)
    (delete-frame)))
(defmacro prot-window-define-with-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame.
    Make the new frame have the `prot-window-popup-frame' parameter."
  `(defun ,(intern (format "prot-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `prot-window-popup-frame' parameter.
    Also see `prot-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((prot-window-popup-frame . t)))))
       (select-frame frame)
       (switch-to-buffer " prot-window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))
(declare-function org-capture "org-capture" (&optional goto keys))
(defvar org-capture-after-finalize-hook)
    ;;;###autoload (autoload 'prot-window-popup-org-capture "prot-window")
(prot-window-define-with-popup-frame org-capture)
(add-hook 'org-capture-after-finalize-hook #'prot-window-delete-popup-frame)
(require-package 'tmr)
(declare-function tmr "tmr" (time &optional description acknowledgep))
(defvar tmr-timer-created-functions)
    ;;;###autoload (autoload 'prot-window-popup-tmr "prot-window")
(prot-window-define-with-popup-frame tmr)
(add-hook 'tmr-timer-created-functions #'prot-window-delete-popup-frame)

;;;; The emacsclient calls that need ot be bound to system-wide keys
;; emacsclient -e '(prot-window-popup-org-capture)'
;; emacsclient -e '(prot-window-popup-tmr)'

(with-eval-after-load 'hydra
  (defshortcuts suk/file-shortcuts ()
    ("C" "~/proj/emacs-calendar/README.org" "Emacs calendar")
    ("e" "~/sync/emacs/Sacha.org" "Config")
    ("E" "~/sync/emacs-news/index.org" "Emacs News")
    ("f" "~/proj/font/README.org" "Font")
    ("I" "~/sync/orgzly/computer-inbox.org" "Computer inbox")
    ("i" "~/sync/orgzly/Inbox.org" "Phone inbox")
    ("o" "~/sync/orgzly/organizer.org" "Main org file")
    ("s" "~/proj/stream/notes.org" "Public Emacs notes")
    ("b" "~/sync/orgzly/business.org" "Business")
    ("p" "/scp:web:/mnt/prev/home/sacha/planet/en.ini" "Planet Emacsen")
    ("P" "~/sync/orgzly/posts.org" "Posts")
    ;;("B" "/ssh:web|sudo::/etc/nginx/sites-available" "Nginx sites")
    ("w" "~/Dropbox/public/sharing/index.org" "Sharing index")
    ("W" "~/Dropbox/public/sharing/blog.org" "Blog index")
    ("1" "~/proj/static-blog/" "Static blog")
    ("r" "~/sync/orgzly/reference.org" "Reference")
    ("R" "~/personal/reviews.org" "Reviews")
    ("g" "~/proj/sachac.github.io/evil-plans/index.org" "Evil plans"))
  )
;; ("C-c f" . #'suk/file-shortcuts/body)

;;Prettify UI
(use-package org-modern
  :custom
  ;;  (org-modern-table nil)
  (prettify-symbols-alist nil)
  :config
  ;; Disable Prettify Symbols mode globally or locally as needed
  ;;(global-prettify-symbols-mode -1)
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)
         ))

;; Recentf
(setq recentf-save-file (concat suk-emacs-var-dir "/recentf"))
;;(setq recentf-save-file "~/.emacs.d/var/recentf")
(use-package recentf
  :ensure nil
  :defer 1
  :init
  (setq recentf-save-file (concat suk-emacs-var-dir "/recentf"))
  ;;(setq recentf-save-file "~/.emacs.d/var/recentf")
  ;;(add-hook 'after-init-hook #'recentf-mode)
  (setq recentf-max-saved-items 500)
  (setq recentf-max-saved-items 17)
  (recentf-mode)
  (recentf-track-opened-file)

  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude ".cache")
  (add-to-list 'recentf-exclude ".cask")
  (add-to-list 'recentf-exclude ".elfeed")
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "cache")
  (add-to-list 'recentf-exclude "persp-confs")
  (add-to-list 'recentf-exclude "recentf")
  (add-to-list 'recentf-exclude "url")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (defun suk/recentf-exclude-p (file)
    (let ((file-dir (file-truename (file-name-directory file))))
      (-any-p (lamdba (dir)
                      (string-prefix-p dir file-dir))
              (mapcar 'file-truename (list var package-user-dir)))))
  (add-to-list 'recentf-exclude #'suk/recentf-exclude-p)
  )

;;; ### Company en words ###
;;; --- 英文助手
(lazy-load-global-keys
 '(
   ("M-r" . toggle-company-english-helper) ;英文助手
   )
 "company-english-helper")

(lazy-load-global-keys
 '(
   ("<f8>" . treemacs)
  )
"init-treemacs")

;; 设置打开 NeoTree 树形列表展示
;;(require-package 'neotree)
;;(use-package neotree
;;  :commands (projectile-switch-project neotree-dir)
;;  :config
;;  (setq neo-theme 'ascii           ; NeoTree 图标的样式
;;        neo-window-width 35
;;        neo-window-fixed-size nil) ; 设置 NeoTree 窗口的宽度可以使用鼠标调整
;;  :bind ("C-c o" . projectile-switch-project))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :defer 2
  :hook ((after-init . global-aggressive-indent-mode)
         ;; NOTE: Disable in large files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (when (too-long-file-p)
                          (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(gitconfig-mode
                  asm-mode web-mode html-mode css-mode go-mode
                  scala-mode shell-mode term-mode vterm-mode
                  prolog-inferior-mode))
    (add-to-list 'aggressive-indent-excluded-modes mode))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands
               #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p
                      'c-mode 'c++-mode 'csharp-mode
                      'java-mode 'go-mode 'swift-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         )))))

(require-package 'markdown-mode)

;; Display available keybindings in popup
(require-package 'which-key)
(use-package which-key
  :diminish
  :bind (("C-h M-m" . which-key-show-major-mode)
         (:map help-map ("C-h" . which-key-C-h-dispatch)))

  :hook (after-init . which-key-mode)
  ;;:custom
  ;; 弹出方式，底部弹出
  ;;(which-key-popup-type 'side-window)
  :init (setq which-key-max-description-length 30
              which-key-lighter nil
              which-key-show-remaining-keys t)
  :config
  (which-key-mode)
  (when (childframe-completion-workable-p)
    (use-package which-key-posframe
      :diminish
      :custom-face
      (which-key-posframe ((t (:inherit tooltip))))
      (which-key-posframe-border ((t (:inherit posframe-border :background unspecified))))
      :init
      (setq which-key-posframe-border-width posframe-border-width
            which-key-posframe-parameters '((left-fringe . 8)
                                            (right-fringe . 8)))
      (which-key-posframe-mode 1))))

;; Music
;;(require-package 'emms)
;;(require-package 'pulseaudio-control)

(require-package 'pomodoro) ;; notify you to take a break for a long time work. pomodoro-start, pomodoro-pause, pomodoro-stop

(require-package 'regex-tool)

;;;; The emacsclient call depends on the daemon or `server-mode' (I use the latter)
(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))

;; efficiency
;;(require-package 'esup)
(require-package 'helpful)
(require-package 'wc-mode)
(require-package 'ws-butler)
(require-package 'async)
;;(require-package 'amx)
(require-package 'popup) ; some old package need it
(require-package 'htmlize) ; prefer stable version
(require-package 'diminish)
;;(require-package 'scratch)

(require-package 'unfill)
(when  sys/linuxp
  (require-package 'eww-lnum) ;; pluin for eww, a built-in web browser
)
(require-package 'rainbow-delimiters)

;; Tools

;;(require-package 'request) ;; a http client
;;(require-package 'websocket) ; for debug debugging of browsers
;;(require-package 'simple-httpd)
;;(require-package 'cpputils-cmake)
;;(require-package 'rust-mode)
;;(require-package 'auto-package-update)
;;(require-package 'keyfreq)

;; Test tools


;;(unless sys/win32p
;;  (use-package daemons)                 ; system services/daemons
;;  )

;;(use-package bind-key)
;;Enhance M-x, use counsel-M-x

(require-package 'session)
(require 'auto-save)
(require 'basic-toolkit)

(setq desktop-load-locked-desktop t) ; don't popup dialog ask user, load anyway
(setq desktop-restore-frames nil)    ; don't restore any frame

(defun emacs-session-restore ()
  "Restore emacs session."
  (interactive)
  (ignore-errors
    ;; Kill other windows.
    (delete-other-windows)
    ;; Kill unused buffers.
    (kill-unused-buffers)
    ;; Restore session.
    (desktop-read "~/.emacs.d/var/")
    ))

(defun emacs-session-save (&optional arg)
  "Save emacs session."
  (interactive "p")
  (ignore-errors
    (if (equal arg 4)
        ;; Kill all buffers if with prefix argument.
        (mapc 'kill-buffer (buffer-list))
      ;; Kill unused buffers.
      (kill-unused-buffers)
      ;; Save all buffers before exit.
      (auto-save-buffers))
    ;; Save session.
    (make-directory "~/.emacs.d/var/" t)
    (desktop-save "~/.emacs.d/var/")
    ;; Exit emacs.
    (kill-emacs)))

(global-set-key (kbd "S-<f9>") 'emacs-session-save)
(global-set-key (kbd "C-<f9>") 'emacs-session-save)

(emacs-session-restore)

;;据说跟 lsp-bridge 冲突
(require-package 'company)
;;(require-package 'company-native-complete)
(require-package 'company-c-headers)
(require-package 'company-statistics)
(use-package company
 :defer 2
 :diminish
 :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
 :hook (after-init . global-company-mode)
 :init (setq company-tooltip-align-annotations t
             company-idle-delay 0 company-echo-delay 0
             company-minimum-prefix-length 1
             company-require-match nil
             company-dabbrev-ignore-case nil
             company-dabbrev-downcase nil
             company-show-numbers t)
 :config
 (setq switch-window-input-style 'minibuffer)
 (setq switch-window-increase 4)
 (setq switch-window-threshold 2)
 (setq switch-window-shortcut-sytle 'querty)
 (setq switch-window-qwerty-shortcuts
       '("a" "s" "d" "f" "j" "k" "l"))
 (setq company-minimum-prefix-length 1)
 (setq company-show-quick-access t)
 :bind (:map company-active-map
             ("C-n" . #'company-select-next)
             ("C-p" . #'company-select-previous)
             ("TAB" . company-complete-selection)
             ("M-h" . company-complete-selection)
             ("M-H" . company-complete-common)
             ("M-s" . company-search-candidates)
             ("M-S" . company-filter-candidates)
             ("M-n" . company-select-next)
             ("M-p" . company-select-previous))
 (:map leader-key
       ("c s" . #'company-yasnippet
        ))
 )
(use-package company-box
 :ensure nil)

;; Optionally use the `orderless' completion style.
(require-package 'orderless)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))
;; Support Pinyin
(use-package pinyinlib
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h"   . consult-history)
         ("C-c k"   . consult-kmacro)
         ("C-c m"   . consult-man)
         ("C-c i"   . consult-info)
         ("C-c r"   . consult-ripgrep)
         ("C-c T"   . consult-theme)
         ("C-."     . consult-imenu)

         ;;("C-c c e" . consult-colors-emacs)
         ;;("C-c c w" . consult-colors-web)
         ;;("C-c c f" . describe-face)
         ;;("C-c c t" . consult-theme)

         ([remap Info-search]        . consult-info)
         ([remap isearch-forward]    . consult-line)
         ([remap recentf-open-files] . consult-recent-file)

         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store)        ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#"   . consult-register)
         ;; Other custom bindings
         ("M-y"     . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e"   . consult-compile-error)
         ("M-g g"   . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o"   . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d"   . consult-find)
         ("M-s D"   . consult-locate)
         ("M-s g"   . consult-grep)
         ("M-s G"   . consult-git-grep)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)
         ;; Isearch integration
         ("M-s e"   . consult-isearch-history)
         :map isearch-mode-map
         ("M-e"     . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s e"   . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l"   . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L"   . consult-line-multi)            ;; needed by consult-line to detect isearch

         ;; Minibuffer history
         :map minibuffer-local-map
         ("C-s" . (lambda ()
                    "Insert the selected region or current symbol at point."
                    (interactive)
                    (insert (with-current-buffer
                                (window-buffer (minibuffer-selected-window))
                              (or (and transient-mark-mode mark-active (/= (point) (mark))
                                       (buffer-substring-no-properties (point) (mark)))
                                  (thing-at-point 'symbol t)
                                  "")))))
         ("M-s" . consult-history) ;;orig. next-matching-history-element
         ("M-r" . consult-history))   ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (use-package consult-flyspell
    :bind ("M-g s" . consult-flyspell))

  (use-package consult-yasnippet
    :bind ("M-g y" . consult-yasnippet))
  ;; Use Consult to select xref locations with preview
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))
  )

(use-package nerd-icons-completion
  :when (icons-displayable-p)
  :hook (vertico-mode . nerd-icons-completion-mode))

(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Only list the commands of the current modes
  (when (boundp 'read-extended-command-predicate)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; More utils
(use-package shackle)

(unless (display-graphic-p)
  ;; only conole packages
  )

(when sys/linuxp
  (use-package vterm))

;; viwer
(when sys/linuxp
  ;; run "M-x pdf-tool-install" at debian and open pdf in GUI Emacs
  ;;(require-package 'pdf-tools) ;; use the package in extension
  (require-package 'nov) ; read epub
  )

;; 代码片段
(when sys/linuxp
  (require-package 'yasnippet)
  (require-package 'auto-yasnippet)
  (require 'yasnippet)
  (with-eval-after-load 'yasnippet
    (setq yas-snippet-dirs '("~/.emacs.d/share/snippets"))
    (yas-global-mode 1)
    (autoload 'yas-minor-mode-on "yasnippet")
    (dolist (x '(org-mode-hook prog-mode-hook snippet-mode-hook))
      (add-hook x #'yas-minor-mode-on))

    )

  (use-package yasnippet)
  (use-package yasnippet-snippets)
  )

(when sys/linuxp
 (require-package 'magit)
 (require-package 'fringe-helper)
 (require-package 'git-gutter) ; dependent to fringe-helper
 (require-package 'git-modes)
 )

(require-package 'web-mode)
;;(require-package 'lua-mode)
(require-package 'yaml-mode)
(require-package 'js2-mode)
;; (require-package 'rjsx-mode) ; use my package in extensions
(require-package 'csv-mode)
(require-package 'emmet-mode)
;;(require-package 'groovy-mode)
;; magit sometime use packages which not released yet
;; so we place it at the end to make sure other packages are installed first
;;(require-package 'graphql-mode)
(require-package 'auto-yasnippet)
(require-package 'typescript-mode)
(require-package 'nvm)
;; Misc. programming modes
(use-package csv-mode)
(use-package yaml-mode)

;;(require-package 'elpy) ;; python
;;(require-package 'request) ;; a http client
;;(require-package 'websocket) ; for debug debugging of browsers
;;(require-package 'simple-httpd)
;;(require-package 'highlight-symbol)
;;(require-package 'cpputils-cmake)
;;(require-package 'rust-mode)
;;(require-package 'cmake-mode)
;;(require-package 'sage-shell-mode)

;; format all, formatter for almost languages
;; great for programmers
(require-package 'format-all)
(use-package format-all
  :ensure t
  :hook (prog-mode . format-all-ensure-formatter)
  :bind ("C-c f" . #'format-all-buffer))

;; Format HTML, CSS and JavaScript/JSON
;; Install: npm -g install prettier
(when (executable-find "prettier")
  (use-package prettier
    :diminish
    :hook ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
    :init (setq prettier-pre-warm 'none)))

(use-package prettier-js
  :ensure t
  :defer 3
  :hook ((css-mode web-mode typescript-mode js-mode json-mode js2-mode) . prettier-js-mode))

;; 折叠和收缩代码
;; builtin
(use-package hideshow
  :diminish hs-minor-mode
  :pretty-hydra
  ((:title (pretty-hydra-title "HideShow" 'octicon "nf-oct-fold")
           :color amaranth :quit-key ("q" "C-g"))
   ("Fold"
    (("t" hs-toggle-all "toggle all")
     ("a" hs-show-all "show all")
     ("i" hs-hide-all "hide all")
     ("g" hs-toggle-hiding "toggle hiding")
     ("c" hs-cycle "cycle block")
     ("s" hs-show-block "show block")
     ("h" hs-hide-block "hide block")
     ("l" hs-hide-level "hide level"))
    "Move"
    (("C-a" mwim-beginning-of-code-or-line "⭰")
     ("C-e" mwim-end-of-code-or-line "⭲")
     ("C-b" backward-char "←")
     ("C-n" next-line "↓")
     ("C-p" previous-line "↑")
     ("C-f" forward-char "→")
     ("C-v" pager-page-down "↘")
     ("M-v" pager-page-up "↖")
     ("M-<" beginning-of-buffer "⭶")
     ("M->" end-of-buffer "⭸"))))
  :bind
  (:map hs-minor-mode-map
        ("C-~" . hideshow-hydra/body)
        ("C-S-<escape>" . hideshow-hydra/body)
        ("C-c ." . hs-toggle-hiding)
        ("C-c ," . hs-show-all)
        )
  :hook (prog-mode . hs-minor-mode)
  :config
  ;;代码折叠
  (add-hook 'c-mode-common-hook   'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'java-mode-hook       'hs-minor-mode)
  (add-hook 'ess-mode-hook        'hs-minor-mode)
  (add-hook 'perl-mode-hook       'hs-minor-mode)
  (add-hook 'sh-mode-hook         'hs-minor-mode)
  (add-hook 'python-mode-hook     'hs-minor-mode)
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (if (char-displayable-p ?⏷) "⏷" "...")
                     'face 'shadow)
                    (propertize
                     (format " (%d lines)"
                             (count-lines (overlay-start ov)
                                          (overlay-end ov)))
                     'face '(:inherit shadow :height 0.8))
                    " "))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts))

;; 设置行号
;; builtin
(require 'display-line-numbers)
;;(global-display-line-numbers-mode 1)
;; Alternatively, to use it only in programming modes:
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Line numbers are not displayed when large files are used.
(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 1000)

(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'java-mode-hook
               'asm-mode-hook
               'haskell-mode-hook
               'rcirc-mode-hook
               'erc-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'python-mode-hook
               'js-mode-hook
               'html-mode-hook
               'css-mode-hook
               'tuareg-mode-hook
               'go-mode-hook
               'coffee-mode-hook
               'qml-mode-hook
               'markdown-mode-hook
               'slime-repl-mode-hook
               'package-menu-mode-hook
               'cmake-mode-hook
               'php-mode-hook
               'web-mode-hook
               'coffee-mode-hook
               'sws-mode-hook
               'jade-mode-hook
               'vala-mode-hook
               'rust-mode-hook
               'ruby-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'llvm-mode-hook
               'conf-toml-mode-hook
               'nxml-mode-hook
               'nim-mode-hook
               'typescript-mode-hook
               'elixir-mode-hook
               'clojure-mode-hook
               'dart-mode-hook
               'zig-mode-hook

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
               'rust-ts-mode-hook
               'java-ts-mode-hook
               'kotlin-mode-hook
               'prog-mode-hook
               'yaml-mode-hook
               'conf-mode-hook
               ))
  (add-hook hook (lambda () (display-line-numbers-mode))))


;;;###autoload
(defun my-use-tags-as-imenu-function-p ()
  "Can use tags file to build imenu function"
  (my-ensure 'counsel-etags)
  (and (locate-dominating-file default-directory "TAGS")
       ;; latest universal ctags has built in parser for javascript/typescript
       (counsel-etags-universal-ctags-p "ctags")
       (memq major-mode '(typescript-mode js-mode javascript-mode))))

;; }}

;;(require-package 'lsp-mode)
;;(require-package 'lsp-ui)
;;(require-package 'dap-mode)

(use-package eglot
  :hook ((c-mode c++-mode go-mode java-mode js-mode python-mode rust-mode web-mode) . eglot-ensure)
  :bind (("C-c e f" . #'eglot-format)
         ("C-c e i" . #'eglot-code-action-organize-imports)
         ("C-c e q" . #'eglot-code-action-quickfix))
  :config
  ;; (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (defun eglot-actions-before-save()
    (add-hook 'before-save-hook (lambda ()
                                  (call-interactively #'eglot-format)
                                  (call-interactively #'eglot-code-action-organize-imports))))
  (add-to-list 'eglot-server-programs '(web-mode "vls"))
  (add-hook 'eglot--managed-mode-hook #'eglot-actions-before-save))

;;Show function arglist or variable docstring
(run-with-idle-timer
 1
 nil
 #'(lambda()
     (use-package eldoc
       :ensure nil
       :diminish
       :config
       (when (childframe-workable-p)
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
                 (alist-get 'right-fringe eldoc-box-frame-parameters) 8))))
     ))

;; Cross-referencing commands
(use-package xref
  :bind (("M-g ." . xref-find-definitions)
         ("M-g ," . xref-go-back))
  :init
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

;; Jump to definition
(use-package dumb-jump
  :pretty-hydra
  ((:title (pretty-hydra-title "Dump Jump" 'faicon "nf-fa-anchor")
           :color blue :quit-key ("q" "C-g"))
   ("Jump"
    (("j" dumb-jump-go "Go")
     ("o" dumb-jump-go-other-window "Go other window")
     ("e" dumb-jump-go-prefer-external "Go external")
     ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
    "Other"
    (("i" dumb-jump-go-prompt "Prompt")
     ("l" dumb-jump-quick-look "Quick look")
     ("b" dumb-jump-back "Back"))))
  :bind (("C-M-j" . dumb-jump-hydra/body))
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'completing-read))

;;(setq copilot-node-executable "C:\\green\\node-v20.10.0-win-x64\\node.exe")
;;(add-to-list 'load-path "C:\\green\\emacs-suk\\.emacs.d\\extensions\\copilot\\copilot.el")
;;(require 'copilot)
;;(add-hook 'prog-mode-hook 'copilot-mode)
;; To customize the behavior of copilot-mode, please check copilot-enable-predicates and copilot-disable-predicates.
;; You need to bind copilot-complete to some key and call copilot-clear-overlay inside post-command-hook.
;;(define-key copilot-completion-map
;;            (kbd "<tab>")
;;            'copilot-accept-completion)
;;(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
;; (add-to-list 'copilot-major-mode-alist '("jsonl" . "json"))
;; Login to Copilot by M-x copilot-login. You can also check the status by M-x copilot-diagnose (NotAuthorized means you don't have a valid subscription).

;; {{ typescript
(use-package typescript-mode
  :load-path "~/.emacs.d/extensions/typescript"
  :hook ((typescript-mode . (typescript-mode-hook-setup)))
  :config
  (defun typescript-mode-hook-setup ()
    "Set up `typescript-mode'."
    (when (my-use-tags-as-imenu-function-p)
      ;; use ctags to calculate imenu items
      (setq imenu-create-index-function
            'counsel-etags-imenu-default-create-index-function)))

  (defun my-typescript-beginning-of-defun-hack (orig-func &rest args)
    "Overwrite typescript beginning detection."
    (ignore orig-func)
    (ignore args)
    (when (my-use-tags-as-imenu-function-p)
      (let* ((closest (my-closest-imenu-item)))
        (when closest
          (imenu closest)))))
  (advice-add 'typescript-beginning-of-defun
              :around #'my-typescript-beginning-of-defun-hack)
  )

;; CSS
(use-package css-mode
  :init (setq css-indent-offset 2))

;; SCSS
(use-package scss-mode
  :init (setq scss-compile-at-save nil))

;; LESS
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; JSON
(unless (fboundp 'js-json-mode)
  (use-package json-mode
    :load-path "~/.emacs.d/extensions/json-mode"))

;; JavaScript
(use-package js
  :init (setq js-indent-level 2))

(with-eval-after-load 'js-mode
  ;; '$' is part of variable name like '$item'
  (modify-syntax-entry ?$ "w" js-mode-syntax-table))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-minor-mode)
         ("\\.jsx\\'" . js2-minor-jsx-mode))
  :interpreter (("node" . js2-minor-mode)
                ("node" . js2-minor-jsx-mode))
  :hook ((js2-minor-mode . (lambda()  (js2-imenu-extras-mode)
                             (js2-highlight-unused-variables-mode)

                             )))
  :config
  (defun my-validate-json-or-js-expression (&optional not-json-p)
    "Validate buffer or select region as JSON.
If NOT-JSON-P is not nil, validate as Javascript expression instead of JSON."
    (interactive "P")
    (let* ((json-exp (if (region-active-p) (my-selected-str)
                       (my-buffer-str)))
           (jsbuf-offet (if not-json-p 0 (length "var a=")))
           errs
           first-err
           (first-err-pos (if (region-active-p) (region-beginning) 0)))
      (unless not-json-p
        (setq json-exp (format "var a=%s;"  json-exp)))
      (with-temp-buffer
        (insert json-exp)
        (my-ensure 'js2-mode)
        (js2-parse)
        (setq errs (js2-errors))
        (cond
         ((not errs)
          (message "NO error found. Good job!"))
         (t
          ;; yes, first error in buffer is the last element in errs
          (setq first-err (car (last errs)))
          (setq first-err-pos (+ first-err-pos (- (cadr first-err) jsbuf-offet)))
          (message "%d error(s), first at buffer position %d: %s"
                   (length errs)
                   first-err-pos
                   (js2-get-msg (caar first-err))))))
      (if first-err (goto-char first-err-pos))))

  (defun my-print-json-path (&optional hardcoded-array-index)
    "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is replaced with it."
    (interactive "P")
    (cond
     ((memq major-mode '(js2-mode))
      (js2-print-json-path hardcoded-array-index))
     (t
      (let* ((cur-pos (point))
             (str (my-buffer-str)))
        (when (string= "json" (file-name-extension buffer-file-name))
          (setq str (format "var a=%s;" str))
          (setq cur-pos (+ cur-pos (length "var a="))))
        (my-ensure 'js2-mode)
        (with-temp-buffer
          (insert str)
          (js2-init-scanner)
          (js2-do-parse)
          (goto-char cur-pos)
          (js2-print-json-path))))))
  (defun my-print-json-path (&optional hardcoded-array-index)
    "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is replaced with it."
    (interactive "P")
    (cond
     ((memq major-mode '(js2-mode))
      (js2-print-json-path hardcoded-array-index))
     (t
      (let* ((cur-pos (point))
             (str (my-buffer-str)))
        (when (string= "json" (file-name-extension buffer-file-name))
          (setq str (format "var a=%s;" str))
          (setq cur-pos (+ cur-pos (length "var a="))))
        (my-ensure 'js2-mode)
        (with-temp-buffer
          (insert str)
          (js2-init-scanner)
          (js2-do-parse)
          (goto-char cur-pos)
          (js2-print-json-path))))))


  ;;Latest rjsx-mode does not have indentation issue
  ;;@see https://emacs.stackexchange.com/questions/33536/how-to-edit-jsx-react-files-in-emacs
  (setq-default js2-additional-externs
                '("$"
                  "$A" ; salesforce lightning component
                  "$LightningApp" ; salesforce
                  "AccessifyHTML5"
                  "Blob"
                  "FormData"
                  "KeyEvent"
                  "Raphael"
                  "React"
                  "URLSearchParams"
                  "__dirname" ; Node
                  "_content" ; Keysnail
                  "after"
                  "afterEach"
                  "angular"
                  "app"
                  "assert"
                  "assign"
                  "before"
                  "beforeEach"
                  "browser"
                  "by"
                  "clearInterval"
                  "clearTimeout"
                  "command" ; Keysnail
                  "content" ; Keysnail
                  "decodeURI"
                  "define"
                  "describe"
                  "display" ; Keysnail
                  "documentRef"
                  "element"
                  "encodeURI"
                  "expect"
                  "ext" ; Keysnail
                  "fetch"
                  "gBrowser" ; Keysnail
                  "global"
                  "goDoCommand" ; Keysnail
                  "hook" ; Keysnail
                  "inject"
                  "isDev"
                  "it"
                  "jest"
                  "jQuery"
                  "jasmine"
                  "key" ; Keysnail
                  "ko"
                  "log"
                  "mockStore"
                  "module"
                  "mountWithTheme"
                  "plugins" ; Keysnail
                  "process"
                  "require"
                  "setInterval"
                  "setTimeout"
                  "shell" ; Keysnail
                  "tileTabs" ; Firefox addon
                  "util" ; Keysnail
                  "utag") )
  )

(use-package rjsx-mode
  :load-path "~/.emacs.d/extensions/rjsx-mode"
  :mode ("\\.js\\'")
  :hook ((rjsx-mode .  (lambda()
                         (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
                         (flycheck-select-checker 'javascript-eslint))))
  ;;:config
  ;;(add-hook 'rjsx-mode-hook 'setup)

  )

;; @see https://github.com/felipeochoa/rjsx-mode/issues/33
(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil))


(require-package 'emmet-mode)
(use-package emmet-mode
  :defer 3
  :init (setq emmet-expand-jsx-className? t)
  :hook (web-mode typescript-mode js-mode js2-mode rjsx-mode css-mode scss-mode sgml-mode))


;; Major mode for editing web templates
(use-package web-mode
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-auto-close-style 2)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-imenu-regexp-list
        '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
          ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
          ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
          ;; angular imenu
          (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "="))))


;; Adds node_modules/.bin directory to `exec_path'
(use-package add-node-modules-path
  :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

(setq-default js2-use-font-lock-faces t
              js2-mode-must-byte-compile nil
              ;; {{ comment indention in modern frontend development
              javascript-indent-level 2
              js-indent-level 2
              css-indent-offset 2
              typescript-indent-level 2
              ;; }}
              js2-strict-trailing-comma-warning nil ; it's encouraged to use trailing comma in ES6
              js2-idle-timer-delay 0.5 ; NOT too big for real time syntax check
              js2-auto-indent-p nil
              js2-indent-on-enter-key nil ; annoying instead useful
              js2-skip-preprocessor-directives t
              js2-strict-inconsistent-return-warning nil ; return <=> return null
              js2-enter-indents-newline nil
              js2-bounce-indent-p t)

(with-eval-after-load 'js-mode
  ;; '$' is part of variable name like '$item'
  (modify-syntax-entry ?$ "w" js-mode-syntax-table))

(defun my-validate-json-or-js-expression (&optional not-json-p)
  "Validate buffer or select region as JSON.
If NOT-JSON-P is not nil, validate as Javascript expression instead of JSON."
  (interactive "P")
  (let* ((json-exp (if (region-active-p) (my-selected-str)
                     (my-buffer-str)))
         (jsbuf-offet (if not-json-p 0 (length "var a=")))
         errs
         first-err
         (first-err-pos (if (region-active-p) (region-beginning) 0)))
    (unless not-json-p
      (setq json-exp (format "var a=%s;"  json-exp)))
    (with-temp-buffer
      (insert json-exp)
      (my-ensure 'js2-mode)
      (js2-parse)
      (setq errs (js2-errors))
      (cond
       ((not errs)
        (message "NO error found. Good job!"))
       (t
        ;; yes, first error in buffer is the last element in errs
        (setq first-err (car (last errs)))
        (setq first-err-pos (+ first-err-pos (- (cadr first-err) jsbuf-offet)))
        (message "%d error(s), first at buffer position %d: %s"
                 (length errs)
                 first-err-pos
                 (js2-get-msg (caar first-err))))))
    (if first-err (goto-char first-err-pos))))

(defun my-print-json-path (&optional hardcoded-array-index)
  "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is replaced with it."
  (interactive "P")
  (cond
   ((memq major-mode '(js2-mode))
    (js2-print-json-path hardcoded-array-index))
   (t
    (let* ((cur-pos (point))
           (str (my-buffer-str)))
      (when (string= "json" (file-name-extension buffer-file-name))
        (setq str (format "var a=%s;" str))
        (setq cur-pos (+ cur-pos (length "var a="))))
      (my-ensure 'js2-mode)
      (with-temp-buffer
        (insert str)
        (js2-init-scanner)
        (js2-do-parse)
        (goto-char cur-pos)
        (js2-print-json-path))))))

(with-eval-after-load 'js2-mode
  ;; I hate the hotkeys to hide things
  (define-key js2-mode-map (kbd "C-c C-e") nil)
  (define-key js2-mode-map (kbd "C-c C-s") nil)
  (define-key js2-mode-map (kbd "C-c C-f") nil)
  (define-key js2-mode-map (kbd "C-c C-t") nil)
  (define-key js2-mode-map (kbd "C-c C-o") nil)
  (define-key js2-mode-map (kbd "C-c C-w") nil))
;; }}

(defun my-js2-mode-setup()
  "Set up javascript."
  ;; if use node.js we need nice output
  (js2-imenu-extras-mode)
  (setq mode-name "JS2")
  ;; counsel/ivy is more generic and powerful for refactoring
  ;; js2-mode has its own syntax linter

  ;; call js-doc commands through `counsel-M-x'!

  ;; @see https://github.com/mooz/js2-mode/issues/350
  (setq forward-sexp-function nil))

(add-hook 'js2-mode-hook 'my-js2-mode-setup)

;; @see https://github.com/felipeochoa/rjsx-mode/issues/33
(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil))


;; Latest rjsx-mode does not have indentation issue
;; @see https://emacs.stackexchange.com/questions/33536/how-to-edit-jsx-react-files-in-emacs
(setq-default js2-additional-externs
              '("$"
                "$A" ; salesforce lightning component
                "$LightningApp" ; salesforce
                "AccessifyHTML5"
                "Blob"
                "FormData"
                "KeyEvent"
                "Raphael"
                "React"
                "URLSearchParams"
                "__dirname" ; Node
                "_content" ; Keysnail
                "after"
                "afterEach"
                "angular"
                "app"
                "assert"
                "assign"
                "before"
                "beforeEach"
                "browser"
                "by"
                "clearInterval"
                "clearTimeout"
                "command" ; Keysnail
                "content" ; Keysnail
                "decodeURI"
                "define"
                "describe"
                "display" ; Keysnail
                "documentRef"
                "element"
                "encodeURI"
                "expect"
                "ext" ; Keysnail
                "fetch"
                "gBrowser" ; Keysnail
                "global"
                "goDoCommand" ; Keysnail
                "hook" ; Keysnail
                "inject"
                "isDev"
                "it"
                "jest"
                "jQuery"
                "jasmine"
                "key" ; Keysnail
                "ko"
                "log"
                "mockStore"
                "module"
                "mountWithTheme"
                "plugins" ; Keysnail
                "process"
                "require"
                "setInterval"
                "setTimeout"
                "shell" ; Keysnail
                "tileTabs" ; Firefox addon
                "util" ; Keysnail
                "utag"))
