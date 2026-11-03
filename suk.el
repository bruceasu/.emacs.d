(setq user-full-name "Suk")
(setq user-mail-address "bruceasu@gmail.com")

(require 'leader-key)
;; Keep the leader buffer prefix on the built-in switch command.
(keymap-set leader-key-map "b b" #'switch-to-buffer)
(lazy-load-global-keys
 '(("h" . ace-window-hydra/body))
 "init-window"
 "M-m w")

(run-with-idle-timer
 1 nil
 #'(lambda()
     (require 'my-bookmark)))
;; C-x r l to list bookmarks

(lazy-load-global-keys
 '(("C-h z" . popper-toggle)
   ("C-<tab>" . popper-cycle)
   ("C-M-<tab>" . popper-toggle-type))
 "init-window")

;;; Rectangle
(lazy-load-global-keys
 '(
   ("r" . hydra-rectangle/body)
   )
 "init-rectangle"
 "M-m"
 )

(when (display-graphic-p)
  (use-package session)
  (require 'auto-save)
  ;; todo dont load
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
  )

;; 记录一下使用方式

;; - =f3= start macro(~kmacro-start-macro-or-insert-counter~),
;; - =f4= done macro or run marcro (~kmacro-end-or-call-macro~).
;; - =C-x (= start macro (~kmacro-start-macro~),
;;           - =C-x )= end done marco(~kmacro-end-macro~),
;; - =C-x e= ~run marco(kmacro-end-macro)~
;; - =C-x C-k r= ~apply-macro-to-region-lines~
;; - =C-x C-k C-n= ~kmacro-cycle-ring-next~                 ;下一个键盘宏
;; - =C-x C-k C-p= ~kmacro-cycle-ring-previous~         ;上一个键盘宏
;; - =C-x C-k RET=  ~kmacro-edit-macro~                          ;编辑键盘宏
;; - ~insert-kbd-macro~
;; - =C-x C-k n=  ~name-last-kbd-macro~ is an alias for ~kmacro-name-last-macro~
;; - =C-x C-k C-d=  ~kmacro-delete-ring-head~

;; 先定义一个宏
;; 然后 ~name-last-kbd-macro~
;; 然后 ~insert-kbd-macro~
;; 等到如下类似的配置
;; : (fset 'delete-empty-lines (kbd "M-x flush-lines RET ^\s-*$ RET"))

;; ;; remove a keybinding for emacs 29 or after
;; (keymap-global-set "C-t" nil)

;; ;; or use
;; (keymap-global-unset "C-t")

;; ;; before emacs 28 or before
;; (global-set-key (kbd "C-t") nil)

;; ;; or
;; (global-unset-key (kbd "C-t"))

;; ;; 自己定义 keymap 其实很简单的，使用 defvar-keymap ，可以从上面的配置中
;; ;; 抽取 C-c n 作为一个 prefix key ，定义如下：
;; (defvar-keymap hsk/notetaking-map
;;   :doc "My notetaking keymap"
;;   "d" #'denote
;;   "t" #'denote-keywords-add
;;   "T" #'denote-keywords-remove)
;; (global-set-key (kbd "C-c n") hsk/notetaking-map)

;; ;; 来将上面定义的 keymap 绑定到 C-c n 上。不过从 emacs-29 开始，更推荐用
;; ;; keymap-set ，上面的代码可以改成：
;; (keymap-set global-map "C-c n" hsk/notetaking-map)
;; (keymap-global-set "C-c n" hsk/notetaking-map)

;; ;; 或者使用旧方法
;; ;; (define-prefix-command hsk/notetaking-map)
;; (defvar hsk/notetaking-map (make-sparse-keymap)
;;   "Keymap for leader bindings.")

;; (global-set-key (kbd "C-c n") hsk/notetaking-map)
;; (define-key hsk/notetaking-map (kbd "d") #'denote)
;; (define-key hsk/notetaking-map (kbd "t") #'isearch-repeat-forward)


;; ;; 新方法省去了 (kbd ...) 这一层，比较方便。

;; ;; 比起原先每个快捷键单独定义，使用 keymap 好处是便于管理，比如之后我想更
;; ;; 换快捷键前缀，从 C-c n 换成 C-c C-n ，只要改一处 keymap-set 就可以了。

;; ;; 另一个优点是可以嵌套定义，比如我想要将 org-journal 相关的操作也挂到
;; ;; hsk/notetaking-map 下面，可以先定义一个 hsk/org-journal-map ：

;; (defvar-keymap hsk/org-journal-map
;;   :doc "My org-journal keymap"
;;   "n" #'org-journal-new-entry
;;   "s" #'org-journal-search
;;   "o" #'org-journal-open-current-journal-file)

;; ;; 然后将它嵌套进另一个 map 下面：
;; (defvar-keymap hsk/notetaking-map
;;   :doc "My notetaking keymap"
;;   ;;...
;;   "j" hsk/org-journal-map)

;; ;; 这样 org-journal-new-entry 就映射到 C-c n j n 上了，非常优雅。


;; (let ((my-keymap (make-sparse-keymap))) ; 创建一个空的 keymap
;;   (keymap-set my-keymap "C-c f" 'find-file)) ; 设置 C-c f 来执行 'find-file 命令

;; (keymap-local-set "C-p" ctl-x-map)

;; ;; 定义leader key 方式 （旧）
;; (define-prefix-command 'my-leader) 
;; (define-key keymap "keystrok" 'command-name)
;; (global-set-key "keystroke" 'command-name)
;; (local-set-key  "keystroke" 'command-name) 

;; ;; 方式一：
;; (define-prefix-command 'SPC-map)
;; (global-set-key (kbd "SPC") 'SPC-map)
;; (global-set-key (kbd "SPC f") 'find-file)

;; ;; 方式二：
;; (define-prefix-command 'SPC-map)
;; (global-set-key (kbd "SPC") #'SPC-map)
;; (define-key SPC-map (kbd "f") #'find-file)

;; ;; 演示了如何定义一个新的按键前缀. 这里定义了M-c作为按键前缀.
;; (define-prefix-command 'comm-map)
;; (global-set-key (kbd "M-c") 'comm-map)
;; (global-set-key [(meta c)]  'comm-map)

;; ;; 新方式更加直观明白
;; (defvar leader-key-map (make-sparse-keymap) "Keymap for leader key bindings.")
;; (global-set-key (kbd "M-m") leader-key-map)
;; (define-key leader-key-map (kbd "f") #'find-file)

;; 定义一个Leader-key mode. 使用 M-m 开启，原来的功能重新绑定到 M-i
;; (global-set-key (kbd "M-i") #'back-to-indentation)
(require 'leader-key)
(lazy-load-global-keys
 '(("h" . my-hydra-describe/body))
 "init-description"
 "M-m")

(run-with-idle-timer
 1 nil
 #'(lambda()
     (use-package bind-key)
     (when sys/is-gui
       ;;(bind-key "C-c x" #'some-function some-package-mode-map)
       ;;(bind-key "C-c y" #'another-function)

       ;; (bind-keys
       ;;  ("C-x C-c" . save-buffers-kill-terminal)
       ;;  ("C-x C-f" . find-file)
       ;;  ("C-x C-s" . save-buffer))

       ;; (bind-keys :map python-mode-map
       ;;            ("C-c C-c" . python-shell-send-buffer)
       ;;            ("C-c C-r" . python-shell-send-region))


       ;; Toggle fullscreen <F11> also bind to fullscreen
       (bind-keys
        ("C-<f11>" . toggle-frame-fullscreen)
        ("C-S-f" . toggle-frame-fullscreen) ; Compatible with macOS
        ("M-S-<return>" . toggle-frame-fullscreen) ; Compatible with Windos
        ))))

(global-set-key (kbd "C-x R") 'recentf-open)    ;; 快捷键 C-x R 用于打开最近文件

(if (display-graphic-p)
    (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

(when emacs/>=29p
  ;; (keymap-global-set <key> <cmmd>)
  (keymap-set global-map "C-<f11>" #'toggle-frame-fullscreen)       ;; 快捷键 C-<f11> 用于切换全屏模式
  (keymap-set global-map "M-s-<return>" #'toggle-frame-fullscreen)  ;; 快捷键 M-s-<return> 也用于切换全屏模式
  )
(unless emacs/>=29p
  (global-set-key (kbd "C-<f11>") 'toggle-frame-fullscreen)      ;; 快捷键 C-<f11> 用于切换全屏模式
  (global-set-key (kbd "M-s-<return>") 'toggle-frame-fullscreen)
  )

(global-set-key  (kbd "C-S-SPC") 'set-mark-command)

;;; ### Toolkit ###
(lazy-load-set-keys
 '(
   ("b ," . bury-buffer)
   ("b ." . unbury-buffer)
   )
 leader-key-map)

;; 一啲方便嘅函数
;; 用于向后跳跃到上一个 sexp C-M-<left> / ESC C-<left>
;; 用于向前跳跃到下一个 sexp C-M-<right> / ESC C-<right>
;; eval-expression 执行表达式 M-: M-ESC :
;; sort-lines, calendar, align-regexp
;; C-t transpose-char M-t tanspose-word
;; toggle-truncate-lines C-x x t
;; revert-buffer-quick C-x x g
;; C-c TAB indent-region
;; C-u C-c TAB => (un)indent-region

;; M-x global-set-key RET 交互式的绑定你的键。
;; C-x Esc Esc 调出上一条“复杂命令”

;;Emacs 自动排版
;;很简单：C-x h C-M-\
;;其中C-x h 是全选
;;C-M-\ 是排版

;; C-x C-q set/unset readonly
;; 大小写转换： M-u, M-l, M-c

;; M-x align-regexp 可以方便的对齐一些文字

;;; Translation commands; local Sdcv bindings are Linux-only.
(lazy-load-global-keys
 '(("y" . my-youdao-dictionary-search-at-point)
   ("Y" . youdao-dictionary-search-at-point)
   ("g" . google-translate-at-point)
   ("G" . google-translate-query-translate)
   ("s" . google-translate-smooth-translate)
   ("f" . fanyi-dwim)
   ("d" . fanyi-dwim2)
   ("h" . fanyi-from-history))
 "init-translate"
 "M-m x")
(when (and (eq system-type 'gnu/linux) (locate-library (symbol-name 'sdcv)))
  (lazy-load-global-keys
   '(("p" . sdcv-search-pointer)
     ("P" . sdcv-search-pointer+)
     ("i" . sdcv-search-input)
     (";" . sdcv-search-input+))
   "init-translate"
   "M-m x"))

;; ;; 设置打开 NeoTree 树形列表展示
;; (when sys/is-gui
;;   (use-package neotree
;;     :commands (projectile-switch-project neotree-dir)
;;     :config
;;     (setq neo-theme 'ascii           ; NeoTree 图标的样式
;;           neo-window-width 35
;;           neo-window-fixed-size nil) ; 设置 NeoTree 窗口的宽度可以使用鼠标调整
;;     :bind ("C-c o" . projectile-switch-project)))

(run-with-idle-timer
 2 nil
 #'(lambda()
     (use-package paredit) ;; useful for lisp
     (use-package wgrep) ;; edit the grep / rg result then apply to the origin buffer. Cancel is supportted.
     (use-package writeroom-mode)
     (use-package yaml-mode)
     (use-package markdown-mode)
     ))


(unless (display-graphic-p)
  ;; only conole packages
  )

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
;;(setq org-ellipsis  "... → ")
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
;; 图 & 附件管理
;; • 图只放 diagrams/
;; • Org 只引用
;; • 不嵌入
(setq org-image-actual-width '(600))

;; 设置打开某种文件类型
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.mm\\'" . system)
        ("\\.x?html?\\'" . system)
        ("\\.pdf\\'" . system)))

;; 运行 Org Babel Tangle 命令：`M-x org-babel-tangle`。
;; 从 org 文件中生成 el 配置文件
;; README.org is canonical for tangled files; Org workflow modules are
;; maintained directly under etc/.
(defun suk/org-babel-tangle-config ()
  (interactive)
  (when (and buffer-file-name
             (file-equal-p buffer-file-name
                           (expand-file-name "README.org" user-emacs-directory)))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (file-equal-p buffer-file-name
                                     (expand-file-name "README.org" user-emacs-directory)))
              (add-hook 'after-save-hook #'suk/org-babel-tangle-config nil t))))

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

(require 'init-org)

(defvar suk/html-to-pdf-command "html2pdf"
  "HTML to PDF command")
(defun suk/export-whitepaper (sections output-pdf)
  "导出指定技术章节为 PDF"
  (let* ((org-file "tmp-whitepaper.org")
         (html-file "tmp-whitepaper.html")
         (pdf-file output-pdf))

    ;; 1️. 生成 Org 汇编文件
    (with-temp-file org-file
      (insert "#+TITLE: Internal Whitepaper\n")
      (insert "#+OPTIONS: toc:2 num:3\n")
      (insert "#+HTML_HEAD: <link rel=\"whitepaper.css\">\n\n")
      (insert "#+INCLUDE: \"cover.org\"\n")
      (insert "#+INCLUDE: \"preface.org\"\n")
      (insert "#+INCLUDE: \"disclaimer.org\"\n")
      (insert "#+INCLUDE: \"toc.org\"\n")
      (insert "#+INCLUDE: \"changelog.org\"\n")
      (dolist (id sections)
        (insert (format "#+INCLUDE: \"../tech.org\" :custom-id \"%s\"\n" id))))

    ;; 2️. 打开 Org 文件并导出 HTML
    (with-current-buffer (find-file-noselect org-file)
      (org-mode)
      (org-html-export-to-html))

    ;; 3️. 调用外部工具生成 PDF
    ;; 这里假设你有一个 html → pdf 的命令
    ;; 比如：html2pdf tmp-whitepaper.html runtime.pdf
    (let ((cmd (format "suk/html-to-pdf-command %s %s" html-file pdf-file)))
      (message "Running: %s" cmd)
      (shell-command cmd))

    (message "Whitepaper generated: %s" pdf-file)
    (delete-file org-file)
    (delete-file html-file)
  ))

;;调用：
;;* Java
;;:PROPERTIES:
;;:CUSTOM_ID: java
;;:END:
;;* Go
;;:PROPERTIES:
;;:CUSTOM_ID: go
;;:END:


;;  假设要导出的标题ID为： java, go 
;;     (suk/export-whitepaper '("java" "go") "runtime.pdf")
