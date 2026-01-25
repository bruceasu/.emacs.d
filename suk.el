(setq user-full-name "Suk")
(setq user-mail-address "bruceasu@gmail.com")

(run-with-idle-timer
 2 nil
 #'(lambda()
     (when (display-graphic-p)
       (use-package pretty-hydra
         :requires hydra
         :custom (pretty-hydra-default-title-body-format-spec " %s%s")
         :hook (emacs-lisp-mode
                .
                (lambda ()
                  (add-to-list
                   'imenu-generic-expression
                   '("Hydras" "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)" 2))))
         :init
         (cl-defun pretty-hydra-title
             (title &optional icon-type icon-name &key face height v-adjust)
           "Add an icon in the hydra title."
           (let ((face (or face `(:inherit highlight :reverse-video t)))
                 (height (or height 1.2))
                 (v-adjust (or v-adjust 0.0)))
             (concat
              (when (and (icons-displayable-p) icon-type icon-name)
                (let ((f (intern (format "nerd-icons-%s" icon-type))))
                  (when (fboundp f)
                    (concat
                     (apply f (list icon-name :face face :height height :v-adjust v-adjust))
                     " "))))
              (propertize title 'face face))))))))

(run-with-idle-timer
 2 nil
 #'(lambda()
     (when (display-graphic-p)
       (use-package posframe
         :hook (after-load-theme . posframe-delete-all)
         :init
         (defface posframe-border `((t (:inherit region)))
           "Face used by the `posframe' border."
           :group 'posframe)
         (defvar posframe-border-width 2
           "Default posframe border width.")
         )

       :config
       (posframe-delete-all)
       )))

;; Toggle two most recent buffers
(run-with-idle-timer
 1 nil
 #'(lambda()
     (fset 'quick-switch-buffer [?\C-x ?b return])
     (global-set-key (kbd "C-z b") 'quick-switch-buffer)))

(run-with-idle-timer
 1 nil
 #'(lambda()
     (require 'my-bookmark)))
;; C-x r l to list bookmarks

(lazy-load-global-keys
 '(
     ("C-c w"       . ace-window-hydra/body)
     ("C-h z"       . popper-toggle)
     ("C-<tab>"     . popper-cycle)
     ("C-M-<tab>"   . popper-toggle-type)
   )
 "init-window")

;;; Rectangle
(lazy-load-global-keys
 '(
   ("r" . hydra-rectangle/body)
   )
 "init-rectangle"
 "C-z"
 )

(when (display-graphic-p)
  (use-package vertico
    :bind (:map vertico-map
		("RET"   . vertico-directory-enter)
		("DEL"   . vertico-directory-delete-char)
		("M-DEL" . vertico-directory-delete-word))
    :hook ((after-init . vertico-mode)
	   (rfn-eshadow-update-overlay . vertico-directory-tidy))
    )
  (use-package vertico-posframe
    :ensure t
    :after (posframe vertico)
    :hook (vertico-mode . vertico-posframe-mode)
    :init (setq vertico-posframe-parameters '((left-fringe  . 8) (right-fringe . 8)))
    )
  )

(when sys/is-gui
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
   ("C-," . bury-buffer)                            ;;隐藏当前buffer
   ("C-." . unbury-buffer)                          ;;反隐藏当前buffer
   ("C-<f12>" . lazycat-theme-toggle)
   )
 (current-global-map)
 "C-z"
 )

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

(when sys/is-gui
(lazy-load-global-keys
 '(
   ("<f8>" . treemacs)
   )
 "init-treemacs"))

(lazy-load-global-keys
 '(
   ("C-x M-t" . toggles-hydra/body)
   )
 "init-toggles")

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

;; 1. Outline path Jump
(setq org-goto-interface 'outline-path-completion)
;; C-c C-j → 秒跳技术点
;; 2. org-occur 过滤内容
;; C-c /
;; 3. ripgrep or grep

;; GTD
(setq org-use-fast-todo-selection t)
;; ! 的含义是记录某项更改为状态的时间。我不把这个添加到完成的状态，是因为它们已
;; 经被记录了。

;; @ 符号表示带理由的提示，所以当切换到 WAITTING 时，Org 模式会问我为什么，并将
;; 这个添加到笔记中。
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n!)"  "|" "DONE(d!)" "CANCELLED(c@/!)")
        (sequence "WAIT(w@/!)" "HOLD(h@/!)" "|" "ABORT" "SOME" "PHONE" "MEETING")))
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "blue" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAIT" :foreground "orange" :weight bold)
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

(setq org-tag-alist '((:startgroup . nil)
                      ("Levle1" . ?u)     ;; 第一象限：紧急且重要
                      ("Level2" . ?n)     ;; 第二象限：不紧急但重要
                      ("Level3" . ?i)     ;; 第三象限：紧急但不重要
                      ("Level4" . ?t)     ;; 第四象限：不紧急且不重要
                      (:endgroup . nil)))

;; 可以使用 org-tags-view 来过滤和查看不同象限的任务
;; 例如：M-x org-tags-view RET +urgent-important
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
        ("WAIT" ("WAITTING" . t))
        ("HOLD" ("WAITTING") ("HOLD" . t))
        (done ("WAITING") ("HOLD"))
        ("DONE" ("WAITTING") ("CANCELLED") ("HOLD"))
        ("ABORT" ("WAITTING") ("CANCELLED") ("HOLD"))
        ("TODO" ("WAITTING") ("CANCELLED") ("HOLD"))
        ("NEXT" ("WAITTING") ("CANCELLED") ("HOLD"))
        ("SOME" ("WAITTING") ("CANCELLED") ("HOLD"))))

(setq org-log-done 'time)

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)
(setq org-agenda-current-time-string
      "⭠ now ─────────────────────────────────────────────────")
(setq org-agenda-diary-file
      (expand-file-name "diary.org" org-files-directory))
;; setup agenda files
;; org-mode manages the org-agenda-files variable automatically
;; using C-c [ and C-c ] to add and remove files respectively.
;; They can be files or directories.
(setq org-agenda-files
      `(,(expand-file-name "index.org" org-files-directory)
        ,(expand-file-name "gtd.org" org-files-directory)
        ,(expand-file-name "work.org" org-files-directory)
        ,(expand-file-name "archive.org" org-files-directory)
        ,(expand-file-name "cancel.org" org-files-directory)
        ,(expand-file-name "journal.org" org-files-directory)
        ;;,(expand-file-name "folder" org-files-directory)
        ))

;; default is "%s_archive::", I want to archive in a sginle file.
;; use C-u C-c C-x C-a to change
(setq org-archive-location "archive.org::* Archived Tasks")

;; capture template
(setq org-default-notes-file (expand-file-name "index.org" org-files-directory))
;; Capture templates for: TODO tasks, Notes,
;; appointments, phone calls, meetings, and (setq
;; org-protocol)
(setq org-capture-templates
      `(
        ("t" "Todo"
         entry (file+headline ,(expand-file-name "gtd.org" org-files-directory) "Tasks")
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("i" "Inbox"
         entry (file ,(expand-file-name "inbox.org" org-files-directory))
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("r" "Respond"
         entry (file ,(expand-file-name "gtd.org" org-files-directory))
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
         :clock-in t :clock-resume t :immediate-finish t)
        ("j" "Journal"
         entry (file+olp+datetree ,(expand-file-name "journal.org" org-files-directory))
         "* %?\nEntered on %U\n  %i\n  %a" :clock-in t :clock-resume t)
        ("m" "Meeting"
         entry (file ,(expand-file-name "gtd.org" org-files-directory))
         "* MEETING with %? :MEETING:\n%U"
         :clock-in t :clock-resume t)
        ("p" "Phone call"
         entry (file ,(expand-file-name "gtd.org" org-files-directory))
         "* PHONE %? :PHONE:\n%U"
         :clock-in t :clock-resume t)
        ("h" "Habit"
         entry (file ,(expand-file-name "gtd.org" org-files-directory))
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
		   "Call org-refile to target: FILE"
		   (interactive "p")
		   (let ((org-refile-targets (list (cons (list ,file) '(:maxlevel . 3))))
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
(my-defshortcut ?e "~/.emacs.d/README.org")
(my-defshortcut ?g (expand-file-name "gtd.org" org-files-directory))
(my-defshortcut ?w (expand-file-name "work.org" org-files-directory))
(my-defshortcut ?f (expand-file-name "finished.org" org-files-directory))
(my-defshortcut ?c (expand-file-name "archive.org" org-files-directory))
(my-defshortcut ?t (expand-file-name "trash.org" org-files-directory))
(my-defshortcut ?n (expand-file-name "inbox.org" org-files-directory))
(my-defshortcut ?j (expand-file-name "journal.org" org-files-directory))
(my-defshortcut ?N "~/notes")
;; 额外定义的位置
(load-if-exists (expand-file-name "my-defshortcuts.el" suk-emacs-config-dir))

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
;; _ 不转义，相当于#+OPTIONS: ^:{} ~:{}
(setq org-export-with-sub-superscripts "{}") ; 处理
(setq org-use-sub-superscripts '{}) ; 显示

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
            (set-fill-column 70)
            (turn-on-font-lock)
            (load-org-font)
            ))
(defun my-execute-function-and-shell-command-async ()
  "执行自定义函数逻辑，然后异步调用外部 shell 命令，传递当前文件名的扩展名替换为 .html 的路径作为参数。"
  (interactive)
  ;; 检查当前缓冲区是否关联有文件
  (if (buffer-file-name)
      (let* ((current-file (buffer-file-name))
             ;; 使用 file-name-sans-extension 获取不带扩展名的文件名
             ;;(base-name (file-name-sans-extension current-file))
             ;; 构建新的文件名，替换扩展名为 .html
             ;;(html-file (my-cygpath-convert (concat base-name ".html")))
             ;; 定义要执行的外部命令，这里以 ls -l 为例
             ;; (command (if sys/win32p
             ;;              ;;"echo You may need to cleanup the html file."
             ;;              (format "c:/green/msys64/usr/bin/bash.exe --login /c/green/htm-cleanup.sh %s" html-file)
             ;;            (format "htm-cleanup.sh %s" (shell-quote-argument html-file))))
             ;; 定义输出缓冲区名称
             ;;(output-buffer "*Shell Command Output*")
             )
        (my-org-publish-buffer)
        ;; 执行自定义函数逻辑
        (message "正在处理文件: %s" current-file)

        ;; 异步调用外部 shell 命令
        ;;(async-shell-command command output-buffer)

        ;; 显示提示信息
        ;;(message "已启动异步命令: %s" command)
        )
    (message "当前缓冲区没有关联的文件。")))

;; 转换msys路径（为了在Windows下使用Bash）
(defun my-cygpath-convert (win-path)
  "Use cygpath to convert WIN-PATH to /cygdrive style."
  (let ((output (shell-command-to-string
                 (format "D:/green/msys64/usr/bin/cygpath -u \"%s\"" win-path))))
    (string-trim output)))

;; 绑定快捷键 C-c e 到上述函数
(global-set-key (kbd "C-S-e") #'my-execute-function-and-shell-command-async)

;;(global-set-key (kbd "C-S-e") #'my-org-publish-buffer)
(defun org-export-docx ()
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
        (template-file (concat  suk-emacs-share-dir "/template/template.docx"))
        (shell-command (format "pandoc %s -o %s --reference-doc=%s" (buffer-file-name) docx-file template-file))
        (message "Convert finish: %s" docx-file))))

;; covert to html
(use-package htmlize :defer 2)
;;(use-package ob-sagemath)

(require 'ox-html)
;; 导出：
;; C-c C-e h h        ;; 整文件
;; C-c C-e C-s h h    ;; 子树（推荐） 光标放在标题上时，执行

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

;; org-mode
(use-package toc-org)

(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define+ hydra-global-org
    (:color blue :quit-key "q" :title "Org 操作")
    ("Timer"
     (("t" org-timer-start "Start Timer")
      ("s" org-timer-stop "Stop Timer")
      ("r" org-timer-set-timer "Set Timer")    ; 需在 orgmode 下使用
      ("p" org-timer "Print Timer")
      )
     "Clock"
     (("i" (org-clock-in '(4)) "Clock-In")
      ("o" org-clock-out "Clock-Out")
      ("j" org-clock-goto "Clock Goto")
      ("d" org-clock-display "Clock Display")
      ("R" org-clock-report "Clock Report")
      ("c" org-clock-cancel "Clock Cancel")
      ("m" org-clock-show-current "Clock Modeline"))
     "Capture"

     (("C" org-capture "Capture")
      ("l" org-capture-goto-last-stored "Last Capture")

      ("q" nil "Quit"))
     )))

;; 推荐设置快捷键
(global-set-key (kbd "C-c o") 'hydra-global-org/body)

;;Prettify UI
(when sys/is-gui
  (use-package org-modern
    :custom
    ;;  (org-modern-table nil)
    (prettify-symbols-alist nil)
    :config
    ;; Disable Prettify Symbols mode globally or locally as needed
    ;;(global-prettify-symbols-mode -1)
    :hook ((org-mode . org-modern-mode)
           (org-agenda-finalize . org-modern-agenda)
           )))

(when sys/is-gui
  (use-package org-roam
    :ensure t
    :init
    (setq org-roam-v2-ack t)
    (setq org-roam-db-location "~/.emacs.d/var/org-roam.db")
    :custom
    ;; (org-roam-directory (file-truename "~/RoadNotes"))
    ;; The file-truename function is only necessary when you use
    ;; symbolic links inside org-roam-directory: Org-roam does not
    ;; resolve symbolic links.
    (make-directory (expand-file-name "daily" org-roam-directory) t)
    (org-roam-completion-everywhere t)
    (org-roam-dailies-capture-templates
     '(("d" "default" entry "* %<%I:%M %p>: %?"
        :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
    :bind (("C-c n l" . org-roam-buffer-toggle)
           ("C-c n f" . org-roam-node-find)
           ("C-c n i" . org-roam-node-insert)
           :map org-mode-map
           ("C-M-i" . completion-at-point)
           :map org-roam-dailies-map
           ("Y" . org-roam-dailies-capture-yesterday)
           ("n" . org-roam-dailies-capture-today)
           ("T" . org-roam-dailies-capture-tomorrow)
           ("v" . org-roam-dailies-capture-date)
           ("d" . org-roam-dailies-goto-today)
           ("t" . org-roam-dailies-goto-tomorrow)
           ("y" . org-roam-dailies-goto-yesterday)
           ("c" . org-roam-dailies-goto-date)
           ("b" . org-roam-dailies-goto-next-note)
           ("f" . org-roam-dailies-goto-previous-note)
           )
    :bind-keymap
    ("C-c n d" . org-roam-dailies-map)
    :config
    (require 'org-roam-dailies) ;; Ensure the keymap is available
    (org-roam-db-autosync-mode)

    )
  )

;;; --- 笔记管理和组织
(define-prefix-command 'F9-map)
(global-set-key (kbd "<f9>") 'F9-map)
(lazy-load-set-keys
 '(("a" . org-agenda)
   ("A" . org-attach)
   ("s" . show-org-agenda)
   ("c" . org-capture)
   ("i" . org-toggle-inline-images)
   ("l" . org-toggle-link-display)
   ("d" . calendar)
   ("r" . my-refile-map)
   ("h" . hydra-global-org/body)
   )
 nil
 "<f9>")
(define-prefix-command 'my-refile-map)
(global-set-key (kbd "C-c r") 'my-refile-map)
;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") #'org-capture)
;; ;; (global-set-key (kbd "C-c C") 'org-capture)
(global-set-key "\C-cl" #'org-store-link)
(global-set-key "\C-ca" #'org-agenda)
;;(global-set-key "\C-cb" #'org-iswitchb)

;; C-',  C-, is org-cycle-agenda-files keys
;; 新版的org-mode使用C-c C-, 替换了 <sTAB 提供的模板功能。
