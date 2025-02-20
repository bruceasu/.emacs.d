(provide 'init-org)

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
         "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
        ("n" "Note"
         entry (file (expand-file-name "notes.org" org-files-directory))
         "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
        ("r" "Respond"
         entry (file (expand-file-name "gtd.org" org-files-directory))
         "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
         :clock-in t :clock-resume t :immediate-finish t)
        ("j" "Journal"
         entry (file+datetree (expand-file-name "journal.org" org-files-directory))
         "* %?\nEntered on %U\n  %i\n  %a" :clock-in t :clock-resume t)
        ("w" "Review"
         entry (file (expand-file-name "gtd.org" org-files-directory))
         "* TODO Review %c\n%U\n"
         :immediate-finish t)
        ("m" "Meeting"
         entry (file (expand-file-name "gtd.org" org-files-directory))
         "* MEETING with %? :MEETING:\n%U"
         :clock-in t :clock-resume t)
        ("p" "Phone call"
         entry (file (expand-file-name "gtd.org" org-files-directory))
         "* PHONE %? :PHONE:\n%U"
         :clock-in t :clock-resume t)
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
(my-defshortcut ?c (expand-file-name "cancel.org" org-files-directory))
(my-defshortcut ?t (expand-file-name "trash.org" org-files-directory))
(my-defshortcut ?n (expand-file-name "notes.org" org-files-directory))
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


(defun my-cygpath-convert (win-path)
  "Use cygpath to convert WIN-PATH to /cygdrive style."
  (let ((output (shell-command-to-string
                 (format "D:/green/msys64/usr/bin/cygpath -u \"%s\"" win-path))))
    (string-trim output)))

;; 绑定快捷键 C-c e 到上述函数
(global-set-key (kbd "C-S-e") #'my-execute-function-and-shell-command-async)

;;(global-set-key (kbd "C-S-e") #'my-org-publish-buffer)

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

;; org-mode
(use-package toc-org)

(with-eval-after-load 'hydra
  (defhydra hydra-global-org (:color blue)
    "
^Org^
-----------------------------------------------------
_t_ Start Timer _w_ Clock In
_s_ Stop Timer  _o_ Clock out
_r_ Set Timer   _j_ CLock Goto
_p_ Print Timer _c_ Capture
_q_ Quit        _l_ Last Capture
"
    ("t" org-timer-start "Start Timer")
    ("s" org-timer-stop "Stop Timer")
    ("r" org-timer-set-timer "Set Timer") ; This one requires you be in an orgmode doc, as it sets the timer for the header
    ("p" org-timer "Print Timer")
    ("w" (org-clock-in '(4)) "Clock-In")  ; used with (org-clock-persistence-insinuate) (setq org-clock-persist t)
    ("o" org-clock-out "Clock-Out")       ; you might also want (setq org-log-note-clock-out t)
    ("j" org-clock-goto "Clock Goto")     ; global visit the clocked task
    ("c" org-capture "Capture")           ; Don't forget to define the captures you want http://orgmode.org/manual/Capture.html
    ("l" org-capture-goto-last-stored "Last Capture")
    ("q" nil "Quit")                      ; 退出
    ))

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
   ("f" . suk/file-shortcuts/body)
   ("r" .  my-refile-map)
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
