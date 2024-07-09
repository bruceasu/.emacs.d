;; -*- lexical-binding: t -*-

(provide 'init-org)

(eval-when-compile
  (require '+const)
  (require '+custom)
  (require 'init-package)
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
                                        ;(setq process-environment (cons "PATH=D:/green/plantUML/bin;%PATH%" process-environment))

(defun my-org-export-before-processing-hook (backend)
  (;let ((process-environment (cons "PATH=%PATH%" process-environment)))
   ;; (org-babel-execute-src-block)
   (org-babel-execute-buffer)))

(add-hook 'org-export-before-processing-hook 'my-org-export-before-processing-hook)

(defun my-org-mode-refresh-images ()
  (when (derived-mode-p 'org-mode) ; 确保当前模式是 Org-mode
    (org-redisplay-inline-images))) ; 刷新内嵌图片

(add-hook 'org-babel-after-execute-hook 'my-org-mode-refresh-images)
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

;; To speed up startup, don't put to init section
(setq org-modules nil)
;;(setq org-startup-indented t)
(setq org-startup-folded t)
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
(setq org-agenda-block-separator ?─)
(setq org-agenda-time-grid
      '((daily today require-timed)
        (800 1000 1200 1400 1600 1800 2000)
        " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))
(setq org-agenda-current-time-string
      "⭠ now ─────────────────────────────────────────────────")
(setq org-tags-column -80)
(setq org-log-done 'time)
(setq org-catch-invisible-edits 'smart)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agenda
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-alphabetical-lists t)
;; Keep tasks with dates on the global todo lists
(setq org-agenda-todo-ignore-with-date nil)
;; Keep tasks with deadlines on the global todo lists
(setq org-agenda-todo-ignore-deadlines nil)
;; Keep tasks with scheduled dates on the global todo lists
(setq org-agenda-todo-ignore-scheduled nil)
;; Keep tasks with timestamps on the global todo lists
(setq org-agenda-todo-ignore-timestamp nil)
;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)
;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)
;; Remove completed items from search results
(setq org-agenda-skip-timestamp-if-done t)
(setq org-agenda-include-diary nil)
(setq org-agenda-insert-diary-extract-time t)
;; Include agenda archive files when searching for things
(setq org-agenda-text-search-extra-files (quote (agenda-archives)))
;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)
;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)
;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      '((agenda habit-down time-up user-defined-up effort-up category-keep)
        (todo category-up effort-up)
        (tags category-up effort-up)
        (search category-up)))
;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)
(setq org-agenda-diary-file (expand-file-name "diary.org" org-files-directory))

(setq org-agenda-persistent-filter t)
;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
;; Compact the block agenda view
(setq org-agenda-compact-blocks t)
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
         :clock-in t
         :clock-resume t)
        ("n" "note"
         entry (file (expand-file-name "notes.org" org-files-directory))
         "* %? :NOTE:\n%U\n%a\n"
         :clock-in t
         :clock-resume t)
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
;; 设置打开某种文件类型
(setq org-file-apps
      '((auto-mode . emacs)
        ("\\.mm\\'" . system)
        ("\\.x?html?\\'" . system)
        ("\\.pdf\\'" . system)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-use-fast-todo-selection t)
;; ! 的含义是记录某项更改为状态的时间。我不把这个添加到完成的状态，是因为它们已
;; 经被记录了。

;; @ 符号表示带理由的提示，所以当切换到 WAITTING 时，Org 模式会问我为什么，并将
;; 这个添加到笔记中。
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n!)" "HANGUP(h)"  "|" "DONE(d!)" "CANCELLED(c@/!)")
        (sequence "⚑(T)" "🏴(N)" "❓(H)" "|" "✔(D)" "✘(C)")
        (sequence "WAITTING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
(setq org-todo-keyword-faces
      '(("TODO" :foreground "red" :weight bold)
        ("NEXT" :foreground "blue" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("WAITTING" :foreground "orange" :weight bold)
        ("HOLD" :foreground "magenta" :weight bold)
        ("CANCELLED" :foreground "forest grey" :weight bold)
        ("ABORT" :foreground "forest yellow" :weight bold)
        ("HANGUP" :foreground "orange" :weight bold)
        ("❓" :foreground "orange" :weight bold)
        ("MEETING" :foreground "forest brown" :weight bold)
        ("PHONE" :foreground "forest pink" :weight bold) ))


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
        ("WAITTING" ("WAITTING" . t))
        ("HOLD" ("WAITTING") ("HOLD" . t))
        ("DONE" ("WAITTING") ("HOLD"))
        ("TODO" ("WAITTING") ("CANCELLED") ("HOLD"))
        ("NEXT" ("WAITTING") ("CANCELLED") ("HOLD"))
        ("DONE" ("WAITTING") ("CANCELLED") ("HOLD"))))

;; Filetags look like this:
;; #+FILETAGS: NORANG @office
;; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))
(setq org-tags-match-list-sublevels t)
;; allows changing todo states with S-left and S-right skipping all of the
;; normal processing when entering or leaving a todo state. This cycles
;; through the todo states but skips setting timestamps and entering notes
;; which is very convenient when all you want to do is fix up the status of an
;; entry.
(setq org-treat-S-cursor-todo-selection-as-state-change nil)
(setq org-deadline-warning-days 30)
(setq org-log-done 'time)
(setq org-log-into-drawer t)
(setq org-log-state-notes-insert-after-drawers nil)
;; Sometimes I change tasks I'm clocking quickly
;; - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

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
(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           ;;(path (concat dir "style.css"))
           (path  org-css-file)
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

(add-hook 'org-agenda-mode-hook
          (lambda() (hl-line-mode 1)))
(add-hook 'org-clock-out-hook
          (lambda()
            (save-excursion
              (beginning-of-line 0)
              (org-remove-empty-drawer-at "LOGBOOK" (point)))))


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
;;
;; The ID changes for every task header when a new ID is generated.
;;
;; It's possible to use named directories for attachments but I
;; haven't needed this functionality yet 鈥?it's there if you need
;; it.
;;
;; I store my org-mode attachments with my org files in a
;; subdirectory data. These are automatically added to my git
;; repository along with any other org-mode changes I've made.

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 快捷键设置 keys are set in init-key.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") #'org-capture)
;; ;; (global-set-key (kbd "C-c C") 'org-capture)
(global-set-key "\C-cl" #'org-store-link)
(global-set-key "\C-ca" #'org-agenda)
;;(global-set-key "\C-cb" #'org-iswitchb)

;; C-',  C-, is org-cycle-agenda-files keys
;; 新版的org-mode使用C-c C-, 替换了 <sTAB 提供的模板功能。

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

;; Export text/html MIME emails
(use-package org-mime
  :bind (:map message-mode-map
              ("C-c M-o" . org-mime-htmlize)
              :map org-mode-map
              ("C-c M-o" . org-mime-org-buffer-htmlize)))

;; covert to html
(use-package htmlize
  :defer 2)


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
;    ("B" "/ssh:web|sudo::/etc/nginx/sites-available" "Nginx sites")
    ("w" "~/Dropbox/public/sharing/index.org" "Sharing index")
    ("W" "~/Dropbox/public/sharing/blog.org" "Blog index")
    ("1" "~/proj/static-blog/" "Static blog")
    ("r" "~/sync/orgzly/reference.org" "Reference")
    ("R" "~/personal/reviews.org" "Reviews")
    ("g" "~/proj/sachac.github.io/evil-plans/index.org" "Evil plans"))
)
;; ("C-c f" . #'suk/file-shortcuts/body)
  