(provide 'init-org-task)

(defvar org-files-directory
  (expand-file-name "org"
                    (if (boundp 'user-home-dir)
                        user-home-dir
                      user-emacs-directory))
  "Directory that stores Org files.")

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
