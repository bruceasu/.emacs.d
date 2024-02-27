;; init-org.el --- Initialize org configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Suk

;; Author: Suk

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Org configurations.
;;

;;; Code:
(eval-when-compile
  (require '+const)
  (require '+custom)
  (require 'init-package)
  )

(require 'org)
(setq org-startup-indented t)
(setq org-startup-folded t)
(setq org-ellipsis " → ")
(setq org-pretty-entities t)
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars nil)
(setq org-startup-indented t)
(setq org-blank-before-new-entry '((heading) (plain-list-item . auto)))
(setq org-insert-heading-respect-content nil)
(setq org-yank-adjusted-subtrees t)
;; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)
;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)


;; (setq org-list-demote-modify-bullet (quote (("+" . "-")
;;                                             ("*" . "-")
;;                                             ("1." . "-")
;;                                             ("1)" . "-")
;;                                             ("A)" . "-")
;;                                             ("B)" . "-")
;;                                             ("a)" . "-")
;;                                             ("b)" . "-")
;;                                             ("A." . "-")
;;                                             ("B." . "-")
;;                                             ("a." . "-")
;;                                             ("b." . "-"))))



(add-to-list 'auto-mode-alist
             '("\\.\\(org\\|org_archive\\)$" . org-mode))

(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

(add-hook 'org-mode-hook
          '(lambda ()
             (abbrev-mode 1)
             (setq truncate-lines nil)
             (set-fill-column 70)
             ;; (turn-on-font-lock)
             (load-org-font)
             )
          'append)

(add-hook 'org-indent-mode-hook
          '(lambda () (diminish #'org-indent-mode))
          'append)



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
(setq org-agenda-diary-file
      (expand-file-name "diary.org" org-files-directory))
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
		;;,(expand-file-name "gw" org-files-directory)
		))

(setq org-agenda-persistent-filter t)
;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)
;; Compact the block agenda view
(setq org-agenda-compact-blocks t)
;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)
;;;###autoload
(defun emacs-startup-screen ()
  "Display the weekly org-agenda and all todos."
  (org-agenda nil "n"))
(add-hook 'emacs-startup-hook #'emacs-startup-screen)

;; Erase all reminders and rebuilt reminders for today from the agenda
;;;###autoload
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; This is at the end of my .emacs
;; - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

;; Activate appointments so we get notifications
(appt-activate t)

;; If we leave Emacs running overnight
;; - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)

;; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-agenda-finalize-hook 'bh/org-agenda-to-appt 'append)


;; Entering C-c C-x i RET inserts a clock table report with your
;; estimated values and any clocked time to date.

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))


(setq bh/keep-clock-running nil)

;;;###autoload
(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "NEXT")
     ((and (member (org-get-todo-state) (list "NEXT"))
           (bh/is-project-p))
      "TODO"))))
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state #'bh/clock-in-to-next)

;;;###autoload
(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ;; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

;;;###autoload
(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))
;;;###autoload
(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))
;;;###autoload
(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))



;;;###autoload
(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))
;;;###autoload
(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)


(require 'org-id)
;;;###autoload
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))


;;;###autoload
(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")
;;;###autoload
(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

;; I have the following setup to remove these empty LOGBOOK drawers if they occur.
;; Remove empty LOGBOOK drawers on clock out
;;;###autoload
(defun suk/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
	(beginning-of-line 0)
	(org-remove-empty-drawer-at "LOGBOOK" (point))))

(add-hook 'org-clock-out-hook
          'suk/remove-empty-drawer-on-clock-out
          'append)
;;;###autoload
(defun show-org-agenda ()
  (interactive)
  (switch-to-buffer "*Org Agenda*")
  (delete-other-windows))

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

;; Phone capture template handling with BBDB lookup
;; Adapted from code by Gregory J. Grubbs
  ;;;###autoload
(defun suk/phone-call ()
  "Return name and company info for caller from bbdb lookup"
  (interactive)
  (let* (name rec caller)
    (setq name (completing-read "Who is calling? "
                                (bbdb-hashtable)
                                'bbdb-completion-predicate
                                'confirm))
    (when (> (length name) 0)
      ;; Something was supplied - look it up in bbdb
      (setq rec
            (or (first
                 (or (bbdb-search (bbdb-records) name nil nil)
                     (bbdb-search (bbdb-records) nil name nil)))
                name)))

    ;; Build the bbdb link if we have a bbdb record, otherwise just return the name
    (setq caller (cond ((and rec (vectorp rec))
                        (let ((name (bbdb-record-name rec))
                              (company (bbdb-record-company rec)))
                          (concat "[[bbdb:"
                                  name "]["
                                  name "]]"
                                  (when company
                                    (concat " - " company)))))
                       (rec)
                       (t "NameOfCaller")))
    (insert caller)))

;; 设置打开某种文件类型
(setq org-file-apps  '((auto-mode . emacs)
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
(setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
                          (sequence "WAITTING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))

(setq org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
                               ("NEXT" :foreground "blue" :weight bold)
                               ("DONE" :foreground "forest green" :weight bold)
                               ("WAITTING" :foreground "orange" :weight bold)
                               ("HOLD" :foreground "magenta" :weight bold)
                               ("CANCELLED" :foreground "forest grey" :weight bold)
                               ("ABORT" :foreground "forest yellow" :weight bold)
                               ("MEETING" :foreground "forest brown" :weight bold)
                               ("PHONE" :foreground "forest pink" :weight bold)))

;; The triggers break down to the following rules:
;;   Moving a task to CANCELLED adds a CANCELLED tag
;;   Moving a task to WAITTING adds a WAITTING tag
;;   Moving a task to HOLD adds WAITTING and HOLD tags
;;   Moving a task to a done state removes WAITTING and HOLD tags
;;   Moving a task to TODO removes WAITTING, CANCELLED, and HOLD tags
;;   Moving a task to NEXT removes WAITTING, CANCELLED, and HOLD tags
;;   Moving a task to DONE removes WAITTING, CANCELLED, and HOLD tags
(setq org-todo-state-tags-triggers '(("CANCELLED" ("CANCELLED" . t))
									 ("WAITTING" ("WAITTING" . t))
									 ("HOLD" ("WAITTING") ("HOLD" . t))
									 ("DONE" ("WAITTING") ("HOLD"))
									 ("TODO" ("WAITTING") ("CANCELLED") ("HOLD"))
									 ("NEXT" ("WAITTING") ("CANCELLED") ("HOLD"))
									 ("DONE" ("WAITTING") ("CANCELLED") ("HOLD"))))


;; Tags with fast selection keys
;; (setq org-tag-alist (quote ((:startgroup)
;;                             ("@errand" . ?e)
;;                             ("@office" . ?o)
;;                             ("@home" . ?H)
;;                             (:endgroup)
;;                             ("WAITTING" . ?w)
;;                             ("HOLD" . ?h)
;;                             ("PERSONAL" . ?P)
;;                             ("WORK" . ?W)
;;                             ("ORG" . ?O)
;;                             ("NORANG" . ?N)
;;                             ("crypt" . ?E)
;;                             ("NOTE" . ?n)
;;                             ("CANCELLED" . ?c)
;;                             ("FLAGGED" . ??))))

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

;; Embed inline CSS read from a file.
;;;###autoload
(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           ;;(path (concat dir "style.css"))
           (path  "~/.emacs.d/documents/my-org-style-min.css")
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
;; exactly isn't important for me – as long as it is saved and
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
;; haven't needed this functionality yet – it's there if you need
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 快捷键设置 keys are set in init-key.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)
;; (global-set-key (kbd "C-c C") 'org-capture)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; C-',  C-, is org-cycle-agenda-files keys
;; 新版的org-mode使用C-c C-, 替换了 <sTAB 提供的模板功能。

;; https://github.com/Fuco1/org-pretty-table
;;(require 'org-pretty-table)
;;(add-hook 'org-mode-hook (lambda () (org-pretty-table-mode)))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  ;;:custom
  ;;(org-roam-directory (concat user-home-dir "/RoamNotes"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-oram-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup)
  )
(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
