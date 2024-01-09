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
  (require '+const))
;;

;; I have the following setup to remove these empty LOGBOOK drawers if they occur.
;; Remove empty LOGBOOK drawers on clock out
(defun suk/remove-empty-drawer-on-clock-out ()
  (interactive)
  (save-excursion
    (beginning-of-line 0)
    (org-remove-empty-drawer-at "LOGBOOK" (point))))

;;; Exclude DONE state tasks from refile targets
(defun suk/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))


(use-package org
  :ensure nil
  :bind (progn ("C-c a" . org-agenda)
               ("C-c b" . org-switchb)
               ("C-c l" . org-store-link)
               ("C-c C" . org-capture))
  :hook (progn (org-indent-mode . (lambda() (diminish 'org-indent-mode)))
               (org-mode-hook . (lambda ()
                                  (abbrev-mode 1)
                                  (setq truncate-lines nil)
                                  (set-fill-column 70)
                                  (turn-on-font-lock)
                                  (org-mode-my-init)))
               (org-clock-out-hook . (lambda ()
								       (quote (suk/remove-empty-drawer-on-clock-out (quote append))))))
  :config
  ;; ==============================================================
  ;; Agenda
  ;; --------------------------------------------------------------
  (setq org-agenda-diary-file "~/org/diary.org")
  ;; setup agenda files
  ;; org-mode manages the org-agenda-files variable automatically
  ;; using C-c [ and C-c ] to add and remove files respectively.
  ;; They can be files or directories.
  (setq org-agenda-files (quote ("~/org/gtd.org"
								 "~/org/work.org"
								 "~/org/notes.org"
								 "~/org/finished.org"
								 "~/org/cancel.org"
								 "~/org/trash.org"
								 "~/org/gw"
								 )))

  (setq org-agenda-persistent-filter t)


  ;; capture template
  (setq org-default-notes-file "~/org/notes.org")
  ;; Capture templates for: TODO tasks, Notes,
  ;; appointments, phone calls, meetings, and (setq
  ;; org-protocol)
  (setq org-capture-templates (quote (("t" "Todo"
									   entry (file+headline "~/org/gtd.org" "Tasks")
									   "* TODO %?\n%U\n%a\n"
									   :clock-in t
									   :clock-resume t)
									  ("n" "note"
									   entry (file "~/org/notes.org")
									   "* %? :NOTE:\n%U\n%a\n"
									   :clock-in t
									   :clock-resume t)
									  ("r" "respond"
									   entry (file "~/org/gtd.org")
									   "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
									   :clock-in t
									   :clock-resume t
									   :immediate-finish t)
									  ("j" "Journal"
									   entry (file+datetree "~/org/journal.org")
									   "* %?\nEntered on %U\n  %i\n  %a")
									  ("w" "Review"
									   entry (file "~/org/gtd.org")
									   "* TODO Review %c\n%U\n"
									   :immediate-finish t)
									  ("m" "Meeting"
									   entry (file "~/org/gtd.org")
									   "* MEETING with %? :MEETING:\n%U"
									   :clock-in t
									   :clock-resume t)
									  ("p" "Phone call"
									   entry (file "~/org/gtd.org")
									   "* PHONE %? :PHONE:\n%U"
									   :clock-in t
									   :clock-resume t)
									  ("h" "Habit"
									   entry (file "~/org/gtd.org")
									   "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
									  )))


  ;; Phone capture template handling with BBDB lookup
  ;; Adapted from code by Gregory J. Grubbs

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
  (setq org-file-apps (quote ((auto-mode . emacs)
                              ("\\.mm\\'" . system)
                              ("\\.x?html?\\'" . system)
                              ("\\.pdf\\'" . system))))
  ;; ! 的含义是记录某项更改为状态的时间。我不把这个添加到完成的状态，是因为它们已经被记录了。
  ;; @ 符号表示带理由的提示，所以当切换到 WAITTING 时，Org 模式会问我为什么，并将这个添加到笔记中。
  (setq org-todo-keywords (quote
                           ((sequence "TODO(t)" "NEXT(n!)" "|" "DONE(d!)" "CANCELLED(c@/!)")
                            (sequence "WAITTING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

  (setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                       ("NEXT" :foreground "blue" :weight bold)
                                       ("DONE" :foreground "forest green" :weight bold)
                                       ("WAITTING" :foreground "orange" :weight bold)
                                       ("HOLD" :foreground "magenta" :weight bold)
                                       ("CANCELLED" :foreground "forest grey" :weight bold)
                                       ("ABORT" :foreground "forest yellow" :weight bold)
                                       ("MEETING" :foreground "forest brown" :weight bold)
                                       ("PHONE" :foreground "forest pink" :weight bold))))

  ;; The triggers break down to the following rules:
  ;;   Moving a task to CANCELLED adds a CANCELLED tag
  ;;   Moving a task to WAITTING adds a WAITTING tag
  ;;   Moving a task to HOLD adds WAITTING and HOLD tags
  ;;   Moving a task to a done state removes WAITTING and HOLD tags
  ;;   Moving a task to TODO removes WAITTING, CANCELLED, and HOLD tags
  ;;   Moving a task to NEXT removes WAITTING, CANCELLED, and HOLD tags
  ;;   Moving a task to DONE removes WAITTING, CANCELLED, and HOLD tags
  (setq org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
											 ("WAITTING" ("WAITTING" . t))
											 ("HOLD" ("WAITTING") ("HOLD" . t))
											 ("DONE" ("WAITTING") ("HOLD"))
											 ("TODO" ("WAITTING") ("CANCELLED") ("HOLD"))
											 ("NEXT" ("WAITTING") ("CANCELLED") ("HOLD"))
											 ("DONE" ("WAITTING") ("CANCELLED") ("HOLD")))))

  ;; allows changing todo states with S-left and S-right skipping all of the
  ;; normal processing when entering or leaving a todo state. This cycles through
  ;; the todo states but skips setting timestamps and entering notes which is very
  ;; convenient when all you want to do is fix up the status of an entry.
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-deadline-warning-days 30)
  (setq org-log-done 'time)
  (setq org-log-done (quote time))
  (setq org-log-into-drawer t)
  (setq org-log-state-notes-insert-after-drawers nil)
  ;; Sometimes I change tasks I'm clocking quickly
  ;; - this removes clocked tasks with 0:00 duration
  (setq org-clock-out-remove-zero-time-clocks t)

  ;; Entering C-c C-x i RET inserts a clock table report with your
  ;; estimated values and any clocked time to date.

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

  ;; EXPORTER
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


  ;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
  (setq org-refile-targets (quote ((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))))
  ;; Use full outline paths for refile targets - we file directly with IDO
  (setq org-refile-use-outline-path t)
  ;; Targets complete directly with IDO
  (setq org-outline-path-complete-in-steps nil)
  ;; Allow refile to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes (quote confirm)  )

  (setq org-refile-target-verify-function (quote suk/verify-refile-target))



  ;; Inline images in HTML instead of producting links to the image
  (setq org-html-inline-images t)

  (setq org-startup-indented t)
  (setq org-startup-folded t)
  (setq org-ellipsis " → ")
  (setq org-pretty-entities t)
  (setq org-hide-emphasis-markers t)
  (setq org-hide-leading-stars nil)
  (setq org-startup-indented t)
  ;; global Effort estimate values
  ;; global STYLE property values for completion
  (setq org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00") ("STYLE_ALL" . "habit"))))
  (setq org-blank-before-new-entry (quote ((heading) (plain-list-item . auto))))
  (setq org-insert-heading-respect-content nil)
  (setq org-yank-adjusted-subtrees t)
  ;; Use the current window for C-c ' source editing
  (setq org-src-window-setup 'current-window)
  ;; Use the current window for indirect buffer display
  (setq org-indirect-buffer-display (quote current-window))
  (add-to-list 'org-export-backends 'md)
  )

(eval-and-compile
  (defun hot-expand (str &optional mod)
    "Expand org template."
    (let (text)
      (when (region-active-p)
        (setq text (buffer-substring (region-beginning) (region-end)))
        (delete-region (region-beginning) (region-end)))
      (insert str)
      (org-try-structure-completion)
      (when mod (insert mod) (forward-line))
      (when text (insert text)))))

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

(setq org-time-stamp-rounding-minutes (quote (1 1)))
(defun show-org-agenda ()
  (interactive)
  (switch-to-buffer "*Org Agenda*")
  (delete-other-windows))

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



;;; 更多的配置
;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)) 'append)

;; The following custom-set-faces create the highlights
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t))

(add-to-list 'Info-default-directory-list "~/org/doc")
;; flyspell mode for spell checking everywhere
;; (add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

(setq default-process-coding-system '(utf-8-unix . utf-8-unix))


(setq org-use-fast-todo-selection t)

;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;; ; Use full outline paths for refile targets - we file directly with IDO
;; (setq org-refile-use-outline-path t)

;; ; Targets complete directly with IDO
;; (setq org-outline-path-complete-in-steps nil)

                                        ; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;;; Use IDO for both buffer and file completion and ido-everywhere to t
;; (setq org-completion-use-ido t)
;; (setq ido-everywhere t)
;; (setq ido-max-directory-size 100000)
;; (ido-mode (quote both))
;;; Use the current window when visiting files and buffers with ido
;; (setq ido-default-file-method 'selected-window)
;; (setq ido-default-buffer-method 'selected-window)

;; Use the current window for indirect buffer display
(setq org-indirect-buffer-display 'current-window)

;;;; Refile settings
;; Do not dim blocked tasks
(setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)


;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
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

(setq bh/keep-clock-running nil)

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
                                        ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p)) (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
                       (org-clock-in))))

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

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
                     (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

(require 'org-id)
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
                     (org-clock-in nil)))

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

;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist
      (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80)))

(setq org-alphabetical-lists t)


;; Do not generate internal css formatting for HTML exports
(setq org-export-htmlize-output-type (quote css))

;; Erase all reminders and rebuilt reminders for today from the agenda
(defun bh/org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))

;; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-agenda-finalize-hook 'bh/org-agenda-to-appt 'append)

;; This is at the end of my .emacs - so appointments are set up when Emacs starts
(bh/org-agenda-to-appt)

;; Activate appointments so we get notifications
(appt-activate t)

;; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'bh/org-agenda-to-appt)


;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook
          '(lambda () (hl-line-mode 1))
          'append)

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
      (quote ((agenda habit-down time-up user-defined-up effort-up category-keep)
              (todo category-up effort-up)
              (tags category-up effort-up)
              (search category-up))))

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; Use the current window for C-c ' source editing
(setq org-src-window-setup 'current-window)

(setq org-export-with-timestamps nil)

(defun emacs-startup-screen ()
  "Display the weekly org-agenda and all todos."
  (org-agenda nil "n"))
(add-hook 'emacs-startup-hook #'emacs-startup-screen)

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



;;; 快捷键设置 keys are set in init-key.el
;; I use C-c c to start capture mode
(global-set-key (kbd "C-c c") 'org-capture)
;; (global-set-key (kbd "C-c C") 'org-capture)
;; (global-set-key "\C-cc" 'org-capture)

;; (global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)


;; C-',  C-, is org-cycle-agenda-files keys
;; 新版的org-mode使用C-c C-, 替换了 <sTAB 提供的模板功能。

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
