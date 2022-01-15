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
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c l" . org-store-link)
         ("C-c C" . org-capture))
  :hook ((org-indent-mode . (lambda() (diminish 'org-indent-mode)))
         (org-mode-hook . (lambda ()
                            (abbrev-mode 1)
                            (setq truncate-lines nil)
                            (set-fill-column 70)
                            (turn-on-font-lock)
                            (org-mode-my-init)))
         (org-clock-out-hook . (lambda ()
								 (quote (suk/remove-empty-drawer-on-clock-out （quote append)))))
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

 ;; Set default column view headings: Task Effort Clock_Summary
 (setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
 (setq org-agenda-persistent-filter t)
 

 ;; capture template
 (setq org-default-notes-file "~/org/notes.org")
 ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and (setq org-protocol)
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
 

 ;; TODO TASK 不要使用中文，自寻烦恼
 ;; org-archive-tag "资料库"
 ;; org-closed-string "任务关闭："
 ;; org-comment-string "备注"
 ;; org-deadline-string "截止时间："
 ;; org-quote-string "引用"
 ;; org-scheduled-string "计划时间："
 ;; org-time-stamp-custom-formats (quote ("<%Y年%m月%d日 %A>" . "<%Y年%m月%d日 %A %H:%M>"))

 ;; 设置打开某种文件类型
 (setq org-file-apps (quote ((auto-mode . emacs)
                             ("\\.mm\\'" . system)
                             ("\\.x?html?\\'" . system)
                             ("\\.pdf\\'" . system))))

 (setq org-todo-keywords (quote
                          ((sequence "TODO(t)" "NEXT(n!)" "|" "DONE(d!)")
                           (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING"))))

 (setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                      ("NEXT" :foreground "blue" :weight bold)
                                      ("DONE" :foreground "forest green" :weight bold)
                                      ("WAITING" :foreground "orange" :weight bold)
                                      ("HOLD" :foreground "magenta" :weight bold)
                                      ("CANCELLED" :foreground "forest grey" :weight bold)
                                      ("ABORT" :foreground "forest yellow" :weight bold)
                                      ("MEETING" :foreground "forest brown" :weight bold)
                                      ("PHONE" :foreground "forest pink" :weight bold))))

 ;; The triggers break down to the following rules:
 ;;   Moving a task to CANCELLED adds a CANCELLED tag
 ;;   Moving a task to WAITING adds a WAITING tag
 ;;   Moving a task to HOLD adds WAITING and HOLD tags
 ;;   Moving a task to a done state removes WAITING and HOLD tags
 ;;   Moving a task to TODO removes WAITING, CANCELLED, and HOLD tags
 ;;   Moving a task to NEXT removes WAITING, CANCELLED, and HOLD tags
 ;;   Moving a task to DONE removes WAITING, CANCELLED, and HOLD tags
 (setq org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
											("WAITING" ("WAITING" . t))
											("HOLD" ("WAITING") ("HOLD" . t))
											("DONE" ("WAITING") ("HOLD"))
											("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
											("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
											("DONE" ("WAITING") ("CANCELLED") ("HOLD")))))

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
 (setq org-tag-alist (quote ((:startgroup)
                             ("@errand" . ?e)
                             ("@office" . ?o)
                             ("@home" . ?H)
                             (:endgroup)
                             ("WAITING" . ?w)
                             ("HOLD" . ?h)
                             ("PERSONAL" . ?P)
                             ("WORK" . ?W)
                             ("ORG" . ?O)
                             ("NORANG" . ?N)
                             ("crypt" . ?E)
                             ("NOTE" . ?n)
                             ("CANCELLED" . ?c)
                             ("FLAGGED" . ??))))
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

;; More fancy UI
;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode)
;; 	:config  (setq org-bullets-bullet-list '("■" "◆" "▲" "▶")))


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

;;; 快捷键设置
;;; I use C-c c to start capture mode 冲突了。
;; (global-set-key (kbd "C-c C") 'org-capture)
;; (global-set-key "\C-cc" 'org-capture)
;; (global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)


;; setting in init-key.el
;;(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)

;; C-',  C-, is org-cycle-agenda-files keys

;;(define-prefix-command 'f9-map)
;;(global-set-key (kbd "<f9>") 'f9-map)
;;(global-set-key (kbd "<f9> a") 'org-agenda)
;;(global-set-key (kbd "<f9> s") 'show-org-agenda)
;;(global-set-key (kbd "<f9> c") 'org-capture)
;;(global-set-key (kbd "<f9> d") 'calendar)
;;(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
;;(global-set-key (kbd "<f9> r") 'boxquote-region)
;;(global-set-key (kbd "<f9> v") 'visible-mode)
;;(global-set-key (kbd "<f9> l") 'org-toggle-link-display)

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

;; 新版的org-mode使用C-c C-, 替换了 <sTAB 提供的模板功能。

;;; 更多的配置
(require 'suk-org)

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
