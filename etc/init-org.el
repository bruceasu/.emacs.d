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

;; 频繁打开/关闭输入法还是挺翻的，比如在连续输入章节或者列表项时，
;; 输入章节前面的 * 或者 列表项 前面的 * 或者 - 号，
;; 都要先关闭输入法，否则输入的是 × 和
;; 不过可以用下面的方法解决:
(defun org-mode-my-init ()
  "中文字符也可以做语法。"
  (define-key org-mode-map (kbd "＃") (kbd "#"))
  (define-key org-mode-map (kbd "＊") (kbd "*"))
  (define-key org-mode-map (kbd "×") (kbd "*"))
  (define-key org-mode-map (kbd "－") (kbd "-"))
  (define-key org-mode-map (kbd "＋") (kbd "+"))
  )


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
  :commands org-try-structure-completion
  :functions hydra-org-template/body
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c l" . org-store-link)
         ("C-c C" . org-capture)
         )
  :hook ((org-indent-mode . (lambda() (diminish 'org-indent-mode)))
         (org-mode-hook . (lambda ()
                            (abbrev-mode 1)
                            (setq truncate-lines nil)
                            (set-fill-column 70)
                            (turn-on-font-lock)
                            (org-mode-my-init)))
         (org-clock-out-hook . (lambda () (quote (suk/remove-empty-drawer-on-clock-out （quote append))))
         )
  :config
  (setq org-log-done 'time
        org-startup-indented t
        org-startup-folded t
        org-ellipsis " → "
        org-pretty-entities t
        org-hide-emphasis-markers t
        org-cycle-include-plain-lists t
        org-cycle-separator-lines 0
        org-clone-delete-id t
        ;; org-archive-tag "资料库"
        ;; org-closed-string "任务关闭："
        ;; org-comment-string "备注"
        ;; org-deadline-string "截止时间："
        ;; org-quote-string "引用"
        ;; org-scheduled-string "计划时间："
        ;; org-time-stamp-custom-formats (quote ("<%Y年%m月%d日 %A>" . "<%Y年%m月%d日 %A %H:%M>"))
        org-todo-interpretation (quote sequence)
        org-file-apps (quote ((auto-mode . emacs)
                              ("\\.mm\\'" . system)
                              ("\\.x?html?\\'" . system)
                              ("\\.pdf\\'" . system)))
        ;;; Overwrite the current window with the agenda
        org-agenda-window-setup 'current-window
        org-agenda-persistent-filter t
        org-agenda-insert-diary-extract-time t
        ;; Do not dim blocked tasks
        org-agenda-dim-blocked-tasks nil
        ;; Compact the block agenda view
        org-agenda-compact-blocks t
        org-todo-keywords (quote
                           ((sequence "TODO(t)" "NEXT(n!)" "|" "DONE(d!)")
                            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")))
        org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                       ("NEXT" :foreground "blue" :weight bold)
                                       ("DONE" :foreground "forest green" :weight bold)
                                       ("WAITING" :foreground "orange" :weight bold)
                                       ("HOLD" :foreground "magenta" :weight bold)
                                       ("CANCELLED" :foreground "forest grey" :weight bold)
                                       ("ABORT" :foreground "forest yellow" :weight bold)
                                       ("MEETING" :foreground "forest brown" :weight bold)
                                       ("PHONE" :foreground "forest pink" :weight bold)))
        org-use-fast-todo-selection t
        ;;; allows changing todo states with S-left and S-right skipping all of the
        ;;; normal processing when entering or leaving a todo state. This cycles through
        ;;; the todo states but skips setting timestamps and entering notes which is very
        ;;; convenient when all you want to do is fix up the status of an entry.
        org-treat-S-cursor-todo-selection-as-state-change nil
        ;;; The triggers break down to the following rules:
        ;;;   Moving a task to CANCELLED adds a CANCELLED tag
        ;;;   Moving a task to WAITING adds a WAITING tag
        ;;;   Moving a task to HOLD adds WAITING and HOLD tags
        ;;;   Moving a task to a done state removes WAITING and HOLD tags
        ;;;   Moving a task to TODO removes WAITING, CANCELLED, and HOLD tags
        ;;;   Moving a task to NEXT removes WAITING, CANCELLED, and HOLD tags
        ;;;   Moving a task to DONE removes WAITING, CANCELLED, and HOLD tags
        org-todo-state-tags-triggers (quote (("CANCELLED" ("CANCELLED" . t))
                                             ("WAITING" ("WAITING" . t))
                                             ("HOLD" ("WAITING") ("HOLD" . t))
                                             (done ("WAITING") ("HOLD"))
                                             ("DONE" ("WAITING") ("HOLD"))
                                             ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
                                             ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
                                             ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))
        ;;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
        org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9)))
        ;;; Use full outline paths for refile targets - we file directly with IDO
        org-refile-use-outline-path t
        ;;; Targets complete directly with IDO
        org-outline-path-complete-in-steps nil
        ;;; Allow refile to create parent tasks with confirmation
        org-refile-allow-creating-parent-nodes (quote confirm)
        ;;; Use the current window for indirect buffer display
        org-indirect-buffer-display (quote current-window)
        ;;; setup agenda files
        ;;; org-mode manages the org-agenda-files variable automatically
        ;;; using C-c [ and C-c ] to add and remove files respectively.
        ;;; They can be files or directories.
        org-agenda-files (quote ("D:/victor/org/gtd.org"
                               "D:/victor/org/work.org"
                               "D:/victor/org/notes.org"
                               "D:/victor/org/finished.org"
                               "D:/victor/org/cancel.org"
                               "D:/victor/org/trash.org"
                               "D:/victor/org/gw"
                               ))
        ;;; capture template
        org-default-notes-file "D:/victor/org/notes.org"
        ;;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
        org-capture-templates (quote (("t" "Todo" entry (file+headline "D:/victor/org/gtd.org" "Tasks")
                                       "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                                      ("n" "note" entry (file "D:/victor/org/notes.org")
                                       "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                                      ("r" "respond" entry (file "D:/victor/org/gtd.org")
                                       "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                                      ("j" "Journal" entry (file+datetree "D:/victor/org/journal.org")
                                       "* %?\nEntered on %U\n  %i\n  %a")
                                      ("w" "org-protocol" entry (file "D:/victor/org/gtd.org")
                                       "* TODO Review %c\n%U\n" :immediate-finish t)
                                      ("m" "Meeting" entry (file "D:/victor/org/gtd.org")
                                       "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                                      ("p" "Phone call" entry (file "D:/victor/org/gtd.org")
                                       "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                                      ("h" "Habit" entry (file "D:/victor/org/gtd.org")
                                       "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")
                                      ))
        org-refile-target-verify-function (quote suk/verify-refile-target)
        ;;; Show lot of clocking history so it's easy to pick items off the C-F11 list
        org-clock-history-length 23
        ;;; Resume clocking task on clock-in if the clock is open
        org-clock-in-resume t
        ;;; Separate drawers for clocking and logs
        org-drawers (quote ("PROPERTIES" "LOGBOOK"))
        ;;; Save clock data and state changes and notes in the LOGBOOK drawer
        org-clock-into-drawer t
        ;;; Sometimes I change tasks I'm clocking quickly - this removes clocked
        ;;; tasks with 0:00 duration
        org-clock-out-remove-zero-time-clocks t
        ;;; Clock out when moving task to a done state
        org-clock-out-when-done t
        ;;; Save the running clock and all clock history when exiting Emacs, load it on startup
        org-clock-persist t
        ;;; Do not prompt to resume an active clock
        org-clock-persist-query-resume nil
        ;;; Enable auto clock resolution for finding open clocks
        org-clock-auto-clock-resolution (quote when-no-clock-is-running)
        ;;; Include current clocking task in clock reports
        org-clock-report-include-clocking-task t
        ;;; Sometimes I change tasks I'm clocking quickly
        ;;; - this removes clocked tasks with 0:00 duration
        org-clock-out-remove-zero-time-clocks t
        ;;; Agenda clock report parameters
        org-agenda-clockreport-parameter-plist (quote (:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))
        ;;; Set default column view headings: Task Effort Clock_Summary
        org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM"
        ;;; global Effort estimate values
        ;;; global STYLE property values for completion
        org-global-properties (quote (("Effort_ALL" . "0:15 0:30 0:45 1:00 2:00 3:00 4:00 5:00 6:00 0:00")
                                      ("STYLE_ALL" . "habit")))
        ;;; Agenda log mode items to display (closed and state changes by default)
        org-agenda-log-mode-items (quote (closed state))

        ;;; Entering C-c C-x i RET inserts a clock table report with your
        ;;; estimated values and any clocked time to date.

        ;;; Tags with fast selection keys
        org-tag-alist (quote ((:startgroup)
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
                              ("FLAGGED" . ??)))
        ;;; Filetags look like this:
        ;;; #+FILETAGS: NORANG @office

        ;;; Allow setting single tags without the menu
        org-fast-tag-selection-single-key (quote expert)

        ;;; For tag searches ignore tasks with scheduled and deadline dates
        org-agenda-tags-todo-honor-ignore-options t

        org-agenda-span (quote day)
        ;;; so only today's date is shown by default.
        ;;; I only need the weekly view during my weekly review
        ;;; and this keeps my agenda generation fast.
        ;;; I have a recurring task which keeps my weekly review checklist handy.
        ;;; This pops up as a reminder on Monday's. This week I'm doing my weekly review
        ;;; on Tuesday since Monday was a holiday.
        ;;; * NEXT Weekly Review [0/6]
        ;;;  SCHEDULED: <2009-05-18 Mon ++1w>
        ;;;  What to review:
        ;;;
        ;;;   - [ ] Check follow-up folder
        ;;;   - [ ] Review weekly agenda =F12 a w //=
        ;;;   - [ ] Check clocking data for past week =v c=
        ;;;   - [ ] Review clock report for past week =R=
        ;;;     - Check where we spent time (too much or too little) and rectify this week
        ;;;   - [ ] Look at entire agenda for today  =F12 SPC=
        ;;;   - [ ] Review projects =F12 SPC //= and =V= repeatedly to view each project
        ;;;
        ;;;   - start work
        ;;;     - daily agenda first - knock off items
        ;;;     - then work on NEXT tasks

        org-stuck-projects (quote ("" nil nil ""))

        org-archive-mark-done nil
        org-archive-location "%s_archive::* Archived Tasks"

        org-alphabetical-lists t
        ;;; Inline images in HTML instead of producting links to the image
        org-html-inline-images t

        ;;; Use org-manual.css from the norang website for export document stylesheets
        ;;; org-html-head-extra "<link rel=\"stylesheet\" href=\"org-manual.css\" type=\"text/css\" />"
        
        org-html-head-include-default-style nil
        ;;; Do not generate internal css formatting for HTML exports
        org-export-htmlize-output-type (quote css)
        ;;; Export with LaTeX fragments
        org-export-with-LaTeX-fragments t
        ;;; Increase default number of headings to export
        org-export-headline-levels 6
        org-latex-listings t
        org-html-xml-declaration (quote (("html" . "")
                                         ("was-html" . "<?xml version=\"1.0\" encoding=\"%s\"?>")))
        org-export-allow-BIND t
        org-read-date-prefer-future 'time
        org-tags-match-list-sublevels t
        org-agenda-persistent-filter t
        org-link-mailto-program (quote (compose-mail "%a" "%s"))
        org-agenda-skip-additional-timestamps-same-entry t
        org-table-use-standard-references (quote from)
        org-clone-delete-id t
        org-cycle-include-plain-lists t
        org-src-fontify-natively t
        org-startup-folded t
        org-src-preserve-indentation nil
        org-edit-src-content-indentation 0
        org-catch-invisible-edits 'error
        org-export-coding-system 'utf-8
        ;;; Use sticky agenda's so they persist
        org-agenda-sticky t
        org-enforce-todo-dependencies t
        org-hide-leading-stars nil
        org-startup-indented t
        org-cycle-separator-lines 0
        org-blank-before-new-entry (quote ((heading)
                                                 (plain-list-item . auto)))
        org-insert-heading-respect-content nil
        org-reverse-note-order nil
        org-show-following-heading t
        org-show-hierarchy-above t
        org-show-siblings (quote ((default)))
        org-special-ctrl-a/e t
        org-special-ctrl-k t
        org-yank-adjusted-subtrees t
        org-deadline-warning-days 30
        org-table-export-default-format "orgtbl-to-csv"
        org-link-frame-setup (quote ((vm . vm-visit-folder)
                                           (gnus . org-gnus-no-new-news)
                                           (file . find-file)))
        ;;; Use the current window for C-c ' source editing
        org-src-window-setup 'current-window
        org-log-done (quote time)
        org-log-into-drawer t
        org-log-state-notes-insert-after-drawers nil
        ;; org-hide-leading-stars nil
        ;; org-enforce-todo-dependencies t
        )

  (add-to-list 'org-export-backends 'md)
  ;; (setq org-bullets-bullet-list '("■" "◆" "▲" "▶"))
  ;; More fancy UI
  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))
                                        ; (unless sys/win32p
                                        ;  (use-package org-fancy-priorities
                                        ;   :diminish
                                        ;  :defines org-fancy-priorities-list
                                        ; :hook (org-mode . org-fancy-priorities-mode)
                                        ;:config (setq org-fancy-priorities-list '("⚡" "⬆" "⬇" "☕"))))
  ;; Babel
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defvar load-language-list '((emacs-lisp . t)
                               (perl . t)
                               (python . t)
                               (ruby . t)
                               (js . t)
                               (css . t)
                               (sass . t)
                               (C . t)
                               (java . t)
                               (plantuml . t)))

  ;; ob-sh renamed to ob-shell since 26.1.
  (if emacs/>=26p
      (cl-pushnew '(shell . t) load-language-list)
    (cl-pushnew '(sh . t) load-language-list))

  (org-babel-do-load-languages 'org-babel-load-languages
                               load-language-list)

  ;; Rich text clipboard
  (use-package org-rich-yank
    :bind (:map org-mode-map
                ("C-M-y" . org-rich-yank)))

  ;; Preview
  (use-package org-preview-html
    :diminish org-preview-html-mode)

  ;; Presentation
  (use-package org-tree-slide
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images
                winner-undo)
    :bind (:map org-mode-map
                ("C-<f9>" . org-tree-slide-mode)
                :map org-tree-slide-mode-map
                ("<left>" . org-tree-slide-move-previous-tree)
                ("<right>" . org-tree-slide-move-next-tree)
                ("S-SPC" . org-tree-slide-move-previous-tree)
                ("SPC" . org-tree-slide-move-next-tree))
    :hook ((org-tree-slide-play . (lambda ()
                                    (text-scale-set 4)
                                    (org-display-inline-images)
                                    (read-only-mode 1)
                                    (if (fboundp 'hide-mode-line-mode)
                                        (hide-mode-line-mode 1))
                                    (delete-other-windows)))
           (org-tree-slide-stop . (lambda ()
                                    (text-scale-set 0)
                                    (org-remove-inline-images)
                                    (read-only-mode -1)
                                    (if (fboundp 'hide-mode-line-mode)
                                        (hide-mode-line-mode -11))
                                    (winner-undo))))
    :config
    (org-tree-slide-simple-profile)
    (setq org-tree-slide-skip-outline-level 2))

  (use-package org-present
    :diminish
    :functions (org-display-inline-images
                org-remove-inline-images
                winner-undo)
    :commands (org-present-big
               org-present-hide-cursor
               org-present-read-only
               org-present-small
               org-present-show-cursor
               org-present-read-write)
    :bind (:map org-mode-map
                ("M-<f9>" . org-present))
    :hook ((org-present-mode . (lambda ()
                                 (org-present-big)
                                 (org-display-inline-images)
                                 (org-present-hide-cursor)
                                 (org-present-read-only)
                                 (if (fboundp 'hide-mode-line-mode)
                                     (hide-mode-line-mode 1))
                                 (delete-other-windows)))
           (org-present-mode-quit . (lambda ()
                                      (org-present-small)
                                      (org-remove-inline-images)
                                      (org-present-show-cursor)
                                      (org-present-read-write)
                                      (read-only-mode -1)
                                      (if (fboundp 'hide-mode-line-mode)
                                          (hide-mode-line-mode -1))
                                      (winner-undo)))))

  ;; Pomodoro
  (use-package org-pomodoro
    :after org-agenda
    :bind (:map org-agenda-mode-map
                ("P" . org-pomodoro)))

  ;; Visually summarize progress
  (use-package org-dashboard)

  (with-eval-after-load 'hydra
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

    (defhydra hydra-org-template (:color blue :hint nil)
      "
_c_enter  _q_uote     _E_macs-lisp    _L_aTeX:
_l_atex   _e_xample   p_y_thon        _i_ndex:
_a_scii   _v_erse     ip_Y_thon       _I_NCLUDE:
_s_rc     _g_o        _r_uby          _H_TML:
_h_tml    _S_HELL     _p_erl          _A_SCII:
^ ^       ^ ^         _P_erl tangled  plant_u_ml
"
      ("s" (hot-expand "<s"))
      ("e" (hot-expand "<e"))
      ("q" (hot-expand "<q"))
      ("v" (hot-expand "<v"))
      ("c" (hot-expand "<c"))
      ("l" (hot-expand "<l"))
      ("h" (hot-expand "<h"))
      ("a" (hot-expand "<a"))
      ("L" (hot-expand "<L"))
      ("i" (hot-expand "<i"))
      ("E" (hot-expand "<s" "emacs-lisp"))
      ("y" (hot-expand "<s" "python :results output"))
      ("Y" (hot-expand "<s" "ipython :session :exports both :results raw drawer\n$0"))
      ("g" (hot-expand "<s" "go :imports '\(\"fmt\"\)"))
      ("p" (hot-expand "<s" "perl"))
      ("r" (hot-expand "<s" "ruby"))
      ("S" (hot-expand "<s" "sh"))
      ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
      ("P" (progn
             (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
             (hot-expand "<s" "perl")))
      ("I" (hot-expand "<I"))
      ("H" (hot-expand "<H"))
      ("A" (hot-expand "<A"))
      ("<" self-insert-command "ins")
      ("t" nil "quit"))

    (bind-key "<"
              (lambda () (interactive)
                (if (or (region-active-p) (looking-back "^" 1))
                    (hydra-org-template/body)
                  (self-insert-command 1)))
              org-mode-map)))

(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))

;;; 快捷键设置
;;; I use C-c c to start capture mode 冲突了。
;; (global-set-key (kbd "C-c C") 'org-capture)
;; (global-set-key "\C-cc" 'org-capture)
;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)


(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)

(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "C-<f10>") 'next-buffer)

(global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)


;; 然后使用define-key来为org-mode定义一个简单的按键绑定，如下：
;;(defun my-org-func ()
;;  (interactive)
;;  (message "hello, org!"))
;;(define-key org-mode-map (kbd "<S-f6>") 'bh/widen)

;; (global-set-key (kbd "<f6>") 'bh/org-todo)
;; (global-set-key (kbd "<S-f6>") 'bh/widen)

; has set to f7, c-f7
;(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
;(global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))

;; (glbal-set-key (kbd "<f7>") 'bh/set-truncate-lines)
;; C-',  C-, is org-cycle-agenda-files keys
;; (global-set-key (kbd "<f8>") 'org-cycle-agenda-files)


(define-prefix-command 'f9-map)
(global-set-key (kbd "<f9>") 'f9-map)
(global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)

(global-set-key (kbd "<f9> a") 'org-capture)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> h") 'bh/hide-other)
(global-set-key (kbd "<f9> I") 'bh/punch-in)
(global-set-key (kbd "<f9> O") 'bh/punch-out)

(global-set-key (kbd "<f9> o") 'bh/make-org-scratch)
(global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))

(defun bh/switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Time Clocking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)

;;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)

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

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

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

(setq org-time-stamp-rounding-minutes (quote (1 1)))

(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "org-manual.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle "~/.emacs.d/documents/org-manual.css" path))) ;; <- set your own style file path
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))

(add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)

;;; 更多的配置
(require 'suk-org)

(provide 'init-org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
