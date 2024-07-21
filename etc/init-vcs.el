;; init-vcs.el --- Initialize version control system configurations.	-*- lexical-binding: t -*-

(provide 'init-lang-vcs)

(eval-when-compile
  (require '+const)
  (require '+fn)
  )


;; ;; {{ Solution 1: disable all vc backends
;; @see http://stackoverflow.com/questions/5748814/how-does-one-disable-vc-git-in-emacs
;; (setq vc-handled-backends nil)
;; }}

;; {{ Solution 2: if NO network mounted drive involved
(setq vc-handled-backends '(Git SVN Hg))
;; @see https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
;; open files faster but you can't check if file is version
;; controlled. other VCS functionality still works.
(remove-hook 'find-file-hook 'vc-find-file-hook)
;; }}


;; Git
;; See `magit-define-global-key-bindings'
(use-package magit
  :init (setq magit-diff-refine-hunk t)
  :config
  (when sys/win32p
    (setenv "GIT_ASKPASS" "git-gui--askpass"))

  ;; Unbind M-1, M-2, M-3, and M-4 shortcuts due to conflict with `ace-window'
  (unbind-key "M-1" magit-mode-map)
  (unbind-key "M-2" magit-mode-map)
  (unbind-key "M-3" magit-mode-map)
  (unbind-key "M-4" magit-mode-map)

  ;; Access Git forges from Magit
  (use-package forge
    :demand t
    :custom-face
    (forge-topic-label ((t (:inherit variable-pitch :height 0.9 :width condensed :weight regular :underline nil))))
    :init (setq forge-topic-list-columns
                '(("#" 5 forge-topic-list-sort-by-number (:right-align t) number nil)
                  ("Title" 60 t nil title  nil)
                  ("State" 6 t nil state nil)
                  ("Updated" 10 t nil updated nil)))))

(with-eval-after-load 'magit
  ;; {{speed up magit, @see https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/
  (when my-prefer-lightweight-magit
    (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
    (remove-hook 'magit-status-sections-hook 'magit-insert-status-headers)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-pushremote)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-pushremote)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpulled-from-upstream)
    (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent))
  ;; }}

  ;; "Continue listing the history of a file beyond renames (works only for a single file)."
  ;; - quoted from "git help log"
  (setq-default magit-buffer-log-args '("--follow"))

  ;; extra check&report after commit
  (defun my-git-check-status ()
    "Check git repo status."
    ;; use timer here to wait magit cool down
    (my-run-with-idle-timer 1 #'my-hint-untracked-files))
  (add-hook 'magit-post-commit-hook #'my-git-check-status)
  (add-hook 'git-commit-post-finish-hook #'my-git-check-status))

;; Display transient in child frame
(when (childframe-completion-workable-p)
  (use-package transient-posframe
    :diminish
    :defines posframe-border-width
    :custom-face
    (transient-posframe ((t (:inherit tooltip))))
    (transient-posframe-border ((t (:inherit posframe-border :background unspecified))))
    :hook (after-init . transient-posframe-mode)
    :init
    (setq transient-posframe-border-width posframe-border-width
          transient-posframe-min-height nil
          transient-posframe-min-width 80
          transient-posframe-poshandler 'posframe-poshandler-frame-center
          transient-posframe-parameters '((left-fringe . 8)
                                          (right-fringe . 8)))
    :config
    (with-no-warnings
      (defun my-transient-posframe--hide ()
        "Hide transient posframe."
        (posframe-hide transient--buffer-name))
      (advice-add #'transient-posframe--delete :override #'my-transient-posframe--hide))))

;; vc-prefix-map 默认是 C-x v
;; Walk through git revisions of a file
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success :foreground unspecified))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning :foreground unspecified))))
  :bind (:map vc-prefix-map
         ("t" . git-timemachine))
  :hook ((git-timemachine-mode . (lambda ()
                                   "Improve `git-timemachine' buffers."
                                   ;; Display different colors in mode-line
                                   (if (facep 'mode-line-active)
                                       (face-remap-add-relative 'mode-line-active 'custom-state)
                                     (face-remap-add-relative 'mode-line 'custom-state))

                                   ;; Highlight symbols in elisp
                                   (and (derived-mode-p 'emacs-lisp-mode)
                                        (fboundp 'highlight-defined-mode)
                                        (highlight-defined-mode t))

                                   ;; Display line numbers
                                   (and (derived-mode-p 'prog-mode 'yaml-mode)
                                        (fboundp 'display-line-numbers-mode)
                                        (display-line-numbers-mode t))))
         (before-revert . (lambda ()
                            (when (bound-and-true-p git-timemachine-mode)
                              (user-error "Cannot revert the timemachine buffer")))))
  :config
  ;; {{ git-timemachine
  (defun my-git-timemachine-show-selected-revision ()
    "Show last (current) revision of file."
    (interactive)
    (let* ((collection (mapcar (lambda (rev)
                                 ;; re-shape list for the ivy-read
                                 (cons (concat (substring-no-properties (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                               (git-timemachine--revisions))))
      (ivy-read "commits:"
                collection
                :action (lambda (rev)
                          ;; compatible with ivy 8+ and later ivy version
                          (unless (string-match "^[a-z0-9]*$" (car rev))
                            (setq rev (cdr rev)))
                          (git-timemachine-show-revision rev)))))

  (defun my-git-timemachine ()
    "Open git snapshot with the selected version."
    (interactive)
    (my-ensure 'git-timemachine)
    (git-timemachine--start #'my-git-timemachine-show-selected-revision))
  ;; }}
  )

;; Pop up last commit information of current line
(use-package git-messenger
  :bind (:map vc-prefix-map
         ("p" . git-messenger:popup-message)
         :map git-messenger-map
         ("m" . git-messenger:copy-message))
  :init (setq git-messenger:show-detail t
              git-messenger:use-magit-popup t)
  :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra git-messenger-hydra (:color blue)
        ("s" git-messenger:popup-show "show")
        ("c" git-messenger:copy-commit-id "copy hash")
        ("m" git-messenger:copy-message "copy message")
        ("," (catch 'git-messenger-loop (git-messenger:show-parent)) "go parent")
        ("q" git-messenger:popup-close "quit")))

    (defun my-git-messenger:format-detail (vcs commit-id author message)
      (if (eq vcs 'git)
          (let ((date (git-messenger:commit-date commit-id))
                (colon (propertize ":" 'face 'font-lock-comment-face)))
            (concat
             (format "%s%s %s \n%s%s %s\n%s  %s %s \n"
                     (propertize "Commit" 'face 'font-lock-keyword-face) colon
                     (propertize (substring commit-id 0 8) 'face 'font-lock-comment-face)
                     (propertize "Author" 'face 'font-lock-keyword-face) colon
                     (propertize author 'face 'font-lock-string-face)
                     (propertize "Date" 'face 'font-lock-keyword-face) colon
                     (propertize date 'face 'font-lock-string-face))
             (propertize (make-string 38 ?─) 'face 'font-lock-comment-face)
             message
             (propertize "\nPress q to quit" 'face '(:inherit (font-lock-comment-face italic)))))
        (git-messenger:format-detail vcs commit-id author message)))

    (defun my-git-messenger:popup-message ()
      "Popup message with `posframe', `pos-tip', `lv' or `message', and dispatch actions with `hydra'."
      (interactive)
      (let* ((hydra-hint-display-type 'message)
             (vcs (git-messenger:find-vcs))
             (file (buffer-file-name (buffer-base-buffer)))
             (line (line-number-at-pos))
             (commit-info (git-messenger:commit-info-at-line vcs file line))
             (commit-id (car commit-info))
             (author (cdr commit-info))
             (msg (git-messenger:commit-message vcs commit-id))
             (popuped-message (if (git-messenger:show-detail-p commit-id)
                                  (my-git-messenger:format-detail vcs commit-id author msg)
                                (cl-case vcs
                                  (git msg)
                                  (svn (if (string= commit-id "-")
                                           msg
                                         (git-messenger:svn-message msg)))
                                  (hg msg)))))
        (setq git-messenger:vcs vcs
              git-messenger:last-message msg
              git-messenger:last-commit-id commit-id)
        (run-hook-with-args 'git-messenger:before-popup-hook popuped-message)
        (git-messenger-hydra/body)
        (cond ((and (fboundp 'posframe-workable-p) (posframe-workable-p))
               (let ((buffer-name "*git-messenger*"))
                 (posframe-show buffer-name
                                :string (concat (propertize "\n" 'face '(:height 0.3))
                                                popuped-message
                                                "\n"
                                                (propertize "\n" 'face '(:height 0.3)))
                                :left-fringe 8
                                :right-fringe 8
                                :max-width (round (* (frame-width) 0.62))
                                :max-height (round (* (frame-height) 0.62))
                                :internal-border-width 1
                                :internal-border-color (face-background 'posframe-border nil t)
                                :background-color (face-background 'tooltip nil t))
                 (unwind-protect
                     (push (read-event) unread-command-events)
                   (posframe-hide buffer-name))))
              ((and (fboundp 'pos-tip-show) (display-graphic-p))
               (pos-tip-show popuped-message))
              ((fboundp 'lv-message)
               (lv-message popuped-message)
               (unwind-protect
                   (push (read-event) unread-command-events)
                 (lv-delete-window)))
              (t (message "%s" popuped-message)))
        (run-hook-with-args 'git-messenger:after-popup-hook popuped-message)))
    (advice-add #'git-messenger:popup-close :override #'ignore)
    (advice-add #'git-messenger:popup-message :override #'my-git-messenger:popup-message)))

;; Resolve diff3 conflicts
(use-package smerge-mode
  :ensure nil
  :diminish
  :pretty-hydra
  ((:title (pretty-hydra-title "Smerge" 'octicon "nf-oct-diff")
    :color pink :quit-key ("q" "C-g"))
   ("Move"
    (("n" smerge-next "next")
     ("p" smerge-prev "previous"))
    "Keep"
    (("b" smerge-keep-base "base")
     ("u" smerge-keep-upper "upper")
     ("l" smerge-keep-lower "lower")
     ("a" smerge-keep-all "all")
     ("RET" smerge-keep-current "current")
     ("C-m" smerge-keep-current "current"))
    "Diff"
    (("<" smerge-diff-base-upper "upper/base")
     ("=" smerge-diff-upper-lower "upper/lower")
     (">" smerge-diff-base-lower "upper/lower")
     ("R" smerge-refine "refine")
     ("E" smerge-ediff "ediff"))
    "Other"
    (("C" smerge-combine-with-next "combine")
     ("r" smerge-resolve "resolve")
     ("k" smerge-kill-current "kill")
     ("ZZ" (lambda ()
             (interactive)
             (save-buffer)
             (bury-buffer))
      "Save and bury buffer" :exit t))))
  :bind (:map smerge-mode-map
         ("C-c m" . smerge-mode-hydra/body))
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (magit-diff-visit-file . (lambda ()
                                    (when smerge-mode
                                      (smerge-mode-hydra/body))))))

;; Open github/gitlab/bitbucket page
(use-package browse-at-remote
  :bind (:map vc-prefix-map
         ("B" . browse-at-remote)))

;; Git configuration modes
(use-package git-modes)


;; {{ git-gutter
(with-eval-after-load 'git-gutter
  (unless (fboundp 'global-display-line-numbers-mode)
    ;; git-gutter's workaround for linum-mode bug.
    ;; should not be used in `display-line-number-mode'
    (git-gutter:linum-setup))

  (setq git-gutter:update-interval 2)
  ;; nobody use bzr
  ;; I could be forced to use subversion or hg which has higher priority
  ;; Please note my $HOME directory is under git control
  (setq git-gutter:handled-backends '(svn hg git))
  (setq git-gutter:disabled-modes
        '(asm-mode
          org-mode
          outline-mode
          markdown-mode
          image-mode))


  )

;;;###autoload
(defun my-git-gutter-reset-to-head-parent()
  "Reset gutter to HEAD^.  Support Subversion and Git."
  (interactive)
  (let* ((filename (buffer-file-name))
         (cmd (concat "git --no-pager log --oneline -n1 --pretty=\"format:%H\" "
                      filename))
         (parent (cond
                  ((eq git-gutter:vcs-type 'svn)
                   "PREV")
                  (filename
                   (concat (shell-command-to-string cmd) "^"))
                  (t
                   "HEAD^"))))
    (git-gutter:set-start-revision parent)
    (message "git-gutter:set-start-revision HEAD^")))
(defvar my-prefer-lightweight-magit t)
;;;###autoload
(defun my-hint-untracked-files ()
  "If untracked files and committed files share same extension, warn users."

  ;; don't scan whole home directory
  (unless (string= (file-truename default-directory) (file-truename "~/"))
    (let* ((exts (mapcar 'file-name-extension (my-lines-from-command-output "git diff-tree --no-commit-id --name-only -r HEAD")))
           (untracked-files (my-lines-from-command-output "git --no-pager ls-files --others --exclude-standard"))
           (lookup-ext (make-hash-table :test #'equal))
           rlt)
      ;; file extensions of files in HEAD commit
      (dolist (ext exts)
        (puthash ext t lookup-ext))
      ;; If untracked file has same file extension as committed files
      ;; maybe they should be staged too?
      (dolist (file untracked-files)
        (when (gethash (file-name-extension file) lookup-ext)
          (push (file-name-nondirectory file) rlt)))
      (when rlt
        (message "Stage files? %s" (mapconcat 'identity rlt " "))))))

;;;###autoload
(defun my-lines-from-command-output (command)
  "Return lines of COMMAND output."
  (let* ((output (string-trim (shell-command-to-string command)))
         (cands (my-nonempty-lines output)))
    (delq nil (delete-dups cands))))


;;;###autoload
(defun my-git-gutter-toggle ()
  "Toggle git gutter."
  (interactive)
  (git-gutter-mode -1)
  ;; git-gutter-fringe doesn't seem to
  ;; clear the markup right away
  (sit-for 0.1)
  (git-gutter:clear))

;;;###autoload
(defun my-git-gutter-reset-to-default ()
  "Restore git gutter to its original status.
Show the diff between current working code and git head."
  (interactive)
  (git-gutter:set-start-revision nil)
  (message "git-gutter reset"))

(my-run-with-idle-timer 2 #'global-git-gutter-mode)

(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)
(global-set-key (kbd "C-x v =") 'git-gutter:popup-hunk)
;; Stage current hunk
(global-set-key (kbd "C-x v s") 'git-gutter:stage-hunk)
;; Revert current hunk
(global-set-key (kbd "C-x v r") 'git-gutter:revert-hunk)

;; }}

;;;###autoload
(defun my-git-commit-id ()
  "Select commit id from current branch."
  (let* ((git-cmd "git --no-pager log --date=short --pretty=format:'%h|%ad|%s|%an'")
         (collection (my-nonempty-lines (shell-command-to-string git-cmd)))
         (item (completing-read "git log:" collection)))
    (when item
      (car (split-string item "|" t)))))

;;;###autoload
(defun my-nonempty-lines (str)
  "Split STR into lines."
  (split-string str "[\r\n]+" t))
        
;;;###autoload
(defun my-git-show-commit-internal ()
  "Show git commit."
  (let* ((id (my-git-commit-id)))
    (when id
      (shell-command-to-string (format "git show %s" id)))))

;;;###autoload
(defun my-git-show-commit ()
  "Show commit using ffip."
  (interactive)
  (let* ((ffip-diff-backends '(("Show git commit" . my-git-show-commit-internal))))
    (ffip-show-diff 0)))


;;;###autoload
(defun git-get-current-file-relative-path ()
  "Get relative path of current file for Git."
  (replace-regexp-in-string (concat "^" (file-name-as-directory default-directory))
                            ""
                            buffer-file-name))

;;;###autoload
(defun git-checkout-current-file ()
  "Git checkout current file."
  (interactive)
  (when (and (buffer-file-name)
             (yes-or-no-p (format "git checkout %s?"
                                  (file-name-nondirectory (buffer-file-name)))))
    (let* ((filename (git-get-current-file-relative-path)))
      (shell-command (concat "git checkout " filename))
      (message "DONE! git checkout %s" filename))))

(defvar git-commit-message-history nil)
;;;###autoload
(defun git-commit-tracked ()
  "Run 'git add -u' and commit."
  (interactive)
  (let* ((hint "Commit tracked files. Please input commit message (Enter to abort):")
         (msg (read-from-minibuffer hint
                                    nil
                                    nil
                                    nil
                                    'git-commit-message-history)))
    (cond
     ((and msg (> (length msg) 3))
      (shell-command "git add -u")
      (shell-command (format "git commit -m \"%s\"" msg))
      (message "Tracked files is committed."))
     (t
      (message "Do nothing!")))))
;;;###autoload
(defun git-add-current-file ()
  "Git add file of current buffer."
  (interactive)
  (when buffer-file-name
    (let* ((filename (git-get-current-file-relative-path))
           (head-info (shell-command-to-string
                       "git log --pretty=format:'%h %s (%an)' --date=short -n1
")))
      (shell-command (concat "git add " filename))
      (message "%s added. HEAD: %s" filename head-info))))

;; {{ look up merge conflict
(defvar my-goto-merge-conflict-fns
  '(("n" my-next-merge-conflict)
    ("p" my-prev-merge-conflict)))
;;;###autoload
(defun my-goto-merge-conflict-internal (forward-p)
  "Goto specific hunk.  If FORWARD-P is t, go in forward direction."
  ;; @see https://emacs.stackexchange.com/questions/63413/finding-git-conflict-in-the-same-buffer-if-cursor-is-at-end-of-the-buffer#63414
  (my-ensure 'smerge-mode)
  (let ((buffer (current-buffer))
        (hunk-fn (if forward-p 'smerge-next 'smerge-prev)))
    (unless (funcall hunk-fn)
      (vc-find-conflicted-file)
      (when (eq buffer (current-buffer))
        (let ((prev-pos (point)))
          (goto-char (if forward-p (point-min) (1- (point-max))))
          (unless (funcall hunk-fn)
            (goto-char prev-pos)
            (message "No conflicts found")))))))

;;;###autoload
(defun my-next-merge-conflict ()
  "Go to next merge conflict."
  (interactive)
  (my-goto-merge-conflict-internal t))
;;;###autoload
(defun my-prev-merge-conflict ()
  "Go to previous merge conflict."
  (interactive)
  (my-goto-merge-conflict-internal nil))

;;;###autoload
(defun my-search-next-merge-conflict ()
  "Search next merge conflict."
  (interactive)
  (my-setup-extra-keymap my-goto-merge-conflict-fns
                         "Goto merge conflict: [n]ext [p]revious [q]uit"
                         'my-goto-merge-conflict-internal
                         t))
;;;###autoload
(defun my-search-prev-merge-conflict ()
  "Search previous merge conflict."
  (interactive)
  (my-setup-extra-keymap my-goto-merge-conflict-fns
                         "Goto merge conflict: [n]ext [p]revious [q]uit"
                         'my-goto-merge-conflict-internal
                         nil))
;; }}

;; {{ look up diff hunk
(defvar my-goto-diff-hunk-fns
  '(("n" diff-hunk-next)
    ("p" diff-hunk-prev)))
;;;###autoload
(defun my-search-next-diff-hunk ()
  "Search next diff hunk."
  (interactive)
  (my-setup-extra-keymap my-goto-diff-hunk-fns
                         "Goto diff hunk: [n]ext [p]revious [q]uit"
                         'diff-hunk-next))
;;;###autoload
(defun my-search-prev-diff-hunk ()
  "Search previous diff hunk."
  (interactive)
  (my-setup-extra-keymap my-goto-diff-hunk-fns
                         "Goto diff hunk: [n]ext [p]revious [q]uit"
                         'diff-hunk-prev))
;; }}

;; {{
;;;###autoload
(defun my-git-extract-based (target lines)
  "Extract based version from TARGET and LINES."
  (let* (based (i 0) break)
    (while (and (not break) (< i (length lines)))
      (cond
       ((string-match (regexp-quote target) (nth i lines))
        (setq break t))
       (t
        (setq i (1+ i)))))
    ;; find child of target commit
    (when (and (< 0 i)
               (< i (length lines)))
      (setq based
            (replace-regexp-in-string "^tag: +"
                                      ""
                                      (car (split-string (nth (1- i) lines)
                                                         " +")))))
    based))
;;;###autoload
(defun my-git-rebase-interactive (&optional user-select-branch)
  "Rebase interactively on the closest branch or tag in git log output.
If USER-SELECT-BRANCH is not nil, rebase on the tag or branch selected by user."
  (interactive "P")
  (let* ((cmd "git --no-pager log --decorate --oneline -n 1024")
         (lines (my-lines-from-command-output cmd))
         (targets (delq nil
                        (mapcar (lambda (e)
                                  (when (and (string-match "^[a-z0-9]+ (\\([^()]+\\)) " e)
                                             (not (string-match "^[a-z0-9]+ (HEAD " e)))
                                    (match-string 1 e)))
                                lines)))
         based)
    (cond
     ((or (not targets) (null targets))
      (message "No tag or branch is found to base on."))
     ((or (not user-select-branch) (eq (length targets) 1))
      ;; select the closest/only tag or branch
      (setq based (my-git-extract-based (nth 0 targets) lines)))
     (t
      ;; select the one tag or branch
      (setq based (my-git-extract-based (completing-read "Select based: " targets)
                                        lines))))

    ;; start git rebase
    (when based
      (magit-rebase-interactive based nil))))
;; }}
;;;###autoload
(defun my-git-cherry-pick-from-reflog ()
  "Cherry pick a commit from git reflog."
  (interactive)
  (let* ((cmd "git --no-pager reflog --date=short")
         (lines (my-lines-from-command-output cmd))
         (selected (completing-read "Commit to cherry pick:" lines))
         (commit-id (and selected (car (split-string selected)))))
    (when commit-id
      (my-ensure 'magit)
      (magit-cherry-copy commit-id))))

;; {{ git-gutter use ivy
;;;###autoload
(defun my-git-reshape-gutter (gutter)
  "Re-shape GUTTER for `ivy-read'."
  (let* ((linenum-start (aref gutter 3))
         (linenum-end (aref gutter 4))
         (target-line "")
         (target-linenum 1)
         (tmp-line "")
         (max-line-length 0))
    (save-excursion
      (while (<= linenum-start linenum-end)
        (my-goto-line linenum-start)
        (setq tmp-line (replace-regexp-in-string "^[ \t]*" ""
                                                 (buffer-substring (line-beginning-position)
                                                                   (line-end-position))))
        (when (> (length tmp-line) max-line-length)
          (setq target-linenum linenum-start)
          (setq target-line tmp-line)
          (setq max-line-length (length tmp-line)))

        (setq linenum-start (1+ linenum-start))))
    ;; build (key . linenum-start)
    (cons (format "%s %d: %s"
                  (if (eq 'deleted (aref gutter 1)) "-" "+")
                  target-linenum target-line)
          target-linenum)))
;;;###autoload
(defun my-git-goto-gutter ()
  "Go to specific git gutter."
  (interactive)
  (if git-gutter:diffinfos
      (ivy-read "git-gutters:"
                (mapcar 'my-git-reshape-gutter git-gutter:diffinfos)
                :action (lambda (e)
                          (unless (numberp e) (setq e (cdr e)))
                          (my-goto-line e)))
    (message "NO git-gutters!")))

;; }}
;;;###autoload
(defun my-git-find-file-in-commit (&optional level)
  "Find file in previous commit with LEVEL.
If LEVEL > 0, find file in previous LEVEL commit."
  (interactive "P")
  (my-ensure 'magit)
  (let* ((rev (concat "HEAD" (if (and level (> level 0)) (make-string level ?^))))
         (pretty (string-trim (shell-command-to-string (format "git --no-pager log %s --oneline --no-walk" rev))))
         (prompt (format "Find file from commit [%s]: " pretty))
         (cmd (my-git-files-in-rev-command rev level))
         (default-directory (my-git-root-dir))
         (file (completing-read prompt (my-lines-from-command-output cmd))))
    (when file
      (find-file file))))
;;;###autoload
(defun my-git-root-dir ()
  "Git root directory."
  ;;(interactive)
  (locate-dominating-file default-directory ".git"))

;;;###autoload
(defun my-git-files-in-rev-command (rev level)
  "Return git command line to show files in REV and LEVEL."
  (unless level (setq level 0))
  (concat "git diff-tree --no-commit-id --name-only -r "
          rev
          (make-string level ?^)))
;;;###autoload            
(defun my-git-commit-create ()
  "Git commit."
  (interactive)
  (let ((msg (read-string "Git commit message: ")))
    (when (> (length msg) 0)
      (shell-command (format "git commit --no-verify -m \"%s\"" msg)))))
;;;###autoload
(defun my-git-commit-amend (&optional reuse-p)
  "Git amend.  If REUSE-P is t, commit by reusing original message."
  (interactive)
  (let* ((original-msg  (shell-command-to-string "git log --pretty=format:'%s' -n1"))
         msg
         (extra-args (if reuse-p "--reuse-message=HEAD" ""))
         cmd)

    (setq msg (unless reuse-p
                (read-string "Git amend message: " original-msg)))
    (when (or reuse-p (> (length msg) 0))
      (setq cmd (format "git commit --no-verify --amend %s" extra-args))
      (unless reuse-p
        (setq cmd (format "%s -m \"%s\"" cmd msg)))
      (shell-command cmd))))
;;;###autoload
(defun my-git-current-branch ()
  "Show current branch name."
  (interactive)
  (message "Git current branch: %s"
           (string-trim (shell-command-to-string "git branch --show-current"))))



(with-eval-after-load 'hydra
    ;; {{ git-gutter, @see https://github.com/abo-abo/hydra/wiki/Git-gutter
    (defhydra my-hydra-git (:body-pre
                            (progn
                              (git-gutter-mode 1)
                              (setq git-link-use-commit t))
                            :after-exit (setq git-link-use-commit nil)
                            :color blue)
      "
Git:
[_dd_] Diff               [_ri_] Rebase closest
[_dc_] Diff staged        [_s_] Show commit
[_dr_] Diff range         [_rr_] Reset gutter
[_au_] Add modified       [_rh_] Gutter => HEAD
[_cc_] Commit             [_l_] Log selected/file
[_ca_] Amend              [_b_] Branches
[_ja_] Amend silent       [_k_] Git commit link
[_tt_] Stash              [_Q_] Quit gutter
[_ta_] Apply stash        [_cr_] Cherry pick from reflog
[_f_] Find file in commit

"
      ("ri" my-git-rebase-interactive)
      ("rr" my-git-gutter-reset-to-default)
      ("rh" my-git-gutter-reset-to-head-parent)
      ("cb" my-git-current-branch)
      ("s" my-git-show-commit)
      ("l" magit-log-buffer-file)
      ("b" magit-show-refs)
      ("k" git-link)
      ("g" magit-status)
      ("ta" magit-stash-apply)
      ("tt" magit-stash)
      ("dd" magit-diff-dwim)
      ("dc" magit-diff-staged)
      ("dr" (magit-diff-range (my-git-commit-id)))
      ("cc" magit-commit-create)
      ("ca" magit-commit-amend)
      ("nn" my-git-commit-create)
      ("na" my-git-commit-amend)
      ("ja" (my-git-commit-amend t))
      ("au" magit-stage-modified)
      ("Q" my-git-gutter-toggle)
      ("f" my-git-find-file-in-commit)
      ("cr" my-git-cherry-pick-from-reflog)
      ("q" nil))
    (global-set-key (kbd "C-c C-g") 'my-hydra-git/body)
    )
  ;; }}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-vcs.el ends here
