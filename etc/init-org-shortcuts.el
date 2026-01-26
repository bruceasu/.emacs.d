(provide 'init-org-shortcuts)

(require 'basic-function)

(defvar org-files-directory
  (expand-file-name "org"
                    (if (boundp 'user-home-dir)
                        user-home-dir
                      user-emacs-directory))
  "Directory that stores Org files.")

(defvar suk-emacs-config-dir
  (expand-file-name "etc" user-emacs-directory)
  "Directory that stores Org-related helper files when not launched from init.el.")

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

;;; --- 笔记管理和组织
(define-prefix-command 'F9-map)
(global-set-key (kbd "<f9>") 'F9-map)
(if (fboundp 'lazy-load-set-keys)
    (lazy-load-set-keys
     '(("a" . org-agenda)
       ("A" . org-attach)
       ("s" . show-org-agenda)
       ("c" . org-capture)
       ("i" . org-toggle-inline-images)
       ("l" . org-toggle-link-display)
       ("d" . calendar)
       ("r" . my-refile-map)
       ("h" . hydra-global-org/body))
     nil
     "<f9>")
  (define-key F9-map (kbd "a") #'org-agenda)
  (define-key F9-map (kbd "A") #'org-attach)
  (define-key F9-map (kbd "s") #'show-org-agenda)
  (define-key F9-map (kbd "c") #'org-capture)
  (define-key F9-map (kbd "i") #'org-toggle-inline-images)
  (define-key F9-map (kbd "l") #'org-toggle-link-display)
  (define-key F9-map (kbd "d") #'calendar)
  (define-key F9-map (kbd "r") #'my-refile-map)
  (define-key F9-map (kbd "h") #'hydra-global-org/body))
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
