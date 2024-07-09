;;; init.el --- Initialize configurations.  -*- lexical-binding: t -*-
;; Copyright (C) 1999 - 2024 Suk
;; Author: Suk

;; This file is not part of GNU Emacs.
;;

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;; Commentary
;;
;; Emacs configurations.
;;

;;; Code:

(provide 'init)


;; Dingyi yidit muluk, fongbin yathòu cingyi.
;; user-emacs-directory tungsöng hai ~/.emacs.d
;; windows há, ~/ tungsöng hai $EMACS_INSTALL_DIR, wákze EMACS citding de `HOME` binlöng
(defvar suk-emacs-root-dir (file-truename user-emacs-directory))
(defvar suk-emacs-config-dir (expand-file-name "etc" suk-emacs-root-dir))
(defvar suk-emacs-extension-dir (expand-file-name "extensions" suk-emacs-root-dir))
(defvar suk-emacs-share-dir (expand-file-name "share" suk-emacs-root-dir))
(defvar suk-emacs-themes-dir (expand-file-name "themes" suk-emacs-share-dir))
(defvar suk-emacs-elpa-dir (expand-file-name "elpa" suk-emacs-root-dir))
(defvar suk-emacs-var-dir (expand-file-name "var" suk-emacs-root-dir))
(defvar suk-emacs-tmp-dir (expand-file-name "tmp" suk-emacs-var-dir))
(defvar suk-emacs-backup-dir (expand-file-name "backup" suk-emacs-tmp-dir))

;; OS ge HOME muluk.
(defvar user-home-dir (getenv "HOME"))

(if (eq system-type 'windows-nt)
    (defvar user-home-dir (getenv "USERPROFILE")))

;; blink search
(setq blink-search-db-path (expand-file-name "blink-search.db" suk-emacs-tmp-dir))
;; Saveplace
(setq save-place-file (concat suk-emacs-var-dir "/saveplace"))
;; Recentf
(setq recentf-save-file (concat suk-emacs-var-dir "/recentf"))
;;(setq recentf-save-file "~/.emacs.d/var/recentf")
;; History
(setq savehist-file (concat suk-emacs-var-dir "/history"))
; Amx
(setq amx-save-file (concat suk-emacs-var-dir "/amx-items"))
;; Auto save
(setq auto-save-list-file-prefix (concat suk-emacs-var-dir "/auto-save-list/.saves-"))
;; Eshell
(setq eshell-directory-name (concat suk-emacs-var-dir "/eschell"))
(setq eshell-history-file-name (concat eshell-directory-name "/history"))
;; projectitle-bookmarks
(setq projectile-known-projects-file (concat suk-emacs-var-dir "/projectile-bookmarks.eld"))
(setq backup-directory-alist `(("" . ,suk-emacs-tmp-dir)))
;; Bookmark
(setq bookmark-default-file (concat suk-emacs-var-dir "/emacs.bmk"))
;; Diary
(setq diary-file (concat user-home-dir "/diary"))

;; Ignore `cl` expiration warnings
(setq byte-compile-warnings '(cl-function))

;; original version
;;(defun add-subdirs-to-load-path (dir)
;;  "Recursive add directories to `load-path'."
;;  (let ((default-directory (file-name-as-directory dir)))
;;     (add-to-list 'load-path dir)
;;     (normal-top-level-add-subdirs-to-load-path)))

;; 王勇的版本 https://manateelazycat.github.io/emacs/2022/03/02/emacs-load-directory-recursively.html
(require 'cl-lib)
(defun add-subdirs-to-load-path (search-dir isFirst)
  (interactive)
  (when isFirst
    ;; 原来的版本没有把第1个 search-dir 本身添加到 `load path`
    ;; 递归时的search-dir是在递归前加入了。
    (add-to-list 'load-path search-dir))
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; golöi bat bityiu ge mukluk, taising Emacs kaidung cudou.
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; m hai mangin
                   (not (file-directory-p (concat dir subdir)))
                   ;; yicöi hámin ge mukluk
                   (member subdir '("." ".." ; Linux/Uniux haitung ge  dongcin mukluk tungmái fu mukluk
                                    "dist" "node_modules" "__pycache__" ; takding ge yüyin söngģán ge mukluk
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github")))) ; bánbun hungjai mukluk
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; mukluk bauhám  .el .so .dll ge mangin di louging sinji gá dou `load-path` binlöng
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非Elisp语言编写的Emacs动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; jüyi: add-to-list ge daisám go cámsou bitsöiwai t, timgá dou meibou,
          ;; kokbou ģongdou yausin
          (add-to-list 'load-path subdir-path t))

        ;; geiöuk daiģai sausok ji mukluk.
        (add-subdirs-to-load-path subdir-path nil)))))

;; gázoi tsiding ge muluk.
(add-subdirs-to-load-path suk-emacs-config-dir t)
(add-subdirs-to-load-path suk-emacs-extension-dir t)
(add-subdirs-to-load-path suk-emacs-themes-dir t)

;; set const
(defconst custom-template-file
  (expand-file-name "custom-template.el" user-emacs-directory)
  "Custom template file of Suk's Emacs.")

(defconst suk-homepage
  "https://github.com/bruceasu/.emacs.d"
  "The Github page of this Emacs config.")

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(defconst emacs/>=30p
  (>= emacs-major-version 30)
  "Emacs is 30 or above.")


(defgroup suk nil
  "suk Emacs customizations."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/bruceasu/.emacs.d"))

(defcustom suk-logo (expand-file-name
                         (if (display-graphic-p) "logo.png" "banner.txt")
                         user-emacs-directory)
  "Set Suk logo. nil means official logo."
  :group 'suk
  :type 'string)

(defcustom suk-full-name "Suk"
  "Set user full name."
  :group 'suk
  :type 'string)

(defcustom suk-mail-address "bruceasu@gmail.com"
  "Set user email address."
  :group 'suk
  :type 'string)

(defcustom suk-proxy "127.0.0.1:1080"
  "Set network proxy."
  :group 'suk
  :type 'string)

(defcustom suk-cnfonts nil
  "Use cnfonts or not."
  :group 'suk
  :type 'boolean)

(defcustom user-home-dir (getenv "HOME")
  "User home directory."
  :group 'suk
  :type 'string)

(if sys/win32p
    (setq user-home-dir (getenv "USERPROFILE"))
)

(defcustom suk-icon t
  "Display icons or not."
  :group 'suk
  :type 'boolean)

(defcustom org-roam-directory (expand-file-name "RoamNotes" user-home-dir)
  "The org roam directory."
  :group 'suk
  :type 'string)

(defcustom org-files-directory (expand-file-name "org" user-home-dir)
  "The org roam directory."
  :group 'suk
  :type 'string)

(defcustom  org-css-file "~/.emacs.d/share/my-org-style-min.css"
  "The org css style file."
  :group 'suk
  :type 'string)

(defcustom windows-bash-path (expand-file-name "C:/Program Files/Git/bin/bash.exe")
  "The windows version of bash."
  :group 'suk
  :type 'string)


(defcustom suk-completion-style 'childframe
  "Completion display style."
  :group 'suk
  :type '(choice (const :tag "Minibuffer" minibuffer)
                 (const :tag "Child Frame" childframe)))

(defcustom suk-dashboard (not (daemonp))
  "Display dashboard at startup or not.
If Non-nil, use dashboard, otherwise will restore previous session."
  :group 'suk
  :type 'boolean)

(defcustom suk-lsp 'eglot
  "Set language server.

`lsp-mode': See https://github.com/emacs-lsp/lsp-mode.
`eglot': See https://github.com/joaotavora/eglot.
nil means disabled."
  :group 'suk
  :type '(choice (const :tag "LSP Mode" lsp-mode)
                 (const :tag "Eglot" eglot)
                 (const :tag "Disable" nil)))

(defcustom suk-tree-sitter t
  "Enable tree-sitter or not.
Native tree-sitter is introduced in 29."
  :group 'suk
  :type 'boolean)

(defcustom suk-lsp-format-on-save nil
  "Auto format buffers on save."
  :group 'suk
  :type 'boolean)

(defcustom suk-lsp-format-on-save-ignore-modes
  '(c-mode c++-mode python-mode markdown-mode)
  "The modes that don't auto format and organize imports while saving the buffers.
`prog-mode' means ignoring all derived modes."
  :group 'suk
  :type '(repeat (symbol :tag "Major-Mode")))

(defcustom suk-prettify-symbols-alist
  '(("lambda" . ?λ)
    ("<-"     . ?←)
    ("->"     . ?→)
    ("->>"    . ?↠)
    ("=>"     . ?⇒)
    ("map"    . ?↦)
    ("/="     . ?≠)
    ("!="     . ?≠)
    ("=="     . ?≡)
    ("<="     . ?≤)
    (">="     . ?≥)
    ("=<<"    . (?= (Br . Bl) ?≪))
    (">>="    . (?≫ (Br . Bl) ?=))
    ("<=<"    . ?↢)
    (">=>"    . ?↣)
    ("&&"     . ?∧)
    ("||"     . ?∨)
    ("not"    . ?¬))
  "A list of symbol prettifications.
Nil to use font supports ligatures."
  :group 'suk
  :type '(alist :key-type string :value-type (choice character sexp)))

(defcustom suk-prettify-org-symbols-alist
  '(("[ ]"            . ?)
    ("[-]"            . ?)
    ("[X]"            . ?)

    (":PROPERTIES:"   . ?)
    (":ID:"           . ?🪪)
    (":END:"          . ?🔚)

    ("#+ARCHIVE:"     . ?📦)
    ("#+AUTHOR:"      . ?👤)
    ("#+CREATOR:"     . ?💁)
    ("#+DATE:"        . ?📆)
    ("#+DESCRIPTION:" . ?⸙)
    ("#+EMAIL:"       . ?📧)
    ("#+HEADERS"      . ?☰)
    ("#+OPTIONS:"     . ?⚙)
    ("#+SETUPFILE:"   . ?⚒)
    ("#+TAGS:"        . ?🏷)
    ("#+TITLE:"       . ?📓)

    ("#+BEGIN_SRC"    . ?✎)
    ("#+END_SRC"      . ?□)
    ("#+BEGIN_QUOTE"  . ?«)
    ("#+END_QUOTE"    . ?»)
    ("#+RESULTS:"     . ?💻))
  "A list of symbol prettifications for `org-mode'."
  :group 'suk
  :type '(alist :key-type string :value-type (choice character sexp)))


;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-template-file
       (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
           (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))

(if (file-exists-p custom-file)
    (load custom-file))

;; Load `custom-post.el'
;; Put personal configurations to override defaults here.
(add-hook 'after-init-hook
          (lambda ()
            (let ((file
                   (expand-file-name "custom-post.el" user-emacs-directory)))
              (if (file-exists-p file)
                  (load file)))))

  
;; The contents of the Emacs configuration file are written below.
(let (;;  Temporarily increase `gc-cons-threshold' when loading to speed up
      ;;  startup.
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.8)

      ;; Clear to avoid analyzing files when loading remote files.
      (file-name-handler-alist nil))
	  ;; Don't pass case-insensitive to `auto-mode-alist'
	  (setq auto-mode-case-fold nil)
	  ;; Prevent flashing of unstyled modeline at startup
	  (setq-default mode-line-format nil)
	  (unless (or (daemonp) noninteractive init-file-debug)
	    ;; Suppress file handlers operations at startup
	    ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
	    (let ((old-value file-name-handler-alist))
	      (setq file-name-handler-alist nil)
	      (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
	      (add-hook 'emacs-startup-hook
	                (lambda ()
	                  "Recover file name handlers."
	                  (setq file-name-handler-alist
	                        (delete-dups (append file-name-handler-alist old-value))))
	                101)))


	  
	(defun childframe-workable-p ()
	  "Whether childframe is workable."
	  (not (or noninteractive
	           emacs-basic-display
	           (not (display-graphic-p)))))

	(defun childframe-completion-workable-p ()
	  "Whether childframe completion is workable."
	  (and (eq suk-completion-style 'childframe)
	       (childframe-workable-p)))

	(defun icons-displayable-p ()
	  "Return non-nil if icons are displayable."
	  (and suk-icon
	       (or (featurep 'nerd-icons)
	           (require 'nerd-icons nil t))))

	(defun suk-treesit-available-p ()
	  "Check whether tree-sitter is available.
	Native tree-sitter is introduced since 29.1."
	  (and suk-tree-sitter
	       (fboundp 'treesit-available-p)
	       (treesit-available-p)))

	(defun too-long-file-p ()
	  "Check whether the file is too long."
	  (or (> (buffer-size) 100000)
	      (and (fboundp 'buffer-line-statistics)
	           (> (car (buffer-line-statistics)) 10000))))

	;; Increase selected region by semantic units
	(defun suk-treesit-available-p ()
	  "Check whether tree-sitter is available.
	Native tree-sitter is introduced since 29.1."
	  (and suk-tree-sitter
	       (fboundp 'treesit-available-p)
	       (treesit-available-p)))


	;; Browse URL
	;;;###autoload
	(defun suk-webkit-browse-url (url &optional pop-buffer new-session)
	  "Browse URL with xwidget-webkit' and switch or pop to the buffer.

	  POP-BUFFER specifies whether to pop to the buffer.
	  NEW-SESSION specifies whether to create a new xwidget-webkit session."
	  (interactive (progn
	                 (require 'browse-url)
	                 (browse-url-interactive-arg "xwidget-webkit URL: ")))
	  (or (featurep 'xwidget-internal)
	      (user-error "Your Emacs was not compiled with xwidgets support"))

	  (xwidget-webkit-browse-url url new-session)
	  (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
	    (when (buffer-live-p buf)
	      (and (eq buf (current-buffer)) (quit-window))
	      (if pop-buffer
	          (pop-to-buffer buf)
	        (switch-to-buffer buf)))))


	(defun my-ensure (feature)
	  "Make sure FEATURE is required."
	  (unless (featurep feature)
	    (condition-case nil
	        (require feature)
	      (error nil))))

	(when sys/linuxp
	;;;###autoload
	  (defun my-hostname ()
	    "Return stripped output of cli program hostname."
	    (let* ((output (shell-command-to-string "hostname")))
	      ;; Windows DOS might output some extra noise
	      (string-trim (replace-regexp-in-string "hostname" "" output))))

	  )
	;;;###autoload
	(defun run-cmd-and-replace-region (cmd)
	  "Run CMD in shell on selected region or current buffer.
	Then replace the region or buffer with cli output."
	  (let* ((orig-point (point))
	         (b (if (region-active-p) (region-beginning) (point-min)))
	         (e (if (region-active-p) (region-end) (point-max))))
	    (shell-command-on-region b e cmd nil t)
	    (goto-char orig-point)))


	;; {{ copied from http://ergoemacs.org/emacs/elisp_read_file_content.html
	;;;###autoload
	(defun my-get-string-from-file (file)
	  "Return FILE's content."
	  (with-temp-buffer
	    (insert-file-contents file)
	    (buffer-string)))
	;;;###autoload
	(defun my-read-lines (file)
	  "Return a list of lines of FILE."
	  (split-string (my-get-string-from-file file) "\n" t))
	;; }}

	;;;###autoload
	(defun my-write-to-file (str file)
	  "Write STR to FILE."
	  (with-temp-buffer
	    (insert str)
	    (write-file (file-truename file))))

	;;;###autoload
	(defun my-write-to-missing-file (str file)
	  "Write STR to FILE if it's missing."
	  (unless (file-exists-p file)
	    (my-write-to-file str file)))

	;;;###autoload
	;; Handier way to add modes to auto-mode-alist
	(defun my-add-auto-mode (mode &rest patterns)
	  "Add entries to `auto-mode-alist' to use MODE for given file PATTERNS."
	  (dolist (pattern patterns)
	    (push (cons pattern mode) auto-mode-alist)))

	;;;###autoload
	(defun my-add-interpreter-mode (mode &rest patterns)
	  "Add entries to `interpreter-mode-alist' to use MODE for given file PATTERNS."
	  (dolist (pattern patterns)
	    (push (cons pattern mode) interpreter-mode-alist )))

	;;;###autoload
	(defmacro my-push-if-uniq (item items)
	  "Push ITEM into ITEMS if it's unique."
	  `(unless (member ,item ,items) (push ,item ,items)))

	;;;###autoload
	(defun my-what-face (&optional position)
	  "Show all faces at POSITION."
	  (let* ((face (get-text-property (or position (point)) 'face)))
	    (unless (keywordp (car-safe face)) (list face))))



	;; String utilities missing from core emacs
	;;;###autoload
	(defun string-all-matches (regex str &optional group)
	  "Find matches for REGEX in STR, returning the full match or GROUP."
	  (let ((result nil)
	        (pos 0)
	        (group (or group 0)))
	    (while (string-match regex str pos)
	      (push (match-string group str) result)
	      (setq pos (match-end group)))
	    result))

	;;;###autoload
	(defun path-in-directory-p (file directory)
	  "FILE is in DIRECTORY."
	  (let* ((pattern (concat "^" (file-name-as-directory directory))))
	    (if (string-match pattern file) file)))

	;;;###autoload
	(defun my-prepare-candidate-fit-into-screen (s)
	  (let* ((w (frame-width))
	         ;; display kill ring item in one line
	         (key (replace-regexp-in-string "[ \t]*[\n\r]+[ \t]*" "\\\\n" s)))
	    ;; strip the whitespace
	    (setq key (replace-regexp-in-string "^[ \t]+" "" key))
	    ;; fit to the minibuffer width
	    (if (> (length key) w)
	        (setq key (concat (substring key 0 (- w 4)) "...")))
	    (cons key s)))

	;;;###autoload
	(defun my-select-from-kill-ring (fn)
	  "If N > 1, yank the Nth item in `kill-ring'.
	If N is nil, use `ivy-mode' to browse `kill-ring'."
	  (interactive "P")
	  (let* ((candidates (cl-remove-if
	                      (lambda (s)
	                        (or (< (length s) 5)
	                            (string-match "\\`[\n[:blank:]]+\\'" s)))
	                      (delete-dups kill-ring)))
	         (ivy-height (/ (frame-height) 2)))
	    (ivy-read "Browse `kill-ring':"
	              (mapcar #'my-prepare-candidate-fit-into-screen candidates)
	              :action fn)))

	;;;###autoload
	(defun my-delete-selected-region ()
	  "Delete selected region."
	  (when (region-active-p)
	    (delete-region (region-beginning) (region-end))))

	;;;###autoload
	(defun my-insert-str (str)
	  "Insert STR into current buffer."
	  ;; ivy8 or ivy9
	  (if (consp str) (setq str (cdr str)))
	  ;; evil-mode?
	  (if (and (functionp 'evil-normal-state-p)
	           (boundp 'evil-move-cursor-back)
	           (evil-normal-state-p)
	           (not (eolp))
	           (not (eobp)))
	      (forward-char))

	  (my-delete-selected-region)

	  ;; insert now
	  (insert str)
	  str)

	;;;###autoload
	(defun my-line-str (&optional line-end)
	  (buffer-substring-no-properties (line-beginning-position)
	                                  (if line-end line-end (line-end-position))))

	;;;###autoload
	(defun my-is-in-one-line (b e)
	  (save-excursion
	    (goto-char b)
	    (and (<= (line-beginning-position) b)
	         (<= e (line-end-position)))))

	;;;###autoload
	(defun my-buffer-str ()
	  (buffer-substring-no-properties (point-min) (point-max)))

	;;;###autoload
	(defun my-selected-str ()
	  "Get string of selected region."
	  (buffer-substring-no-properties (region-beginning) (region-end)))

	;;;###autoload
	(defun my-use-selected-string-or-ask (&optional hint)
	  "Use selected region or ask for input.
	If HINT is empty, use symbol at point."
	  (cond
	   ((region-active-p)
	    (my-selected-str))
	   ((or (not hint) (string= "" hint))
	    (thing-at-point 'symbol))
	   (t
	    (read-string hint))))

	;;;###autoload
	(defun delete-this-file ()
	  "Delete the current file, and kill the buffer."
	  (interactive)
	  (or (buffer-file-name) (error "No file is currently being edited"))
	  (when (yes-or-no-p (format "Really delete '%s'?"
	                             (file-name-nondirectory buffer-file-name)))
	    (delete-file (buffer-file-name))
	    (kill-this-buffer)))

	;;;###autoload
	(defun rename-this-file-and-buffer (new-name)
	  "Renames both current buffer and file it's visiting to NEW-NAME."
	  (interactive "sNew name: ")
	  (let ((name (buffer-name))
	        (filename (buffer-file-name)))
	    (unless filename
	      (error "Buffer '%s' is not visiting a file!" name))
	    (if (get-buffer new-name)
	        (message "A buffer named '%s' already exists!" new-name)
	      (progn
	        (rename-file filename new-name 1)
	        (rename-buffer new-name)
	        (set-visited-file-name new-name)
	        (set-buffer-modified-p nil)))))

	(defvar my-load-user-customized-major-mode-hook t)

	;;;###autoload
	(defun buffer-too-big-p ()
	  "Test if current buffer is too big."
	  ;; 5000 lines
	  (> (buffer-size) (* 5000 80)))

	;;;###autoload
	(defun my-file-too-big-p (file)
	  "Test if FILE is too big."
	  (> (nth 7 (file-attributes file))
	     (* 5000 64)))

	(defvar my-force-buffer-file-temp-p nil)
	;;;###autoload
	(defun my-buffer-file-temp-p ()
	  "If no file or a temp file or HTML file converted from org file."
	  (interactive)
	  (let* ((f (buffer-file-name)) (rlt t))
	    (cond
	     ((not my-load-user-customized-major-mode-hook)
	      (setq rlt t))

	     ((and (buffer-name) (string-match "\\* Org SRc" (buffer-name)))
	      ;; org-babel edit inline code block need calling hook
	      (setq rlt nil))

	     ((null f)
	      (setq rlt t))

	     ((string-match (concat "^" temporary-file-directory) f)
	      ;; file is create from temp directory
	      (setq rlt t))

	     ((and (string-match "\.html$" f)
	           (file-exists-p (replace-regexp-in-string "\.html$" ".org" f)))
	      ;; file is a html file exported from org-mode
	      (setq rlt t))

	     (my-force-buffer-file-temp-p
	      (setq rlt t))

	     (t
	      (setq rlt nil)))
	    rlt))


	(defvar my-mplayer-extra-opts ""
	  "Extra options for mplayer (ao or vo setup).
	For example, you can '(setq my-mplayer-extra-opts \"-fs -ao alsa -vo vdpau\")'.")

	;;;###autoload
	(defun my-guess-mplayer-path ()
	  "Guess cli program mplayer's path."
	  (let* ((program "mplayer")
	         (common-opts "-fs -quiet"))
	    (cond
	     (*is-a-mac*
	      (cond
	       ((executable-find "mplayer")
	        (setq program "mplayer"))
	       (t
	        (setq program "open")))

	      (setq program "mplayer"))

	     (sys/linuxp
	      (setq program "mplayer -stop-xscreensaver"))

	     (sys/cygwin
	      (if (file-executable-p "/cygdrive/c/mplayer/mplayer.exe")
	          (setq program "/cygdrive/c/mplayer/mplayer.exe")
	        (setq program "/cygdrive/d/mplayer/mplayer.exe")))

	     (sys/win32p
	      (cond
	       ((file-executable-p "c:/mplayer/mplayer.exe")
	        (setq program "c:/mplayer/mplayer.exe"))
	       ((file-executable-p "d:/mplayer/mplayer.exe")
	        (setq program "d:/mplayer/mplayer.exe"))
	       ((file-executable-p "c:/Program Files/mplayer/mplayer.exe")
	        (setq program "\"c:/Program Files/mplayer/mplayer.exe\""))
	       ((file-executable-p "d:/Program Files/mplayer/mplayer.exe")
	        (setq program "\"d:/Program Files/mplayer/mplayer.exe\""))
	       ((file-executable-p "c:/mpv/mpv.exe")
	        (setq program "c:/mpv/mpv.exe"))
	       ((file-executable-p "d:/mpv/mpv.exe")
	        (setq program "d:/mpv/mpv.exe"))
	       ((file-executable-p "c:/Program Files/mpv/mpv.exe")
	        (setq program "\"c:/Program Files/mpv/mpv.exe\""))
	       ((file-executable-p "d:/Program Files/mpv/mpv.exe")
	        (setq program "\"d:/Program Files/mpv/mpv.exe\""))
	       (t
	        (error "Can't find media player."))))


	     (t
	      (error "Can't find any media player!")))

	    (unless (string-match "mplayer" program)
	      (setq common-opts ""))
	    (format "%s %s %s" program common-opts my-mplayer-extra-opts)))

	;;;###autoload
	(defun my-guess-image-viewer-path (image &optional stream-p)
	  "How to open IMAGE which could be STREAM-P."
	  (cond
	   (sys/macp
	    (format "open %s &" image))

	   (sys/linuxp
	    (if stream-p (format "curl -L %s | feh -F - &" image)
	      (format "feh -F %s &" image)))

	   (sys/cygwin
	    "feh -F")

	   (t ; windows
	    (format "rundll32.exe %s\\\\System32\\\\\shimgvw.dll, ImageView_Fullscreen %s &"
	            (getenv "SystemRoot")
	            image))))

	;;;###autoload
	(defun my-gclip ()
	  "Get clipboard content."
	  (let (powershell-program)
	    (cond
	     ;; Windows
	     ((and *win64* (fboundp 'w32-get-clipboard-data))
	      ;; `w32-set-clipboard-data' makes `w32-get-clipboard-data' always return null
	      (w32-get-clipboard-data))

	     ;; Windows 10
	     ((and sys/win32p (setq powershell-program (executable-find "powershell.exe")))
	      (string-trim-right
	       (with-output-to-string
	         (with-current-buffer standard-output
	           (call-process powershell-program nil t nil "-command" "Get-Clipboard")))))

	     (sys/cygwinp
	      (string-trim-right (shell-command-to-string "cat /dev/clipboard")))

	     ;; xclip can handle
	     (t
	      (xclip-get-selection 'clipboard)))))

	(defvar my-ssh-client-user nil
	  "User name of ssh client.")

	;;;###autoload
	(defun my-send-string-to-cli-stdin (string program)
	  "Send STRING to cli PROGRAM's stdin."
	  (with-temp-buffer
	    (insert string)
	    (call-process-region (point-min) (point-max) program)))

	;;;###autoload
	(defun my-write-string-to-file (string file)
	  "Write STRING to FILE."
	  (with-temp-buffer
	    (insert string)
	    (write-region (point-min) (point-max) file)))

	;;;###autoload
	(defun my-pclip (str-val)
	  "Put STR-VAL into clipboard."
	  (let* (win64-clip-program
	         ssh-client)
	    (cond
	     ;; Windows 10
	     ((and sys/win32p (setq win64-clip-program (executable-find "clip.exe")))
	      (my-send-string-to-cli-stdin str-val win64-clip-program))

	     ;; Windows
	     ((and sys/win32p (fboundp 'w32-set-clipboard-data))
	      ;; Don't know why, but on Windows 7 this API does not work.
	      (w32-set-clipboard-data str-val))

	     ;; Cygwin
	     (sys/cygwinp
	      (my-write-string-to-file str-val "/dev/clipboard"))

	     ;; If Emacs is inside an ssh session, place the clipboard content
	     ;; into "~/.tmp-clipboard" and send it back into ssh client
	     ;; Make sure you already set up ssh correctly.
	     ;; Only enabled if ssh server is macOS
	     ((and (setq ssh-client (getenv "SSH_CLIENT"))
	           (not (string= ssh-client ""))
	           *is-a-mac*)
	      (let* ((file "~/.tmp-clipboard")
	             (ip (car (split-string ssh-client "[ \t]+")))
	             (cmd (format "scp %s %s@%s:~/" file my-ssh-client-user ip)))
	        (when my-ssh-client-user
	          (my-write-to-file str-val file)
	          (shell-command cmd)
	          ;; clean up
	          (delete-file file))))

	     ;; xclip can handle
	     (t
	      (xclip-set-selection 'clipboard str-val)))))
	;; }}


	;;;###autoload
	(defun my-should-use-minimum-resource ()
	  "Use minimum resource (no highlight or line number)."
	  (and buffer-file-name
	       (string-match "\.\\(mock\\|min\\|bundle\\)\.js" buffer-file-name)))

	;;;###autoload
	(defun my-async-shell-command (command)
	  "Execute string COMMAND asynchronously."
	  (let* ((proc (start-process "Shell"
	                              nil
	                              shell-file-name
	                              shell-command-switch command)))
	    (set-process-sentinel proc `(lambda (process signal)
	                                  (let* ((status (process-status process)))
	                                    (when (memq status '(exit signal))
	                                      (unless (string= (substring signal 0 -1) "finished")
	                                        (message "Failed to run \"%s\"." ,command))))))))

	;;;###autoload
	(defun my-org-babel-execute:python-hack (orig-func &rest args)
	  ;; @see https://github.com/Liu233w/.spacemacs.d/issues/6
	  (let ((coding-system-for-write 'utf-8))
	    (apply orig-func args)))

	;;;###autoload
	(defun my-setup-language-and-encode (language-name coding-system)
	  "Set up LANGUAGE-NAME and CODING-SYSTEM at Windows.
	For example,
	- \"English\" and 'utf-16-le
	- \"Chinese-GBK\" and 'gbk"
	  (cond
	   ((eq system-type 'windows-nt)
	    (set-language-environment language-name)
	    (prefer-coding-system 'utf-8)
	    (set-terminal-coding-system coding-system)

	    (modify-coding-system-alist 'process "*" coding-system)

	    (advice-add 'org-babel-execute:python :around #'my-org-babel-execute:python-hack))

	   (t
	    (set-language-environment "UTF-8")
	    (prefer-coding-system 'utf-8))))
	;; }}

	;;;###autoload
	(defun my-skip-white-space (start step)
	  "Skip white spaces from START, return position of first non-space character.
	If STEP is 1,  search in forward direction, or else in backward direction."
	  (let* ((b start)
	         (e (if (> step 0) (line-end-position) (line-beginning-position))))
	    (save-excursion
	      (goto-char b)
	      (while (and (not (eq b e)) (memq (following-char) '(9 32)))
	        (forward-char step))
	      (point))))

	;;;###autoload
	(defun my-comint-current-input-region ()
	  "Region of current shell input."
	  (cons (process-mark (get-buffer-process (current-buffer)))
	        (line-end-position)))
	;;;###autoload
	(defun my-comint-kill-current-input ()
	  "Kill current input in shell."
	  (interactive)
	  (let* ((region (my-comint-current-input-region)))
	    (kill-region (car region) (cdr region))))
	;;;###autoload
	(defun my-comint-current-input ()
	  "Get current input in shell."
	  (let* ((region (my-comint-current-input-region)))
	    (string-trim (buffer-substring-no-properties (car region) (cdr region)))))

	;;;###autoload
	(defun my-rescan-imenu-items (&optional index-function)
	  "Get imenu items using INDEX-FUNCTION."
	  (my-ensure 'imenu)
	  (let* ((imenu-auto-rescan t)
	         (imenu-create-index-function (or index-function imenu-create-index-function))
	         (imenu-auto-rescan-maxout (buffer-size))
	         (items (imenu--make-index-alist t)))
	    (delete (assoc "*Rescan*" items) items)))

	;;;###autoload
	(defun my-create-range (&optional inclusive)
	  "Return range by font face.
	Copied from 3rd party package evil-textobj."
	  (let* ((point-face (my-what-face))
	         (pos (point))
	         (backward-none-space-point pos) ; last none white space char
	         (forward-none-space-point pos) ; last none white space char
	         (start pos)
	         (end pos))

	    ;; check chars backward,
	    ;; stop when char is not white space and has different face
	    (save-excursion
	      (let ((continue t))
	        (while (and continue (>= (- (point) 1) (point-min)))
	          (backward-char)
	          (unless (= 32 (char-after))
	            (if (equal point-face (my-what-face))
	                (setq backward-none-space-point (point))
	              (setq continue nil))))))

	    ;; check chars forward,
	    ;; stop when char is not white space and has different face
	    (save-excursion
	      (let ((continue t))
	        (while (and continue (< (+ (point) 1) (point-max)))
	          (forward-char)
	          (let ((forward-point-face (my-what-face)))
	            (unless (= 32 (char-after))
	              (if (equal point-face forward-point-face)
	                  (setq forward-none-space-point (point))
	                (setq continue nil)))))))

	    (cond
	     (inclusive
	      (setq start backward-none-space-point)
	      (setq end forward-none-space-point))
	     (t
	      (setq start (1+ backward-none-space-point))
	      (setq end (1- forward-none-space-point))))

	    (cons start (1+ end))))
	;;;###autoload
	(defun my-get-char (position)
	  "Get character at POSITION."
	  (save-excursion
	    (goto-char position)
	    (following-char)))
	;;;###autoload
	(defun my-extended-regexp (str)
	  "Build regex compatible with pinyin from STR."
	  (let* ((len (length str))
	         bn)
	    (cond
	     ;; do nothing
	     ((<= (length str) 1))

	     ;; If the first character of input in ivy is ":" or ";",
	     ;; remaining input is converted into Chinese pinyin regex.
	     ((or (and (string-match "[:\|;]" (substring str 0 1))
	               (setq str (substring str 1 len)))
	          (and (setq bn (buffer-name))
	               (or (member bn '("*Org Agenda*"))
	                   (string-match ".*EMMS Playlist\\|\\.org$" bn))))
	      (my-ensure 'pinyinlib)
	      (setq str (pinyinlib-build-regexp-string str)))

	     ;; If the first character of input in ivy is "/",
	     ;; remaining input is converted to pattern to search camel case word
	     ;; For example, input "/ic" match "isController" or "isCollapsed"
	     ((string= (substring str 0 1) "/")
	      (let* ((rlt "")
	             (i 0)
	             (subs (substring str 1 len))
	             c)
	        (when (> len 2)
	          (setq subs (upcase subs))
	          (while (< i (length subs))
	            (setq c (elt subs i))
	            (setq rlt (concat rlt (cond
	                                   ((and (< c ?a) (> c ?z) (< c ?A) (> c ?Z))
	                                    (format "%c" c))
	                                   (t
	                                    (concat (if (= i 0) (format "[%c%c]" (+ c 32) c)
	                                              (format "%c" c))
	                                            "[a-z]+")))))
	            (setq i (1+ i))))
	        (setq str rlt))))
	    str))


	(defvar my-disable-idle-timer (daemonp)
	  "Function passed to `my-run-with-idle-timer' is run immediately.")
	(defun my-run-with-idle-timer (seconds func)
	  "After SECONDS, run function FUNC once."
	  (cond
	   (my-disable-idle-timer
	    (funcall func))
	   (t
	    (run-with-idle-timer seconds nil func))))

	;;;###autoload
	(defun my-imenu-item-position (item)
	  "Handle some strange imenu ITEM."
	  (if (markerp item) (marker-position item) item))
	;;;###autoload
	(defun my-closest-imenu-item-internal (cands)
	  "Return closest imenu item from CANDS."
	  (let* ((pos (point))
	         closest)
	    (dolist (c cands)
	      (let* ((item (cdr c))
	             (m (cdr item)))
	        (when (and m (<= (my-imenu-item-position m) pos))
	          (cond
	           ((not closest)
	            (setq closest item))
	           ((< (- pos (my-imenu-item-position m))
	               (- pos (my-imenu-item-position (cdr closest))))
	            (setq closest item))))))
	    closest))
	;;;###autoload
	(defun my-mark-to-position (&optional position)
	  "Mark text from point to POSITION or end of of line."
	  (set-mark (or position (line-end-position)))
	  (activate-mark))
	;;;###autoload
	(defun my-closest-imenu-item ()
	  "Return the closest imenu item."
	  (my-ensure 'counsel)
	  (my-closest-imenu-item-internal (counsel--imenu-candidates)))
	;;;###autoload
	(defun my-setup-extra-keymap (extra-fn-list hint fn &rest args)
	  "Map EXTRA-FN-LIST to new keymap and show HINT after calling FN with ARGS."
	  (let ((echo-keystrokes nil))
	    (when fn (apply fn args))
	    (message hint)
	    (set-transient-map
	     (let ((map (make-sparse-keymap))
	           cmd)
	       (dolist (item extra-fn-list)
	         (setq cmd (nth 1 item))
	         (setq cmd (cond
	                    ((commandp cmd)
	                     cmd)
	                    (t
	                     `(lambda ()
	                        (interactive)
	                        (if (functionp ,cmd) (funcall ,cmd) ,cmd)))))
	         (define-key map (kbd (nth 0 item)) cmd))
	       map)
	     t)))

	;; @see http://emacs.stackexchange.com/questions/14129/which-keyboard-shortcut-to-use-for-navigating-out-of-a-string
	;;;###autoload
	(defun my-font-face-similar-p (f1 f2)
	  "Font face F1 and F2 are similar or same."
	  ;; (message "f1=%s f2=%s" f1 f2)
	  ;; in emacs-lisp-mode, the '^' from "^abde" has list of faces:
	  ;;   (font-lock-negation-char-face font-lock-string-face)
	  (if (listp f1) (setq f1 (nth 1 f1)))
	  (if (listp f2) (setq f2 (nth 1 f2)))

	  (or (eq f1 f2)
	      ;; C++ comment has different font face for limit and content
	      ;; f1 or f2 could be a function object because of rainbow mode
	      (and (string-match "-comment-" (format "%s" f1))
	           (string-match "-comment-" (format "%s" f2)))))
	;;;###autoload
	(defun my-font-face-at-point-similar-p (font-face-list)
	  "Test if font face at point is similar to any font in FONT-FACE-LIST."
	  (let* ((f (get-text-property (point) 'face))
	         rlt)
	    (dolist (ff font-face-list)
	      (if (my-font-face-similar-p f ff) (setq rlt t)))
	    rlt))
	;;;###autoload
	(defun my-pdf-view-goto-page (page)
	  "Go to pdf file's specific PAGE."
	  (cond
	   ((eq major-mode 'pdf-view-mode)
	    (pdf-view-goto-page page))
	   (t
	    (doc-view-goto-page page))))
	;;;###autoload
	(defun my-focus-on-pdf-window-then-back (fn)
	  "Focus on pdf window and call function FN then move focus back."
	  (let* ((pdf-window (cl-find-if (lambda (w)
	                                   (let ((file (buffer-file-name (window-buffer w))))
	                                     (and file (string= (file-name-extension file) "pdf"))))
	                                 (my-visible-window-list)))
	         (pdf-file (buffer-file-name (window-buffer pdf-window)))
	         (original-window (get-buffer-window)))
	    (when (and pdf-window pdf-file)
	      ;; select pdf-window
	      (select-window pdf-window)
	      ;; do something
	      (funcall fn pdf-file)
	      ;; back home
	      (select-window original-window))))
	;;;###autoload
	(defun my-list-windows-in-frame (&optional frame)
	  "List windows in FRAME."
	  (window-list frame 0 (frame-first-window frame)))
	;;;###autoload
	(defun my-visible-window-list ()
	  "Visible window list."
	  (cl-mapcan #'my-list-windows-in-frame (visible-frame-list)))
	;;;###autoload
	(defun my-lisp-find-file-or-directory (root regexp prefer-directory-p)
	  "Find files or directories in ROOT whose names match REGEXP.
	If PREFER-DIRECTORY-P is t, return directory; or else, returns file.
	This function is written in pure Lisp and slow."
	  (my-ensure 'find-lisp)
	  (find-lisp-find-files-internal
	   root
	   (lambda (file dir)
	     (let* ((directory-p (file-directory-p (expand-file-name file dir)))
	            (rlt (and (if prefer-directory-p directory-p (not directory-p))
	                      (not (or (string= file ".") (string= file "..")))
	                      (string-match regexp file))))
	       rlt))
	   'find-lisp-default-directory-predicate))


	(defvar my-media-file-extensions
	  '("3gp"
	    "avi"
	    "crdownload"
	    "flac"
	    "flv"
	    "m4v"
	    "mid"
	    "mkv"
	    "mov"
	    "mp3"
	    "mp4"
	    "mpg"
	    "ogm"
	    "part"
	    "rm"
	    "rmvb"
	    "wav"
	    "wmv"
	    "webm")
	  "File extensions of media files.")

	(defvar my-image-file-extensions
	  '("gif"
	    "jpg"
	    "jpeg"
	    "tif"
	    "png"
	    "svg"
	    "xpm")
	  "File extensions of image files.")

	;;;###autoload
	(defun my-file-extensions-to-regexp (extensions)
	  "Convert file EXTENSIONS to one regex."
	  (concat "\\." (regexp-opt extensions t) "$"))
	;;;###autoload
	(defun my-binary-file-p (file)
	  "Test if it's binary FILE."
	  (let* ((other-exts '("pyim" "recentf"))
	         (exts (append my-media-file-extensions
	                       my-image-file-extensions
	                       other-exts))
	         (regexp (my-file-extensions-to-regexp exts)))
	    (string-match regexp file)))
	;;;###autoload
	(defun my-strip-path (path strip-count)
	  "Strip PATH with STRIP-COUNT."
	  (let* ((i (1- (length path)))
	         str)
	    (while (and (> strip-count 0)
	                (> i 0))
	      (when (= (aref path i) ?/)
	        (setq strip-count (1- strip-count)))
	      (setq i (1- i)))
	    (setq str (if (= 0 strip-count) (substring path (1+ i)) path))
	    (replace-regexp-in-string "^/" "" str)))
	;;;###autoload
	(defun my-goto-line (n)
	  "Goto line N."
	  (goto-char (point-min))
	  (forward-line (1- n)))

  
;; Speed up startup
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen t) ; hèicèi makying ge kaidung gaimin

;; Goyan sönsik
(setq user-full-name suk-full-name)
(setq user-mail-address suk-mail-address)

(setq-default major-mode 'text-mode
              indent-tabs-mode nil)     ;; Permanently indent with spaces, never with TABs
(setq read-process-output-max #x10000)  ; 64kb.  Increase how much is read from processes in a single chunk (default is 4kb)
(setq vc-follow-symlinks t)
(setq font-lock-maximum-decoration t)
(setq initial-scratch-message nil)
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq set-mark-command-repeat-pop t) ; Repeating C-SPC after popping mark pops it again
(setq sentence-end "\\([。！？￥%×（）—]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*") ;; citding sentence-end sikbit tsungman biudim, bat yungzoi `fill` shi, zoi gêihòu hau cápyap 2 go hung gák.

(add-hook 'after-change-major-mode-hook (lambda ()(modify-syntax-entry ?_ "w"))) ;; yöng `_` bèi shiwai dánci ge zòusing bòufan
(add-hook 'after-change-major-mode-hook (lambda () (modify-syntax-entry ?- "w"))) ;; `-` fuhòu tungsöng
(setq sentence-end-double-space nil)

(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position 'always)

(when sys/win32p
  ;; Key Modifiers
  ;; make PC keyboard's Win key or other to type Super or Hyper
  ;; (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)    ; Left Windows key
  (setq w32-apps-modifier 'hyper)       ; Menu/App key
  ;; w32-register-hot-key 在 Emacs 中是用来在Windows系统上注册全局热键的函数，
  ;; 但它并不直接关联到执行 Emacs Lisp 函数。
  ;; 这个函数更多的是告诉Windows操作系统，
  ;; “当这个按键组合被按下时，应该通知Emacs”。
  ;; 要使Emacs在按下这个热键时执行特定的Elisp函数，还需要在Emacs内部设置相应的
  ;; 响应机制。这通常涉及到编写一些额外的Elisp代码来监听这个热键，
  ;; 并在它被按下时触发相应的操作。
  ;; 实际上，w32-register-hot-key 更多地用于在操作系统级别处理特定的按键组合，
  ;; 而不是在Emacs的编辑环境内。如果您想在Emacs内部绑定热键并执行函数，
  ;; 通常会使用像 global-set-key 或 define-key 这样的函数。
  (w32-register-hot-key [s-t])

  ;; Optimization
  (setq w32-get-true-file-attributes nil   ; decrease file IO workload
        w32-use-native-image-API t         ; use native w32 API
        w32-pipe-read-delay 0              ; faster IPC
        w32-pipe-buffer-size 65536)        ; read more at a time (64K, was 4K)
  )


;;====================================================
;; Encoding begin
;;====================================================
;; Set UTF-8 as the default coding system
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)            ;缓存文件编码
(setq default-file-name-coding-system 'utf-8-unix)              ;文件名编码
(setq default-keyboard-coding-system 'utf-8-unix)               ;键盘输入编码
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)) ;进程输出输入编码
(setq default-sendmail-coding-system 'utf-8-unix)               ;发送邮件编码
(setq default-terminal-coding-system 'utf-8-unix)               ;终端编码


(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq buffer-file-coding-system 'utf-8)
(setq session-save-file-coding-system 'utf-8)

(set-language-environment "UTF-8")

;; 重要提示:写在最后一行的，实际上最优先使用; 最前面一行，反而放到最后才识别。
;; utf-16le-with-signature 相当于 Windows 下的 Unicode 编码，这里也可写成
;; utf-16 (utf-16 ham:  utf-16le, utf-16be, utf-16le-with-signature dang)
;; Unicode
;; (prefer-coding-system 'utf-16le-with-signature)
;; (prefer-coding-system 'utf-16)
;; (prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8)
;; Unix like OS.
(unless sys/win32p
  ;; 新建文件使用utf-8-unix方式
  (prefer-coding-system 'utf-8-unix)
  (setq system-time-locale "C")
  (set-selection-coding-system 'utf-8))
(when sys/win32p
  (setq w32-unicode-filenames t)        ; kaiyung Unicode mangin ming jichi
  (setq file-name-coding-system 'utf-8) ; citji mangin ming pinmá wai UTF-8
  (setq locale-coding-system 'utf-8)    ; citji kêiwik pinmá wai UTF-8

  ;; gámtai
  ;;(prefer-coding-system 'gb2312)
  ;;(prefer-coding-system 'cp936)
  ;;(prefer-coding-system 'gb18030)
  ;;(setq file-name-coding-system 'gb18030)
  ;;(setq locale-coding-system 'gb18030)

  ;; jìngtai
  ;; (prefer-coding-system 'cp950)
  ;; (prefer-coding-system 'big5-hkscs)
  ;; (setq file-name-coding-system 'big5-hkscs) ; Hong Kong and Taiwan
  ;; (setq locale-coding-system 'big5-hkscs)

  ;; yatman
  ;; (setq file-name-coding-system 'cp932)
  ;; (setq locale-coding-system 'cp932)

)

(with-eval-after-load 'hydra

  (defhydra my-hydra-describe (:color blue :hint nil)
    "
Describe Something: (q to quit)
_a_ all help for everything screen
_b_ bindings
_c_ char
_C_ coding system
_f_ function
_i_ input method
_k_ key briefly
_K_ key
_l_ language environment
_m_ major mode
_M_ minor mode
_n_ current coding system briefly
_N_ current coding system full
_o_ lighter indicator
_O_ lighter symbol
_p_ package
_P_ text properties
_s_ symbol
_t_ theme
_v_ variable
_w_ where is something defined
"
    ("b" describe-bindings)
    ("C" describe-categories)
    ("c" describe-char)
    ("C" describe-coding-system)
    ("f" describe-function)
    ("i" describe-input-method)
    ("K" describe-key)
    ("k" describe-key-briefly)
    ("l" describe-language-environment)
    ("M" describe-minor-mode)
    ("m" describe-mode)
    ("N" describe-current-coding-system)
    ("n" describe-current-coding-system-briefly)
    ("o" describe-minor-mode-from-indicator)
    ("O" describe-minor-mode-from-symbol)
    ("p" describe-package)
    ("P" describe-text-properties)
    ("q" nil)
    ("a" help)
    ("s" describe-symbol)
    ("t" describe-theme)
    ("v" describe-variable)
    ("w" where-is))
  (global-set-key (kbd "C-c C-q") 'my-hydra-describe/body))



  (require 'lazy-load)
  (require 'init-key)
  (require 'init-package)
  (require 'init-completion)
  (require 'init-ui)
  (require 'init-org)
  (require 'init-utils)
  (require 'init-mode)
  (when sys/linuxp
    (progn
      (require 'init-im)   ;; windows 下表现不好
      (require 'init-sudo)
      )
    )


  ;; delay load
  (run-with-idle-timer
   1 nil
   #'(lambda ()
       ;; Restore session at last.
       (require 'init-session)
       (emacs-session-restore)
       (server-start)
       (require 'init-recentf)
       ;; auto-save
		(auto-save-enable)
		(setq auto-save-silent t)
		(setq auto-save-delete-trailing-whitespace t)
       (require 'init-edit)
       (require 'init-idle)
       ;;(require 'highlight-parentheses)
       (require 'init-highlight)
       (require 'init-window)
       (require 'init-reader)
       (require 'load-abbrev)
       ;; Programming
       (require 'init-ide)
       (autoload 'calendar "init-calendar" "Config Chinese calendar " t)
       ;; Make gc pauses faster by decreasing the threshold.
       (setq gc-cons-threshold (* 16 1000 1000))
       ))
  )



;; Reset the GC setting
(add-hook 'emacs-startup-hook
         (lambda ()
           "Fuifuk makying ge zik"
           (setq file-name-handler-alist default-file-name-handler-alist)
           ;; makying zik wai 0.8MB
           ;;(setq gc-cons-threshold 80000000)
           (message "Emacs ready in %s with %d garbage collections."
                    (format "%.2f seconds"
                            (float-time
                             (time-subtract after-init-time before-init-time)))
                    gcs-done)
           (add-hook 'focus-out-hook 'garbage-collect)))

;; @see https://www.reddit.com/r/emacs/comments/55ork0/is_emacs_251_noticeably_slower_than_245_on_windows/
;; Emacs 25 does gc too frequently
;; (setq garbage-collection-messages t) ; for debug
(defun my-cleanup-gc ()
  "Clean up gc."
  (setq gc-cons-threshold  67108864) ; 64M
  (setq gc-cons-percentage 0.1) ; original value
  (garbage-collect))

(message "*** Emacs loaded in %s with %d garbage collections."
         (format "%.2f seconds"
                 (float-time (time-subtract after-init-time before-init-time)))
         gcs-done)
(run-with-idle-timer 4 nil #'my-cleanup-gc)
;; =========================================================
;; 通过编辑配置文件使其可以调用外部程序，来为其添加功能。
;; 增加命令
;;(defun lxr (names)
;;  (interactive "s查找联系人，请输入条件：")
;;  (call-process-shell-command "lxr" nil t t "-s" names))
;;执行命令
;;首先按功能键，Alt+x，然后输入命令 lxr 。
;;系统提示：“查找联系人，请输入条件："。
;;输入完成后，emacs 会执行命令lxr -s names，并输出执行的结果。
;; =========================================================
;; async-shell-command