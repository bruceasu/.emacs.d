(provide 'init)
;; Define some directories to make future migration easier. 
;; user-emacs-directory is usually ~/.emacs.d
;; On Windows, ~/ is usually $EMACS_INSTALL_DIR, or the `HOME` variable detected by Emacs

(defvar suk-emacs-root-dir      (file-truename user-emacs-directory))
(defvar suk-emacs-config-dir    (expand-file-name "etc" suk-emacs-root-dir))
(defvar suk-emacs-extension-dir (expand-file-name "extensions" suk-emacs-root-dir))
(defvar suk-emacs-share-dir     (expand-file-name "share" suk-emacs-root-dir))
(defvar suk-emacs-themes-dir    (expand-file-name "themes" suk-emacs-share-dir))
(defvar suk-emacs-var-dir       (expand-file-name "var" suk-emacs-root-dir))
(defvar suk-emacs-tmp-dir       (expand-file-name "tmp" suk-emacs-var-dir))
(defvar suk-emacs-backup-dir    (expand-file-name "backup" suk-emacs-tmp-dir))

;; OS 嘅 HOME 目录。
(defvar user-home-dir (getenv "HOME"))
(if (eq system-type 'windows-nt) (defvar user-home-dir (getenv "USERPROFILE")))


;; blink search
(setq blink-search-db-path (expand-file-name "blink-search.db" suk-emacs-tmp-dir))
;; History
(setq savehist-file (concat suk-emacs-var-dir "/history"))
;; Amx
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
;; Clear to avoid analyzing files when loading remote files.
(setq file-name-handler-alist nil)
;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (file-exists-p suk-emacs-var-dir)
  (make-directory cache-dir t))
(setq projectile-cache-file (expand-file-name "projectile.cache" suk-emacs-var-dir))

;; server
(require 'server)

;; Ignore `cl` expiration warnings
(setq byte-compile-warnings '(cl-function))
;; 王勇的版本 https://manateelazycat.github.io/emacs/2022/03/02/emacs-load-directory-recursively.html
(require 'cl-lib)
(defun add-subdirs-to-load-path (search-dir isFirst)
  (interactive)
  (when isFirst
    ;; The original version did not add the first search-dir itself to
    ;; the `load path`. The recursive search-dir was added before the
    ;; recursion.
    (add-to-list 'load-path search-dir))
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
	     ;; Filter the Unnecessary direcotires, speed up emacs startup.
	     (cl-remove-if
	      #'(lambda (subdir)
		  (or
		   ;; Not file
		   (not (file-directory-p (concat dir subdir)))
		   ;; remove the follow directoies
		   (member subdir '("." ".." ; The current direcotiry and parent diretory in  Linux/Uniux OS
				    "dist" "node_modules" "__pycache__" ; Conventional directories for specific languages.
				    "RCS" "CVS" "rcs" "cvs" ".git" ".github")))) ; Version control directories.
	      (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
	;; The directories including:  .el .so .dll will add to `load-path` variable.
	(when (cl-some #'(lambda (subdir-file)
			   (and (file-regular-p (concat subdir-path subdir-file))
				;; .so .dll 文件指非Elisp语言编写的Emacs动态库
				(member (file-name-extension subdir-file) '("el" "so" "dll"))))
		       (directory-files subdir-path))

	  ;; Notice: The third argument of add-to-list must be `t`, add to the end,
	  ;; make sure Breadth-first.
	  (add-to-list 'load-path subdir-path t))

	;; Continue recursively through subdirectories.
	(add-subdirs-to-load-path subdir-path nil)))))

;; Load the defined directories.
(add-subdirs-to-load-path suk-emacs-config-dir t)
(add-subdirs-to-load-path suk-emacs-extension-dir t)
(add-subdirs-to-load-path suk-emacs-themes-dir t)

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

;; (setq custom-file "~/.config/emacs/custom-settings.el")
;; (load custom-file t)

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.8)
;; Reset the GC setting
(add-hook 'emacs-startup-hook
	  (lambda ()
	    ;; default: 0.8MB
	    ;;(setq gc-cons-threshold 80000000)
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)
	    (defun my-cleanup-gc ()
	      "Clean up gc."
	      (setq gc-cons-threshold  67108864) ; 64M
	      (setq gc-cons-percentage 0.1) ; original value
	      (garbage-collect))
	    (run-with-idle-timer 4 nil #'my-cleanup-gc)
	    (add-hook 'focus-out-hook 'garbage-collect)))

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

;; set const
(defconst custom-template-file
  (expand-file-name "custom-template.el" user-emacs-directory)
  "Custom template file of Suk's Emacs.")

(defconst suk-homepage
  "https://github.com/bruceasu/.emacs.d"
  "The Github page of this Emacs config.")


(defgroup suk nil
  "suk Emacs customizations."
  :group 'convenience
  :link '(url-link :tag "Homepage" "https://github.com/bruceasu/.emacs.d"))

(defcustom org-roam-directory (expand-file-name "RoamNotes" user-home-dir)
  "The org roam directory."
  :group 'suk
  :type 'string)

(defcustom org-files-directory (expand-file-name "org" user-home-dir)
  "The org roam directory."
  :group 'suk
  :type 'string)

(defcustom my-org-inline-css-file "~/.emacs.d/share/my-org-style-min.css"
  "The org css style file."
  :group 'suk
  :type 'string)

(defcustom suk-rime t
  "Display use emacs rime or not."
  :group 'suk
  :type 'boolean)

(defcustom suk-copilot-enable nil
  "Enable copilot mode"
  :group 'suk
  :type 'boolean)

;; Load `custom-file'
;; If it doesn't exist, copy from the template, then load it.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(let ((custom-template-file
       (expand-file-name "custom-template.el" user-emacs-directory)))
  (if (and (file-exists-p custom-template-file)
	   (not (file-exists-p custom-file)))
      (copy-file custom-template-file custom-file)))

(load-file custom-file)
;;(if (file-exists-p custom-file)
;;    (load custom-file))

;; Load `custom-post.el'
;; Put personal configurations to override defaults here.
(add-hook 'after-init-hook
	  (lambda ()
	    (let ((file
		   (expand-file-name "custom-post.el" user-emacs-directory)))
	      (if (file-exists-p file)
		  (load file)))))

(require 'basic-function)

;; This sets up the load path so that we can override it
(setq warning-suppress-log-types '((package reinitialization)))
;; 指定ELPA目录
(setq package-user-dir (expand-file-name "elpa" "~/.local/share"))
(add-subdirs-to-load-path package-user-dir t)

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

(setq package-check-signature nil) ; 个别时候会出现签名校验失败

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Should set before loading `use-package'
;; make use-package default behavior better
;; with `use-package-always-ensure' you won't need ":ensure t" all the time
;; with `use-package-always-defer' you won't need ":defer t" all the time
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  )

(require 'use-package)

;; Compatibility
(use-package compat :demand t)

(load-if-exists (expand-file-name "suk.el" suk-emacs-root-dir))
(unless (server-running-p) (server-start))
