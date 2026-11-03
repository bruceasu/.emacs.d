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
;; Keep file handlers for remote and compressed files.
;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (file-exists-p suk-emacs-var-dir)
  (make-directory suk-emacs-var-dir t))
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

;; Load configuration and theme libraries recursively.
(add-subdirs-to-load-path suk-emacs-config-dir t)
(add-subdirs-to-load-path suk-emacs-themes-dir t)

;; Add only local extension directories used by the active configuration.
;; Package.el manages package-user-dir itself; scanning it recursively is unnecessary.
(dolist (relative-dir
         '("" "company-english-helper" "duplicate-line" "emacs-rime"
           "expand-region" "goto-line-preview" "input-method"
           "input-method/pyim" "lazy-load" "lazycat" "move-text"
           "open-newline" "sdcv" "thing-edit" "vdiff" "vundo"))
  (let ((dir (expand-file-name relative-dir suk-emacs-extension-dir)))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir t))))

;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp")

;; (setq custom-file "~/.config/emacs/custom-settings.el")
;; (load custom-file t)

(require 'basic-function)

(load-if-exists (expand-file-name "core.el" suk-emacs-root-dir))

(load-if-exists (expand-file-name "suk.el" suk-emacs-root-dir))
(unless (server-running-p) (server-start))

;; coding modes
(defun suk/enable-coding ()
  "Enable coding-related tools."
  (interactive)
  (condition-case err
      (require 'init-coding)
    (error
     (display-warning 'suk
                      (format "Coding features failed: %s"
                              (error-message-string err))))))

(defalias 'suk/enable-devtools #'suk/enable-coding)
(add-hook 'after-init-hook #'suk/enable-coding)

(defun suk/configure-frame-ui (frame)
  "Load the UI adapter and optional appearance for FRAME."
  (with-selected-frame frame
    (condition-case err
        (if (display-graphic-p frame)
            (unless (featurep 'suk-ui-gui)
              (load (expand-file-name "gui.el" suk-emacs-root-dir) nil t))
          (unless (featurep 'suk-ui-tui)
            (load (expand-file-name "tui.el" suk-emacs-root-dir) nil t)))
      (error
       (display-warning 'suk
                        (format "UI adapter failed: %s"
                                (error-message-string err)))))
    (when suk-enable-appearance
      (run-with-idle-timer 0.5 nil #'suk/load-frame-appearance frame))))

(defun suk/load-frame-appearance (frame)
  "Load optional appearance after startup for FRAME."
  (when (and suk-enable-appearance (frame-live-p frame))
    (with-selected-frame frame
      (let ((appearance-feature
             (if (display-graphic-p frame)
                 'suk-appearance-gui
               'suk-appearance-tui)))
        (unless (featurep appearance-feature)
          (condition-case err
              (when (load (expand-file-name "appearance.el" suk-emacs-root-dir) nil t)
                (provide appearance-feature))
            (error
             (display-warning 'suk
                              (format "Appearance layer failed: %s"
                                     (error-message-string err))))))))))

(add-hook 'after-make-frame-functions #'suk/configure-frame-ui)
(unless (daemonp)
  (suk/configure-frame-ui (selected-frame)))

;; Hanlde minified code
(if emacs/>=27p (add-hook 'after-init-hook #'global-so-long-mode))
(when sys/linuxp
  (load-if-exists (expand-file-name "linux.el" suk-emacs-root-dir)))
(when sys/win32p
  (load-if-exists (expand-file-name "windows.el" suk-emacs-root-dir)))
(when sys/macp
  (load-if-exists (expand-file-name "mac.el" suk-emacs-root-dir)))
