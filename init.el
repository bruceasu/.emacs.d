(provide 'init)

;; 定义一尐目录，方便日后迁移。user-emacs-directory 通常係 ~/.emacs.d
;; windows 下, ~/ 通常係 $EMACS_INSTALL_DIR, 或者 EMACS 测定嘅 `HOME`
;; 变量。
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

(if (eq system-type 'windows-nt)
    (defvar user-home-dir (getenv "USERPROFILE")))
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
;; server 无效
(require 'server)
;;(setq server-socket-dir suk-emacs-var-dir)

;; Clear to avoid analyzing files when loading remote files.
(setq file-name-handler-alist nil)
;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

(unless (file-exists-p suk-emacs-var-dir)
    (make-directory cache-dir t))
   (setq projectile-cache-file (expand-file-name "projectile.cache" suk-emacs-var-dir))

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
    ;; The original version did not add the first search-dir itself to
    ;; the `load path`. The recursive search-dir was added before the
    ;; recursion.
    (add-to-list 'load-path search-dir))
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; goleui bat bityiu ge mukluk, taising Emacs kaidung cudou.
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; m hai mangin
                   (not (file-directory-p (concat dir subdir)))
                   ;; yiceui haamin ge mukluk
                   (member subdir '("." ".." ; Linux/Uniux haitung ge  dongcin mukluk tungmaai fu mukluk
                                    "dist" "node_modules" "__pycache__" ; takding ge yüyin seunggwaan ge mukluk
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github")))) ; baanbun hungjai mukluk
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; mukluk bauhaam  .el .so .dll ge mangin di louging sinji gaa dou `load-path` binleung
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非Elisp语言编写的Emacs动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; jüyi: add-to-list ge daisaam go caamsou bitseuiwai t, timgaa dou meibou,
          ;; kokbou gwongdou yausin
          (add-to-list 'load-path subdir-path t))

        ;; geieuuk daigwai sausok ji mukluk.
        (add-subdirs-to-load-path subdir-path nil)))))

;; gaazoi tsiding ge mukluk.
(add-subdirs-to-load-path suk-emacs-config-dir t)
(add-subdirs-to-load-path suk-emacs-extension-dir t)
(add-subdirs-to-load-path suk-emacs-themes-dir t)


;; (add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
;; (add-to-list 'load-path "~/vendor/org-mode/lisp")
;; (add-to-list 'load-path "~/vendor/org-mode/contrib/lisp")
;; (setq custom-file "~/.config/emacs/custom-settings.el")
;; (setq use-package-always-ensure t)
;; (load custom-file t)

(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.8)
;; Reset the GC setting
(add-hook 'emacs-startup-hook
          (lambda ()
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
  (run-with-idle-timer 4 nil #'my-cleanup-gc)

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

;; This sets up the load path so that we can override it
(setq warning-suppress-log-types '((package reinitialization)))
;; 指定ELPA目录
(setq package-user-dir (expand-file-name "elpa" "~/.local/share"))
(add-subdirs-to-load-path package-user-dir t)

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))

(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

(require 'package)
;; gnu：
;; http://elpa.gnu.org/packages/
;; https://elpa.emacs-china.org/gnu/ http://1.15.88.122/gnu/
;; https://mirrors.163.com/elpa/gnu/
;; https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/
;; melpa:
;; http://melpa.org/packages/
;; https://www.mirrorservice.org/sites/melpa.org/packages/
;; https://elpa.emacs-china.org/melpa/ http://1.15.88.122/melpa/
;; https://mirrors.163.com/elpa/melpa/
;; https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/

;;(setq package-archives '(("melpa" . "http://melpa.org/packages/")
;;                         ("gnu" . "http://elpa.gnu.org/packages/")
;;                         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

;; Un-comment below line if you follow "Install stable version in easiest way"
(setq package-archives '(("myelpa" . "~/myelpa/")))

(setq package-check-signature nil) ; 个别时候会出现签名校验失败

;; Initialize packages
;; (unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
;;   (setq package-enable-at-startup nil)          ; To prevent initializing twice
;;   (package-initialize))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
;; make use-package default behavior better
;; with `use-package-always-ensure' you won't need ":ensure t" all the time
;; with `use-package-always-defer' you won't need ":defer t" all the time
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)

(require 'use-package)


    ;;;###autoload
(defun my-ensure (feature)
  "Make sure FEATURE is required."
  (unless (featurep feature)
    (condition-case nil
        (require feature)
      (error nil))))

;; On-demand installation of packages
(defun require-package (&rest packages)
  "Ensure PACKAGES are installed.
 If a package is not installed, it will be installed automatically."
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package)))
  (use-package package)
  )

;; Compatibility
(use-package compat :demand t)

(load-file (expand-file-name "suk.el" suk-emacs-root-dir))

(unless (server-running-p) (server-start))

(run-with-idle-timer
 1
 nil
 #'(lambda()
   (require 'load-abbrev)
   ))
;; chmod +x
;; ref. http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#chmod
(add-hook 'after-save-hook'executable-make-buffer-file-executable-if-script-p)
(autoload 'calendar "init-calendar" "Config Chinese calendar " t)
;; Hanlde minified code
   (if emacs/>=27p (add-hook 'after-init-hook #'global-so-long-mode))
(when sys/linuxp
  (load-file (expand-file-name "linux.el" suk-emacs-root-dir)))
(when sys/win32p
   (load-file (expand-file-name "windows.el" suk-emacs-root-dir)))
 (when sys/macp
     (load-file (expand-file-name "mac.el" suk-emacs-root-dir)))
