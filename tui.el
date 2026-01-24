(defconst suk/is-gui (display-graphic-p))
(defconst suk/is-tui (not suk/is-gui))
;; =========================
;; TUI
;; =========================
;; 启动干净
(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      make-backup-files nil
      auto-save-default nil
      frame-inhibit-implied-resize t)
;; 终端友好 UI
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;; 元素一部分禁用
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;(toggle-frame-maximized)

;; UTF-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
;; 行为
(column-number-mode 1)
;; 剪贴板（TUI 下也有用）
(setq select-enable-clipboard t)
(fset 'yes-or-no-p 'y-or-n-p)


;; 緩衝區，无令立殺之。
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
(setq find-file-visit-truename t)    
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.8)
;; Reset the GC setting
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; 默认係 0.8MB
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
;; 历史
(setq savehist-file (concat suk-emacs-var-dir "/history"))
;; Amx
(setq amx-save-file (concat suk-emacs-var-dir "/amx-items"))
;; 自动保存
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

;; 加载定义的目录
(add-subdirs-to-load-path suk-emacs-config-dir t)
(add-subdirs-to-load-path suk-emacs-extension-dir t)
(add-subdirs-to-load-path suk-emacs-themes-dir t)

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


;;(setq package-user-dir "c:/green/emacs-suk/.local/share/elpa")
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(setq package-archives
;;      '(("gnu" . "https://elpa.gnu.org/packages/")
;;        ("melpa" . "https://melpa.org/packages/")))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; =========================
;; Git（TUI 下也要好用）
;; =========================
(unless (package-installed-p 'magit)
  (package-install 'magit))
(global-set-key (kbd "C-x g") #'magit-status)
;; =========================
;; 搜索 / 跳转（TUI 必备）
;; =========================

;;(package-initialize)
;;(unless package-archive-contents
;;  (package-refresh-contents))
(dolist (pkg '(vertico consult))
  (unless (package-installed-p pkg)
    (package-install pkg)))
(vertico-mode 1)
(global-set-key (kbd "C-c o") #'consult-outline)
(global-set-key (kbd "C-c s") #'consult-ripgrep)


;;(unless (package-installed-p 'use-package)
;;  (package-refresh-contents)
;;  (package-install 'use-package))

;;(require 'use-package)
;;(use-package treemacs
;;  :ensure t)

;;(load "C:/green/emacs-suk/.emacs.d/share/themes/lazycat-theme/lazycat-theme.el")
;;Switch to dark theme:
;;(lazycat-theme-load-dark)
;; Switch to light theme:
;; (lazycat-theme-load-light)
;; (lazycat-theme-toggle)
;;(require 'lazycat-theme)
;;(lazycat-theme-load)
;;(run-with-idle-timer 1 nil #'lazycat-theme-load-with-sunrise)

(when my/is-tui  

  ;; 行号在小屏可能太挤，可关
  (global-display-line-numbers-mode 0)
  (setq-default line-spacing 0.2)
  ;; 视觉换行（写书很重要）
  (global-visual-line-mode 1)
  ;; 光标明显一点
  (setq cursor-type 'bar)
  ;; 主题（终端最稳） 颜色（终端友好）
  (load-theme 'tango-dark t)
  ;; 滚动更顺
  (setq scroll-margin 3
        scroll-conservatively 101)

  ;; =========================
  ;; Org（写书核心）
  ;; =========================
  (require 'org)
  (setq 
   ;; 不折叠太狠
   org-startup-folded 'content
   ;; 写书时更像文本
   org-hide-emphasis-markers t
   ;; Org 美化不要太激进
   org-ellipsis " ▼")
  (defun my/org-writing-mode ()
    "TUI 下写书专用模式"
    (interactive)
    (org-content)
    (visual-line-mode 1)
    (setq-local line-spacing 0.2))
  (global-set-key (kbd "C-c w") #'my/org-writing-mode)
  (global-set-key (kbd "C-c j") #'org-goto)


  )
