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

;; kill buffer without my confirmation
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))


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


;; Clear to avoid analyzing files when loading remote files.
(setq file-name-handler-alist nil)
;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)
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


(require '+const)
(require '+custom)
(require '+fn)
(require 'init-basic)
;; Goyan sönsik
(setq user-full-name suk-full-name)
(setq user-mail-address suk-mail-address)
(require 'init-package)
(require 'lazy-load)
(require 'init-key)
(require 'init-completion)
(require 'init-ui)
(require 'init-org)
(require 'init-mode)
(when sys/linuxp
  (progn
    (require 'init-im)   ;; windows 下表现不好
    (require 'init-sudo)
    )
  )

;; delay load
(run-with-idle-timer 1 nil
  #'(lambda ()
    ;; Restore session at last.
    (require 'init-session)
    (emacs-session-restore)
    (server-start)
    (require 'init-recentf)
    (require 'init-idle)
    ;;(require 'highlight-parentheses)
    (require 'init-highlight)
    (require 'init-window)
    (require 'load-abbrev)
    ;; Programming
    (require 'init-ide)
    (autoload 'calendar "init-calendar" "Config Chinese calendar " t)
    ;; Make gc pauses faster by decreasing the threshold.
    (setq gc-cons-threshold (* 16 1000 1000))
    ))


;; Reset the GC setting
(add-hook 'emacs-startup-hook
         (lambda ()
           "Fuifuk makying ge zik"
           (setq file-name-handler-alist default-file-name-handler-alist)
           (message "*** Emacs ready in %s with %d garbage collections."
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
