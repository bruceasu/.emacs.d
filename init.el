;;k; init.el --- Emacs configurations.    -*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2018 Suk

;; Author: Suk
;; Version: 1.0.0
;; Keywords: .emacs.d

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
;; Emacs configurations.
;;

;;; Code:

;; 定义一些启动目录，方便下次迁移修改
(defvar suk-emacs-root-dir (file-truename user-emacs-directory))
(defvar suk-emacs-config-dir (concat suk-emacs-root-dir "/etc"))
(defvar suk-emacs-extension-dir (concat suk-emacs-root-dir "/extensions"))
(defvar suk-emacs-share-dir (concat suk-emacs-root-dir "/share"))
(defvar suk-emacs-themes-dir (concat suk-emacs-share-dir "/themes"))
(defvar suk-emacs-elpa-dir (concat suk-emacs-root-dir "/elpa"))
(defvar suk-emacs-var-dir (concat suk-emacs-root-dir "/var"))
(defvar suk-emacs-tmp-dir (concat suk-emacs-var-dir "/tmp"))
(defvar suk-emacs-backup-dir (concat suk-emacs-tmp-dir "/backup"))

(defvar user-home-dir (getenv "HOME"))
(if (eq system-type 'windows-nt)
    (defvar user-home-dir (getenv "USERPROFILE"))
)

;; 设置缓存文件/杂七杂八的文件存放的地址
;; 不好的做法
;; (setq user-emacs-directory "~/.emacs.d/var")

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
(setq eshell-directory-name (concat suk-emacs-var-dir "eschell"))
(setq eshell-history-file-name (concat eshell-directory-name "/history"))
;; projectitle-bookmarks
(setq projectile-known-projects-file (concat suk-emacs-var-dir "/projectile-bookmarks.eld"))
(setq backup-directory-alist `(("" . ,suk-emacs-tmp-dir)))
;; Bookmark
(setq bookmark-default-file (concat suk-emacs-var-dir "/emacs.bmk"))
;; Diary
(setq diary-file (concat user-home-dir "/diary"))


;; 忽略 cl 过期警告
(setq byte-compile-warnings '(cl-function))


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
    ;; 原来的版本没有把第1个 search-dir 本身添加到load path
    ;; 递归时的search-dir是在递归前加入了。
    (add-to-list 'load-path search-dir))
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; 过滤出不必要的目录，提升Emacs启动速度
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; 不是文件的都移除
                   (not (file-directory-p (concat dir subdir)))
                   ;; 目录匹配下面规则的都移除
                   (member subdir '("." ".." ;Linux当前目录和父目录
                                    "dist" "node_modules" "__pycache__" ;语言相关的模块目录
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github")))) ;版本控制目录
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; 目录下有 .el .so .dll 文件的路径才添加到 load-path 中，提升Emacs启动速度
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非Elisp语言编写的Emacs动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; 注意：add-to-list 函数的第三个参数必须为 t ，表示加到列表末尾
          ;; 这样Emacs会从父目录到子目录的顺序搜索Elisp插件，顺序反过来会导致Emacs无法正常启动
          (add-to-list 'load-path subdir-path t))

        ;; 继续递归搜索子目录
        (add-subdirs-to-load-path subdir-path nil)))))


(add-subdirs-to-load-path suk-emacs-config-dir t)
(add-subdirs-to-load-path suk-emacs-extension-dir t)
(add-subdirs-to-load-path suk-emacs-themes-dir t)
(add-subdirs-to-load-path suk-emacs-elpa-dir t)


(let (;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.8)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))

  ;; Emacs配置文件内容写到下面.
   (add-hook 'emacs-startup-hook
            (lambda ()
              "Restore defalut values after init."
              (setq file-name-handler-alist default-file-name-handler-alist)
              ;; The default is 0.8MB
              ;;(setq gc-cons-threshold 80000000)
              (message "Emacs ready in %s with %d garbage collections."
                       (format "%.2f seconds"
                               (float-time
                                (time-subtract after-init-time before-init-time)))
                       gcs-done)
              (add-hook 'focus-out-hook 'garbage-collect)))


  (with-temp-message ""                 ;抹掉插件启动的输出
    ;; Constants
    (require '+const)

    ;; Customization
    (require '+custom)
    ;; Packages
    (require 'init-package)
    (require 'init-basic)
    (require 'lazy-load)
    (require 'init-key)
    (require 'init-ui)
    (require 'init-ext-packages)
    ;; (use-package esup
    ;;              :ensure t
    ;;              ;; To use MELPA Stable use ":pin melpa-stable",
    ;;              :pin melpa
    ;;              :commands (esup))

    ;; windows 下表现不好
    (when sys/linuxp
      (progn
        (require 'init-im)
		(require 'init-sudo)
        )
      )
     ;; 可以延后加载的
    (run-with-idle-timer
     1 nil
     #'(lambda ()
     	;;  (require 'init-org)
		 ;; Restore session at last.
         ;; 速度有点慢
         (require 'init-session)
         (emacs-session-restore)
         (require 'init-line-number)
         (require 'init-idle)
         ;;(require 'highlight-parentheses)
         (require 'init-auto-save)
         (require 'init-awsome-pair)
         (require 'init-calendar)
         (require 'load-abbrev)
         ;; Programming
         (require 'init-ide)
         (server-start)
	     ;; Make gc pauses faster by decreasing the threshold.
	     (setq gc-cons-threshold (* 16 1000 1000))

         ))
    )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
