;;; init.el --- Emacs configurations.    -*- lexical-binding: t no-byte-compile: t; -*-

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
(defvar suk-emacs-extension-dir (concat suk-emacs-root-dir "/site-lisp"))
(defvar suk-emacs-share-dir (concat suk-emacs-root-dir "/share"))

(defun add-subdirs-to-load-path (dir)
  "Recursive add directories to `load-path'."
  (let ((default-directory (file-name-as-directory dir)))
    (add-to-list 'load-path dir)
    (normal-top-level-add-subdirs-to-load-path)))

(add-subdirs-to-load-path (expand-file-name "etc"       user-emacs-directory))
(add-subdirs-to-load-path (expand-file-name "site-lisp" user-emacs-directory))
(add-subdirs-to-load-path (expand-file-name "themes"    suk-emacs-share-dir))

(let (;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      (gc-cons-percentage 0.6)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))

  ;; Emacs配置文件内容写到下面.
  ;; (when (version< emacs-version "25.1")
  ;;   (error "This requires Emacs 25.1 and above!"))

  (add-hook 'emacs-startup-hook
    (lambda ()
      "Restore defalut values after init."
      (setq file-name-handler-alist default-file-name-handler-alist)
      ;; The default is 0.8MB
      (setq gc-cons-threshold 8000000)
      (message "Emacs ready in %s with %d garbage collections."
        (format "%.2f seconds"
          (float-time
            (time-subtract after-init-time before-init-time)))
        gcs-done)
      (add-hook 'focus-out-hook 'garbage-collect)))

  ;; 忽略 cl 过期警告
  (setq byte-compile-warnings '(cl-function))

  (with-temp-message ""                 ;抹掉插件启动的输出
    ;; autoload functions
    (require '+autoload)

    ;; Constants
    (require '+const)

    ;; Customization
    (require '+custom)

    (require 'lazy-load)
    (require 'awesome-pair)
    (require 'display-line-numbers)
    (require 'basic-toolkit)

    (require 'one-key)
    (require 'init-one-key)
    (require 'init-key)

    ;; Packages
    (require 'init-package)
    (require 'init-basic)
    (require 'init-ui)
    (require 'init-utils)
    (require 'init-file-encoding)
    (require 'init-buffers)
    ;; (use-package esup
    ;;              :ensure t
    ;;              ;; To use MELPA Stable use ":pin melpa-stable",
    ;;              :pin melpa
    ;;              :commands (esup))

    ;; windows 下表现不好
    (when sys/linuxp
      (progn
        ;; Programming
        (require 'init-ide)
        (require 'init-im)
        )
    )

    ;; 个人的一些特别设置
    (require 'init-suk)


    ;; 可以延后加载的
    (run-with-idle-timer
     1 nil
     #'(lambda ()
         ;; Restore session at last.
         (require 'init-session)
         (emacs-session-restore)
         (require 'init-idle)
         (require 'highlight-parentheses)
         (require 'init-auto-save)
         (require 'init-auto-sudoedit)
         (require 'init-awsome-pair)
         (require 'init-awesome-tray)
         (require 'init-awesome-tab)
         (require 'init-sudo)
         (require 'init-bookmark)
         (require 'init-dired)
		 (require 'init-calendar)
         ;; Music
         ;;(require 'init-emms)
         ;; (server-start)            ;为emacsclient准备使用场景，比如git
         ))
    )
)

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 8 1000 1000))
(put 'scroll-left 'disabled nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
(put 'upcase-region 'disabled nil)
