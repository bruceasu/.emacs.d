;;; init-ide.el --- IDE configuration

;; Filename: init-ide.el
;; Description: IDE configuration
;; Author: Bruce bruceasu@gmail.com
;; Maintainer: Bruce bruceasu@gmail.com
;; Copyright (C) 2008, 2009, Bruce, all rights reserved.
;; Created: 2008-10-20 09:22:09
;; Version: 0.1
;; Last-Updated: 2022-01-20 09:30:45
;;       By: Bruce
;; URL:
;; Keywords: ide
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; IDE configuration
;;

;;; Code:



(eval-when-compile
  (require '+const)
  (require '+custom)
  (require 'init-package)
  )

;; 语法检查包
(use-package flycheck
  :ensure t
  :defer 3)

;; format all, formatter for almost languages
;; great for programmers
(use-package format-all
  :hook (prog-mode . format-all-ensure-formatter)
  :bind ("C-c f" . #'format-all-buffer))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; 折叠和收缩代码
(use-package hideshow
  :ensure t
  :defer 1
  :diminish hs-minor-mode
  :bind (:map prog-mode-map
        ("C-c ." . hs-toggle-hiding)
        ("C-c ," . hs-show-all)
        )
  :hook (prog-mode . hs-minor-mode))
;;代码折叠
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'ess-mode-hook        'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)

;; 代码片段
(require 'yasnippet)
(yas-global-mode 1)
(autoload 'yas-minor-mode-on "yasnippet")
  (setq yas-snippet-dirs '("~/.emacs.d/share/snippets"))
  (dolist (x '(org-mode-hook prog-mode-hook snippet-mode-hook))
  (add-hook x #'yas-minor-mode-on))


(use-package projectile
  :ensure t
  :config
  ;; Eanble Projectile globally
  (setq projectile-completion-system 'ido)
  (setq ido-enable-flex-matching t)
  (projectile-mode 1)

  ;; Set akeybinding for projectile commands
  (global-set-key (kbd "C-c p") 'projectitle-command-map)
)


(defvar user-home-dir (getenv "HOME"))
(if sys/win32p
    (defvar user-home-dir (getenv "USERPROFILE"))
)

(setq lsp-maven-path (concat user-home-dir "/.m2/settings.xml"))  ;; maven setting path
(setq org-directory (concat user-home-dir "/org") ;; org root path
(setq org-roam-directory (concat org-dirctory "/org-roam") ;; org roam root path
(setq lsp-java-java-path (concat (getenv "JAVA_HOME") "/bin/java"))    ;; java11 exec path

(use-package lsp-mode
  :ensure t
  :hook (java-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

(use-package lsp-java
  :ensure t
  :after lsp-mode
  :config
  (add-hook 'java-mode-hook 'lsp))  ;; 在打开 Java 文件时启动 lsp

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
    
(use-package eglot-java)

(add-hook 'java-mode-hook 'eglot-java-mode)
(add-hook 'eglot-java-mode-hook (lambda ()
								  (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
								  (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
								  (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
								  (define-key eglot-java-mode-map (kbd "C-c l N") #'eglot-java-project-new)
								  (define-key eglot-java-mode-map (kbd "C-c l T") #'eglot-java-project-build-task)
								  (define-key eglot-java-mode-map (kbd "C-c l R") #'eglot-java-project-build-refresh)))



(setq copilot-node-executable "C:\\green\\node-v20.10.0-win-x64\\node.exe")
(add-to-list 'load-path "C:\\green\\emacs-29.1\\.emacs.d\\site-lisp\\copilot\\copilot.el")
(require 'copilot)
(add-hook 'prog-mode-hook 'copilot-mode)
;; To customize the behavior of copilot-mode, please check copilot-enable-predicates and copilot-disable-predicates.
;; You need to bind copilot-complete to some key and call copilot-clear-overlay inside post-command-hook.
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(add-to-list 'copilot-major-mode-alist '("c" . "cpp" . "css" . "go" . "java" . "html" . "javascript" . "javascriptreact" . "json" . "python" . "sql" . "shellscript"))
;; Login to Copilot by M-x copilot-login. You can also check the status by M-x copilot-diagnose (NotAuthorized means you don't have a valid subscription).

;;--------------------------------------
;; web develop
;;--------------------------------------

;; 快速编写 HTML 代码
(use-package emmet-mode
	:defer 3
	:init (setq emmet-expand-jsx-className? t)
	:hook (web-mode typescript-mode js-mode js2-mode rjsx-mode css-mode scss-mode sgml-mode))


;; New `less-cs-mde' in Emacs 26
(unless (fboundp 'less-css-mode)
	(use-package less-css-mode))

;;JSON mode
(use-package json-mode
	:defer 3
	:mode "\\.json\\'")

;; Improved JavaScript editing mode
(use-package js2-mode)
;; Major mode for editing web templates
(use-package web-mode
	:ensure t
	:defines company-backends
	:mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\|jsx?$\\)$"
)


(use-package rjsx-mode
	:ensure t
	:mode ("\\.js\\'")
	:config
	(add-hook 'rjsx-mode-hook (lambda()
	              (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
	              (flycheck-select-checker 'javascript-eslint)))
)

(use-package prettier-js
	:ensure t
	:defer 3
	:hook ((css-mode web-mode typescript-mode js-mode json-mode js2-mode) . prettier-js-mode))

;; 直接编辑 HTML 文件时的设置
(add-hook 'mhtml-mode-hook 'web-dev-attached)

(provide 'init-ide)

;;; init-ide.el ends here
