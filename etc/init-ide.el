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
  )

;; 语法检查包
(use-package flycheck
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

;; 著名的Emacs补全框架
(use-package company
  :defer 2
  :hook (after-init . global-company-mode)
  :init (setq company-tooltip-align-annotations t
        company-idle-delay 0 company-echo-delay 0
        company-minimum-prefix-length 1
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-show-numbers t)
  ;; :config
  :bind (:map company-active-map
        ("C-n" . #'company-select-next)
        ("C-p" . #'company-select-previous)
        ("TAB" . company-complete-selection)
        ("M-h" . company-complete-selection)
        ("M-H" . company-complete-common)
        ("M-s" . company-search-candidates)
        ("M-S" . company-filter-candidates)
        ("M-n" . company-select-next)
        ("M-p" . company-select-previous))
  (:map leader-key
    ("c s" . #'company-yasnippet
     ))
  )

;; 代码片段
(use-package yasnippet
  :ensure t
  :commands (yas-reload-all)
  :init
  (autoload 'yas-minor-mode-on "yasnippet")
  (setq yas-snippet-dirs '("~/.emacs.d/share/snippets"))
  (dolist (x '(org-mode-hook prog-mode-hook snippet-mode-hook))
  (add-hook x #'yas-minor-mode-on))
  :config
  (require 'init-yasnippet)
  ;; 大量可用的代码片段
  (use-package
  yasnippet-snippets
  :ensure t
  )
  )

(use-package projectile)
(use-package hydra)


;; install dash s editorconfig

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
