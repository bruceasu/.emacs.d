;;; init-ide.el --- IDE configuration

;; Filename: init-ide.el
;; Description: IDE configuration
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-10-20 09:22:09
;; Version: 0.1
;; Last-Updated: 2008-10-20 09:22:12
;;           By: Andy Stewart
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

;;; ### Hippie-exapnd ###
;;; --- 符号补全
;; hippie-expand 自动补全策略
(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev-visible         ;dabbrev策略, 可见窗口优先
        try-expand-dabbrev                 ;dabbrev策略
        try-expand-dabbrev-all-buffers     ;dabbrev策略, 包括所有窗口(除了当前窗口)
        try-expand-dabbrev-from-kill       ;dabbrev策略, 从所有删除记录里搜索
        try-complete-file-name             ;补全文件名
        try-complete-file-name-partially   ;补全文件名, 匹配优先
        try-expand-list                    ;补全list
        try-expand-list-all-buffers        ;补全list, 包括所有窗口(除了当前窗口)
        try-expand-line                    ;整行补全
        try-expand-line-all-buffers        ;整行补全, 包括所有窗口(除了当前窗口)
        try-complete-lisp-symbol           ;补全符号, 符号太多了, 设置低优先级利于高效补全
        try-complete-lisp-symbol-partially ;补全符号, 包括所有窗口(除了当前窗口)
        try-expand-whole-kill              ;kill-ring里面补全
        ))

;;代码折叠
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'ess-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'python-mode-hook         'hs-minor-mode)
 
(global-set-key [f2] 'hs-toggle-hiding)


; 著名的Emacs补全框架
(use-package
  company 
  :defer 2 
  :hook (after-init . global-company-mode) 
  :init (setq company-tooltip-align-annotations t company-idle-delay 0 company-echo-delay 0
              company-minimum-prefix-length 1 company-require-match nil company-dabbrev-ignore-case
              nil company-dabbrev-downcase nil company-show-numbers t) 
  :config 
  :bind (:map company-active-map
              ("M-n" . nil) 
              ("M-p" . nil) 
              ("C-n" . #'company-select-next) 
              ("C-p" . #'company-select-previous)) 
  (:map leader-key
        ("c s" . #'company-yasnippet))) 

;; 人工智能补全代码
(use-package 
  company-tabnine
  :disabled 
  :ensure t 
  :after 'company-mode 
  'company-tabnine-mode 
  :config (add-to-list 'company-backends #'company-tabnine))


;; Emacs对语言服务器支持的插件
(use-package 
  lsp-mode 
  :ensure t
  :defer t
  :commands lsp
  :hook ((java-mode python-mode js-mode js2-mode web-mode c-mode c++-mode objc-mode) . lsp)
  :custom
  (lsp-idle-delay 1200)
  (lsp-auto-guess-root nil)
  (lsp-file-watch-threshold 2000)
  (read-process-output-max (* 1024 1024))
  (lsp-eldoc-hook nil)
  (lsp-prefer-flymake nil)
  :bind (:map lsp-mode-map
			  ("C-c C-f" . lsp-format-buffer)
			  ("M-RET" . lsp-ui-sideline-apply-code-actions)
			  ("M-\\" . lsp-execute-code-action))
  :config
  (setq lsp-prefer-capf t))

;; 各个语言的Debug工具
(use-package dap-mode
  :ensure t
  :diminish
  :bind
  (:map dap-mode-map
		(("<f3>" . dap-debug)
		 ("<f4>" . dap-continue)
		 ("<f5>" . dap-next)
		 ("<f6>" . dap-step-in)
		 ("<f7>" . dap-step-out)
		 ("<f8>" . dap-breakpoint-toggle))))


;; 美化company
(use-package 
  company-box 
  :ensure t 
  :hook (company-mode . company-box-mode))


;; 代码片段
(use-package 
  yasnippet 
  :ensure t 
  :commands (yas-reload-all) 
  :init (autoload 'yas-minor-mode-on "yasnippet") 
  (setq yas-snippet-dirs '("~/.emacs.d/share/snippets")) 
  (dolist (x '(org-mode-hook prog-mode-hook snippet-mode-hook)) 
    (add-hook x #'yas-minor-mode-on)))

;; 大量可用的代码片段
(use-package 
  yasnippet-snippets 
  :ensure t)

;;(require 'init-flycheck)
;;(require 'init-vcs)
;; 项目管理
(require 'init-projectile)
(require 'init-program-c)
(require 'init-program-java)
(require 'init-program-web)
(require 'init-program-python)

(provide 'init-ide)

;;; init-ide.el ends here
