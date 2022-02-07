;;; init-ide.el --- IDE configuration

;; Filename: init-ide.el
;; Description: IDE configuration
;; Author: Bruce bruceasu@gmail.com
;; Maintainer: Bruce bruceasu@gmail.com
;; Copyright (C) 2008, 2009, Bruce, all rights reserved.
;; Created: 2008-10-20 09:22:09
;; Version: 0.1
;; Last-Updated: 2022-01-20 09:30:45
;;		   By: Bruce
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
  (require '+custom))

;;; ### Hippie-exapnd ###
;;; --- 符号补全
;; hippie-expand 自动补全策略
(setq hippie-expand-try-functions-list
	  '(
		try-expand-dabbrev-visible		;dabbrev策略, 可见窗口优先
		try-expand-dabbrev				;dabbrev策略
		try-expand-dabbrev-all-buffers	;dabbrev策略, 包括所有窗口(除了当前窗口)
		try-expand-dabbrev-from-kill	;dabbrev策略, 从所有删除记录里搜索
		try-complete-file-name			;补全文件名
		try-complete-file-name-partially	;补全文件名, 匹配优先
		try-expand-list					;补全list
		try-expand-list-all-buffers		;补全list, 包括所有窗口(除了当前窗口)
		try-expand-line					;整行补全
		try-expand-line-all-buffers		;整行补全, 包括所有窗口(除了当前窗口)
		try-complete-lisp-symbol		;补全符号, 符号太多了, 设置低优先级利于高效补全
		try-complete-lisp-symbol-partially ;补全符号, 包括所有窗口(除了当前窗口)
		try-expand-whole-kill			;kill-ring里面补全
		))

;; 折叠和收缩代码
(use-package hideshow
  :ensure t
  :defer 1
  :diminish hs-minor-mode
  :bind (:map prog-mode-map
			  ("C-c TAB" . hs-toggle-hiding)
			  ("C-c p +" . hs-show-all)
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

; 著名的Emacs补全框架
(use-package
	company
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

;; 人工智能补全代码
(use-package
  company-tabnine
  :load-path "~/.emacs.d/site-lisp/company-tabnine"
  :disabled
  :ensure t
  :after 'company-mode
  'company-tabnine-mode
  :config
  (add-to-list 'company-backends #'company-tabnine)
  )

 (require 'init-company-tabnine)

;; ===============================
;; Emacs对语言服务器支持的插件
;; ===============================
;; (use-package
;; 	lsp-mode
;; 	:ensure t
;; 	:defer t
;; 	:commands lsp
;; 	:hook ((java-mode python-mode js-mode js2-mode web-mode c-mode c++-mode objc-mode) . lsp)
;; 	:custom
;; 	(lsp-idle-delay 1200)
;; 	(lsp-auto-guess-root nil)
;; 	(lsp-file-watch-threshold 2000)
;; 	(read-process-output-max (* 1024 1024))
;; 	(lsp-eldoc-hook nil)
;; 	(lsp-prefer-flymake nil)
;; 	:bind (:map lsp-mode-map
;; 			("C-c C-f" . lsp-format-buffer)
;; 			("M-RET" . lsp-ui-sideline-apply-code-actions)
;; 			("M-\\" . lsp-execute-code-action))
;; 	:config
;; 	(defun my/lsp-client-clear-leak-handlers (lsp-client)
;; 	"Clear leaking handlers in LSP-CLIENT."
;; 	(let ((response-handlers (lsp--client-response-handlers lsp-client))
;; 			to-delete-keys)
;; 		(maphash (lambda (key value)
;; 				 (when (> (time-convert (time-since (nth 3 value)) 'integer)
;; 							(* 2 lsp-response-timeout))
;; 					(push key to-delete-keys)))
;; 				 response-handlers)
;; 		(when to-delete-keys
;; 			(message "Deleting %d handlers in %s lsp-client..."
;; 				(length to-delete-keys)
;; 				(lsp--client-server-id lsp-client))
;; 		(mapc (lambda (k) (remhash k response-handlers))
;; 				to-delete-keys))))
;; 	(defun my/lsp-clear-leak ()
;; 		"Clear all leaks"
;; 		(maphash (lambda (_ client)
;; 			(my/lsp-client-clear-leak-handlers client))
;; 			lsp-clients))
;; 	(setq my/lsp-clear-leak-timer
;; 		(run-with-timer 5 5 #'my/lsp-clear-leak))
;; 	(add-to-list 'lsp-language-id-configuration '(".*\\.less$" . "css"))
;; 	(setq lsp-prefer-capf t))

;; ;; LSP 模式的帮助文档相关
;; (use-package lsp-ui
;;   :after lsp-mode
;;   :commands lsp-ui-mode
;;   :config
;;   (setq lsp-ui-doc-delay 3)
;;   (setq lsp-ui-doc-enable nil)
;;   (setq lsp-ui-sideline-delay 1)
;;   (setq lsp-ui-sideline-enable t))


;; 代码片段
(use-package
  yasnippet
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


;; ===============================
;; 项目管理
;; ===============================
;; Manage and navigate projects
;; (use-package projectile
;;   :diminish
;;   :bind (:map projectile-mode-map
;; 			  ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
;; 			  ("C-c p" . projectile-command-map))
;;   :hook (after-init . projectile-mode)
;;   :init
;;   (setq projectile-mode-line-prefix "")
;;   (setq projectile-sort-order 'recentf)
;;   (setq projectile-use-git-grep t)
;;   :config
;;   (projectile-update-mode-line)		 ; Update mode-line at the first time

;;   ;; Use the faster searcher to handle project files: ripgrep `rg'
;;   (let ((command
;; 		 (cond
;; 		  ((executable-find "rg")
;; 		   (let ((rg-cmd ""))
;; 			 (dolist (dir projectile-globally-ignored-directories)
;; 			   (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
;; 			 (concat "rg -0 --files --color=never --hidden" rg-cmd)))
;; 		  )))
;; 	(setq projectile-generic-command command))

;;   ;; Faster searching on Windows
;;   (when sys/win32p
;; 	(when (executable-find "rg")
;; 	  (setq projectile-indexing-method 'alien)
;; 	  (setq projectile-enable-caching nil))

;; 	;; FIXME: too slow while getting submodule files on Windows
;; 	(setq projectile-git-submodule-command nil))

;; )

;;======================================
;; program languages
;;======================================
;;--------------------------------------
;; C/C++ Mode
;;--------------------------------------
(use-package cc-mode
  :ensure nil
  :bind (:map c-mode-base-map
			  ("C-c c" . compile))
  :hook (c-mode-common . (lambda ()
						   (c-set-style "bsd")
						   (setq tab-width 4)
						   (setq c-basic-offset 4)))
  :config
  ;; (use-package modern-cpp-font-lock
  ;;   :diminish
  ;;   :init (modern-c++-font-lock-global-mode t))

  (use-package irony
	  :defines (irony-mode-map irony-server-w32-pipe-buffer-size)
	  :hook (((c-mode c++-mode objc-mode) . irony-mode)
			 (irony-mode . irony-cdb-autosetup-compile-options))
	  :config
	  ;; Windows performance tweaks
	  (when (boundp 'w32-pipe-read-delay)
		(setq w32-pipe-read-delay 0))
	  ;; Set the buffer size to 64K on Windows (from the original 4K)
	  (when (boundp 'w32-pipe-buffer-size)
		(setq irony-server-w32-pipe-buffer-size (* 64 1024)))

	  (with-eval-after-load 'counsel
		(bind-keys :map irony-mode-map
				   ([remap completion-at-point] . counsel-irony)
				   ([remap complete-symbol] . counsel-irony)))

	  (use-package irony-eldoc
		:hook (irony-mode . irony-eldoc))

	  (with-eval-after-load 'company
		(use-package company-irony
		  :init (cl-pushnew 'company-irony company-backends))
		(use-package company-irony-c-headers
		  :init (cl-pushnew 'company-irony-c-headers company-backends)))

	  (with-eval-after-load 'flycheck
		(use-package flycheck-irony
		  :hook (flycheck-mode . flycheck-irony-setup)))

	;; Company mode backend for C/C++ header files
	(with-eval-after-load 'company
	  (use-package company-c-headers
		:init (cl-pushnew 'company-c-headers company-backends)))))

(use-package google-c-style
  :defer t
  :ensure t
  :commands
  (google-set-c-style))


;;--------------------------------------
;; Java 配置
;;--------------------------------------
;; (use-package lsp-java
;;   :ensure t
;;   :hook (java-mode . (lambda () (request 'lsp-java)))
;;   :config
;;   (setq lsp-java-server-install-dir (expand-file-name "~/.emacs.d/var/jdt-lsp")))

;; ;; Java 调试配置
;; (use-package dap-mode
;;   :after lsp-java
;;   :config (setq dap-auto-configure-features (remove 'controls dap-auto-configure-features)))
;; (use-package dap-java :ensure nil)

;; (use-package autodisass-java-bytecode
;;   :ensure t
;;   :defer t)

;;--------------------------------------
;; web develop
;;--------------------------------------
(use-package prettier-js
  :defer 3
  :hook ((css-mode web-mode typescript-mode js-mode json-mode) . prettier-js-mode))

;; 快速编写 HTML 代码
(use-package emmet-mode
  :defer 3
  :init (setq emmet-expand-jsx-className? t)
  :hook (web-mode typescript-mode js-mode)
  :config
  (add-to-list 'emmet-jsx-major-modes 'js-mode)
  (add-to-list 'emmet-jsx-major-modes 'typescript-mode))

;; 附加 Web 开发的各种插件
(defun web-dev-attached ()
  ;; 设置关闭自动换行
  (setq truncate-lines t)
  ;; 开启显示行号
  (display-line-numbers-mode +1)
  ;; 启动行号左侧对齐，并且不随着宽度变化而变化
  (setq display-line-numbers-width-start t)
  ;; 开启代码折叠子模式
  (hs-minor-mode t)
  ;; 设置列参考线：120
  (setq display-fill-column-indicator-column 120)
  (display-fill-column-indicator-mode t)
  ;; 开启代码折叠快捷键
  (define-key hs-minor-mode-map (kbd "C-c C-f") 'hs-toggle-hiding))

(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2)
  :hook
  (css-mode-hook
          (lambda ()
            ;; 开启 LSP 模式自动完成
            (lsp)
            ;; 设置自动缩进的宽度
            (setq css-indent-offset 2)
            ;; 设置 Company 后端
            (add-to-list (make-local-variable 'company-backends)
                         '(company-css company-files company-capf company-dabbrev))
            ;; 其它开发设置
            (web-dev-attached)))
  )

;; New `less-cs-mde' in Emacs 26
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

; JSON mode
(use-package json-mode
  :defer 3
  :mode "\\.json\\'"
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              ;; 开启 LSP 模式自动完成
              (lsp)
              ;; 设置自动缩进的宽度
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)
              ;; 其它开发设置
              (web-dev-attached))))

;; JavaScript/TypeScript 语法检查设置
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint
          (and root
               (expand-file-name "node_modules/.bin/eslint"
                                 root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
;; CSS/LESS 语法检查设置
(defun my/use-stylelint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (stylelint
          (and root
               (expand-file-name "node_modules/.bin/stylelint"
                                 root))))
    (when (and stylelint (file-executable-p stylelint))
      (setq-local flycheck-css-stylelint-executable stylelint)
      (setq-local flycheck-less-stylelint-executable stylelint))))

(defun my/web-html-setup()
  "Setup for html files."
  ;; 开启 LSP 模式自动完成
  (lsp)
  ;; 设置 Company 后端
  (add-to-list (make-local-variable 'company-backends)
               '(company-files company-css company-capf company-dabbrev)))

(defun my/web-vue-setup()
  "Setup for vue related."
  ;; 开启 LSP 模式自动完成
  (lsp)
  ;; 设置 Company 后端
  (add-to-list (make-local-variable 'company-backends)
               '(company-files company-css)))

(defun my/web-js-setup()
  "Setup for js related."
  ;; 开启 LSP 模式自动完成
  (lsp)
  ;; 设置 Company 后端
  (add-to-list (make-local-variable 'company-backends)
               '(company-files company-css company-capf company-dabbrev-code :separate)))

;; Major mode for editing web templates
(use-package web-mode
  :ensure t
  :defines company-backends
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\|jsx?$\\)$"
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (setq
    web-mode-css-indent-offset 2                  ;; CSS 默认缩进 2 空格：包含 HTML 的 CSS 部分以及纯 CSS/LESS/SASS 文件等
    web-mode-code-indent-offset 2                 ;; JavaScript 默认缩进 2 空格：包含 HTML 的 SCRIPT 部分以及纯 JS/JSX/TS/TSX 文件等
    web-mode-markup-indent-offset 2               ;; HTML 默认缩进 2 空格：包含 HTML 文件以及 Vue 文件的 TEMPLATE 部分
    web-mode-enable-css-colorization t            ;; 开启 CSS 部分色值的展示：展示的时候会有光标显示位置异常
    web-mode-enable-current-column-highlight nil)
  :config
  (add-hook 'web-mode-hook 'company-mode)
  (add-hook 'web-mode-hook (lambda()
                             (web-dev-attached)
                             (cond ((equal web-mode-content-type "html")
                                    (my/web-html-setup))
                                   ((member web-mode-content-type '("vue"))
                                    (my/web-vue-setup)))))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

(use-package js
  :ensure nil
  :init
  (setq js-indent-level 2)
  :config
  (add-hook 'js-mode-hook (lambda()
                            (web-dev-attached)
                            (my/web-js-setup))))

;; Improved JavaScript editing mode
(use-package js2-mode
  :defines flycheck-javascript-eslint-executable
  :mode (("\\.js\\'" . js2-mode)
		 ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
				("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
		 (js2-mode . js2-highlight-unused-variables-mode))
)

(use-package typescript-mode
  :defer 3
  :mode "\\.ts[x]?\\'"
  :init
  ;; 设置缩进两个空格
  (setq typescript-indent-level 2)
  :config
  (add-hook 'typescript-mode-hook (lambda()
                             (web-dev-attached)
                             (my/web-js-setup))))

;; 直接编辑 HTML 文件时的设置
(add-hook 'mhtml-mode-hook 'web-dev-attached)
;; 语法检查包
(use-package flycheck
  :defer 3
  :config
  (add-hook 'flycheck-mode-hook 'my/use-eslint-from-node-modules)
  (add-hook 'flycheck-mode-hook 'my/use-stylelint-from-node-modules)
  :hook ((css-mode web-mode js-mode typescript-mode) . flycheck-mode))

(use-package haml-mode)
(use-package emmet-mode
  :ensure t
  :hook ((web-mode . emmet-mode)
		 (css-mode . emmet-mode)))


(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(defun adjust-languages-indent (n)
  (setq-local c-basic-offset n)

  (setq-local coffee-tab-width n)
  (setq-local javascript-indent-level n)
  (setq-local js-indent-level n)
  (setq-local js2-basic-offset n)

  (setq-local web-mode-attr-indent-offset n)
  (setq-local web-mode-attr-value-indent-offset n)
  (setq-local web-mode-code-indent-offset n)
  (setq-local web-mode-css-indent-offset n)
  (setq-local web-mode-markup-indent-offset n)
  (setq-local web-mode-sql-indent-offset n)

  (setq-local css-indent-offset n))

(dolist (hook (list
			   'c-mode-hook
			   'c++-mode-hook
			   'java-mode-hook
			   'haskell-mode-hook
			   'asm-mode-hook
			   'emms-tag-editor-mode-hook
			   'sh-mode-hook
			   'haskell-cabal-mode-hook
			   'ruby-mode-hook
			   'qml-mode-hook
			   'scss-mode-hook
			   'coffee-mode-hook
			   ))
  (add-hook hook #'(lambda ()
					 (setq indent-tabs-mode nil)
					 (adjust-languages-indent 4)
					 )))

(dolist (hook (list
			   'web-mode-hook
			   'js-mode-hook
			   ))
  (add-hook hook #'(lambda ()
					 (setq indent-tabs-mode nil)
					 (adjust-languages-indent 2)
					 )))


(provide 'init-ide)

;;; init-ide.el ends here
