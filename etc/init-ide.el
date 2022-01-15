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



(eval-when-compile
  (require '+const)
  (require '+custom))

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

(global-set-key [f2] 'hs-toggle-hiding)


; 著名的Emacs补全框架
(use-package
  company
  :defer 2
  :hook (after-init . global-company-mode)
  :init (setq company-tooltip-align-annotations t company-idle-delay 0 company-echo-delay 0
              company-minimum-prefix-length 1 company-require-match nil company-dabbrev-ignore-case
              nil company-dabbrev-downcase nil company-show-numbers t)
  ;; :config 
  :bind (:map company-active-map
              ("C-n" . #'company-select-next)
              ("C-p" . #'company-select-previous)
              ("TAB" . company-complete-selection)
              ("M-h" . company-complete-selection)
              （"M-H" . company-complete-common)
              （"M-w" . company-show-location)
              ("M-s" . company-search-candidates)
              ("M-S" . company-filter-candidates)
              ("M-n" . company-select-next)
              ("M-p" . company-select-previous)
              （"M-I" . yas-expand)
              )
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


	 
;; 项目管理

;; Manage and navigate projects
(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
              ("s-t" . projectile-find-file) ; `cmd-t' or `super-t'
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix "")
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)
  :config
  (projectile-update-mode-line)         ; Update mode-line at the first time

  ;; Use the faster searcher to handle project files: ripgrep `rg'
  (let ((command
         (cond
          ((executable-find "rg")
           (let ((rg-cmd ""))
             (dolist (dir projectile-globally-ignored-directories)
               (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
             (concat "rg -0 --files --color=never --hidden" rg-cmd)))
          )))
    (setq projectile-generic-command command))

  ;; Faster searching on Windows
  (when sys/win32p
    (when (executable-find "rg")
      (setq projectile-indexing-method 'alien)
      (setq projectile-enable-caching nil))

    ;; FIXME: too slow while getting submodule files on Windows
    (setq projectile-git-submodule-command nil))

)

;; program languages


;; C/C++ Mode
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

  (unless suk-lsp
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
          :hook (flycheck-mode . flycheck-irony-setup))))

    ;; Company mode backend for C/C++ header files
    (with-eval-after-load 'company
      (use-package company-c-headers
        :init (cl-pushnew 'company-c-headers company-backends)))))
(use-package google-c-style
  :defer t
  :ensure t
  :commands
  (google-set-c-style))
  
(use-package lsp-java
  :ensure t
  :hook (java-mode . (lambda () (request 'lsp-java)))
  :config
  (setq lsp-java-server-install-dir (expand-file-name "~/.emacs.d/var/jdt-lsp")))



(use-package autodisass-java-bytecode
  :ensure t
  :defer t)

(use-package css-mode
  :ensure nil
  :init (setq css-indent-offset 2))

;; SCSS mode
(use-package scss-mode
  :init
  ;; Disable complilation on save
  (setq scss-compile-at-save nil))

;; New `less-cs-mde' in Emacs 26
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; CSS eldoc
(use-package css-eldoc
  :commands turn-on-css-eldoc
  :hook ((css-mode scss-mode less-css-mode) . turn-on-css-eldoc))

;; JSON mode
(use-package json-mode)

;; Improved JavaScript editing mode
(use-package js2-mode
  :defines flycheck-javascript-eslint-executable
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :config
  ;; Use default keybindings for lsp
  (if suk-lsp
      (unbind-key "M-." js2-mode-map))

  (with-eval-after-load 'flycheck
    (if (or (executable-find "eslint_d")
            (executable-find "eslint")
            (executable-find "jshint"))
        (setq js2-mode-show-strict-warnings nil))
    (if (executable-find "eslint_d")
        ;; https://github.com/mantoni/eslint_d.js
        ;; npm -i -g eslint_d
        (setq flycheck-javascript-eslint-executable "eslint_d")))

  (use-package js2-refactor
    :diminish js2-refactor-mode
    :hook (js2-mode . js2-refactor-mode)
    :config (js2r-add-keybindings-with-prefix "C-c C-m")))

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
  :config
  (add-hook 'web-mode-hook 'company-mode)
  (add-hook 'web-mode-hook 'lsp-vue-enable)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'"))))

;; Live browser JavaScript, CSS, and HTML interaction
(use-package skewer-mode
  :diminish skewer-mode
  :hook ((js2-mode . skewer-mode)
         (css-mode . skewer-css-mode)
         (web-mode . skewer-html-mode)
         (html-mode . skewer-html-mode))
  :init
  ;; diminish
  (with-eval-after-load 'skewer-css
    (diminish 'skewer-css-mode))
  (with-eval-after-load 'skewer-html
    (diminish 'skewer-html-mode)))

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
