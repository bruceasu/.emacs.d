;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Suk

;; Author: Suk

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
;; Emacs Package management configurations.
;;

;;; Code:

(eval-when-compile
  (require '+const)
  (require '+custom))

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))
(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

(require 'package)
;; gnu：
;; https://elpa.gnu.org/packages/
;; https://elpa.emacs-china.org/gnu/ http://1.15.88.122/gnu/
;; https://mirrors.163.com/elpa/gnu/
;; https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/
;; melpa:
;; https://melpa.org/packages/
;; https://www.mirrorservice.org/sites/melpa.org/packages/
;; https://elpa.emacs-china.org/melpa/ http://1.15.88.122/melpa/
;; https://mirrors.163.com/elpa/melpa/
;; https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/
(add-to-list 'package-archives
   '("melpa" . "http://1.15.88.122/melpa/"))
(add-to-list 'package-archives
   '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives
   '("gnu" . "http://1.15.88.122/gnu/"))
(add-to-list 'package-archives
   '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
     
;; (setq package-check-signature nil) ; 个别时候会出现签名校验失败

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
;; Required by `use-package'
;;(use-package diminish)
(use-package bind-key)
;; Enhance M-x
(use-package amx)

;; 增强了搜索功能
(use-package swiper
  :bind
  (("C-s" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

;; Treat undo history as a tree, ^x u
(use-package undo-tree
  :ensure t
  :defer 2
  :diminish undo-tree-mode
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil)
  ;; HACK: keep the diff window
  (with-no-warnings
    (make-variable-buffer-local 'undo-tree-visualizer-diff)
    (setq-default undo-tree-visualizer-diff t))
  )
;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :defer 2
  :custom
  ;; 弹出方式，底部弹出
  (which-key-popup-type 'side-window)
  :config
  (which-key-mode)
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :hook (after-init . which-key-mode))

;; Persistent the scratch buffer
(use-package persistent-scratch
  :defer 2
  :preface
  (defun my-save-buffer ()
    "Save scratch and other buffer."
    (interactive)
    (let ((scratch-name "*scratch*"))
      (if (string-equal (buffer-name) scratch-name)
          (progn
            (message "Saving %s..." scratch-name)
            (persistent-scratch-save)
            (message "Wrote %s" scratch-name))
        (save-buffer))))
  :hook (after-init . persistent-scratch-setup-default)
  :bind (:map lisp-interaction-mode-map
              ("C-x C-s" . my-save-buffer)))


(use-package htmlize
  :defer 2)								; covert to html


(use-package centaur-tabs
  :demand
  :init
  ;; Set the style to rounded with icons
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-icons t)
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (("C-c t" . treemacs))
  :custom
  (treemacs-is-never-other-window t)
  :config
  (setq treemacs-width 30)
  :hook
  (treemacs-mode . treemacs-project-follo-mode)
)
(defun hide-mode-line-in-treemsacs()
  "Hide the mode line in the treemacs buffer."
  (setq-local  mode-line-format nil)
)
(add-hook 'treemacs-mode-hook 'hide-mode-line-in-treemacs)
(use-package helm
  :ensure t)
(use-package ac-helm
  :ensure t)
;; 著名的Emacs补全框架, 为 LSP 提供额外的功能，如自动补全
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
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-sytle 'querty)
  (setq switch-window-qwerty-shortcuts
          '("a" "s" "d" "f" "j" "k" "l"))
  (global-company-mode)        
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

;; On-the-fly spell checker
(unless sys/win32p
  (use-package flyspell
    :ensure t
    :defer 2
    :diminish flyspell-mode
    :if (executable-find "aspell")
    :hook (((text-mode outline-mode) . flyspell-mode)
           (prog-mode . flyspell-prog-mode)
           (flyspell-mode . (lambda ()
                              (unbind-key "C-;" flyspell-mode-map)
                              (unbind-key "C-," flyspell-mode-map)
                              (unbind-key "C-." flyspell-mode-map))))
    :init
    (setq flyspell-issue-message-flag nil)
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))
  )

;; Open files as another user
(use-package sudo-edit)
(use-package mood-line
  :ensure t
  :if window-system
  :init
  (mood-line-mode)
)
;; 主题设置
(use-package doom-themes
  :ensure t
  :config
  ;; 加载一个主题，DOOM One 是 DOOM Emacs 的默认主题，非常美观
  (load-theme 'doom-one t))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config (column-number-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-window-width-limit nil)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count t)
  ;;(doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-modification-icon t)
  ;;(doom-modeline-env-python-executeable "python")
  ;; needs display-time-mode to be one
  (doom-modeline-time t)
  (doom-modeline-vcs-max-leghth 50)
)

;; 切换buffer焦点时高亮动画
(use-package beacon
  :ensure t
  :hook (after-init . beacon-mode))

;; 图形界面插件的设置
(when (display-graphic-p)
	;; 图标支持
	(use-package all-the-icons
		;; :ensure t
		:load-path "~/.emacs.d/extensions/all-the-icons"
		:if (display-graphic-p)
	)
	(use-package hydra)
    (use-package hydra-posframe
  		:load-path "~/.emacs.d/extensions/hydra-posframe/hydra-posframe.el"
  		:defer 1
  		:hook (after-init . hydra-posframe-mode))
  	;; 浮动窗口支持
  	(use-package posframe :ensure t)
  	(use-package vertico-posframe
  		:ensure t
  		:custom
  		(vertico-posframe-parameters
  		'((left-fringe . 8)
     		(right-fringe . 8))))
)


;; 一些我不知道用途的依赖  
(use-package slime
  :ensure t)
(use-package anaconda-mode
  :ensure t)
(use-package company-anaconda
  :ensure t)
(use-package company-box
  :ensure t)
(use-package auto-compile
  :ensure t)
(use-package haml-mode
  :ensure t)
(use-package wgrep
  :ensure t)
(use-package multiple-cursors
  :ensure t)
(use-package rtags
  :ensure t)
(use-package window-purpose
  :ensure t)
(use-package password-store
  :ensure t)
(use-package historian
  :ensure t)
(use-package gitlab
  :ensure t)
(use-package bibtex-completion
  :ensure t)
(use-package ov
  :ensure t)
(use-package xml-rpc
  :ensure t)
(use-package deferred
  :ensure t)
(use-package frame-local
  :ensure t)
(use-package shell-split-string
  :ensure t)
(use-package pythonic
  :ensure t)
(use-package packed
  :ensure t)

(use-package alert
  :ensure t)
(use-package yasnippet
  :ensure t
  :load-path "~/.emacs.d/extensions/yasnippet")
(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
