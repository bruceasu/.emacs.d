;;; init-ui.el --- Initialize UI configurations.	-*- lexical-binding: t -*-
;; Copyright (C) 2018 Suk
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

;;; Commentary:
;;
;; Emacs Package management configurations.
;;

;;; Code:



(eval-when-compile
  (require '+const)
  (require '+custom)
  (require 'init-package))

;; 字体

(require 'load-set-font)

(display-time-mode -1)

(setq column-number-mode t)
(setq-default fill-column 80)
(setq column-number-mode t)

;; Title
(setq frame-title-format
      '("Emacs - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(use-package centaur-tabs
  :demand
  :init
  ;; Set the style to rounded with icons
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-icons t)
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward) ;; Ctrl PgUp
  ("C-<next>" . centaur-tabs-forward))  ;; Ctrl PgDn



;; ===================================
;; Theme 主题设置
;; -----------------------------------
;;(require 'lazycat-theme)
;;(lazycat-theme-load-dark)

(use-package doom-themes
  :ensure t
  ;;:config
  ;; 加载一个主题，DOOM One 是 DOOM Emacs 的默认主题，非常美观
  :init
  (load-theme 'doom-one t))

;; 似乎冇加载，手工处理一下。
;;(load-theme 'doom-one t)


(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (column-number-mode 1)
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
	:if (display-graphic-p))

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



(provide 'init-ui)
