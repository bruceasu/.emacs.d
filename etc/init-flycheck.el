;; init-flycheck.el --- Initialize flycheck configurations.	-*- lexical-binding: t -*-

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
;; Flycheck configurations.
;;

;;; Code:

;; 代码检查
(use-package flycheck
  :diminish flycheck-mode
  :ensure t
  :hook (after-init . global-flycheck-mode)
  :bind (:map leader-key
              ("t t" . global-flycheck-mode)) 
  :config
  (which-key-add-key-based-replacements "M-SPC t t" "开关flycheck")
  (setq flycheck-global-modes
		'(not text-mode outline-mode fundamental-mode org-mode diff-mode
            shell-mode eshell-mode term-mode vterm-mode))
  (setq flycheck-indication-mode 'right-fringe)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  ;; 只在打开和保存文件时才进行检查
  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; 美化一下
  (when (fboundp 'define-fringe-bitmap) 
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow [16 48 112 240 112 48 16] nil nil
      'center))
  
  ;; Display Flycheck errors in GUI tooltips
  (when (display-graphic-p)
	  (use-package flycheck-posframe 
		:ensure t 
		:custom-face (flycheck-posframe-border-face
					  ((t (:inherit default)))) 
		:hook (flycheck-mode . flycheck-posframe-mode) 
		:init (setq flycheck-posframe-border-width 1
					flycheck-posframe-inhibit-functions
					'((lambda (&rest _) 
						(bound-and-true-p company-backend)))))
	(use-package flycheck-pos-tip
	  :ensure t 
      :defines flycheck-pos-tip-timeout 
	  :hook (global-flycheck-mode . flycheck-pos-tip-mode)
	  :config (setq flycheck-pos-tip-timeout 30))
	(use-package flycheck-popup-tip
	  :ensure t 
	  :hook (global-flycheck-mode . flycheck-popup-tip-mode))
	
  )

  ;; Jump to and fix syntax errors via `avy'
  (use-package avy-flycheck
    :hook (global-flycheck-mode . avy-flycheck-setup)))

(provide 'init-flycheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
