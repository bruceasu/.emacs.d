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

;; gnu https://elpa.gnu.org/packages/ https://elpa.emacs-china.org/gnu/ https://mirrors.163.com/elpa/gnu/ https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/
;; melpa https://melpa.org/packages/ https://www.mirrorservice.org/sites/melpa.org/packages/ https://elpa.emacs-china.org/melpa/ https://mirrors.163.com/elpa/melpa/ https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/
'(package-archives
   '(("melpa" . "http://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")
     ("gnu" . "https://elpa.gnu.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
     
(setq package-check-signature nil) ; 个别时候会出现签名校验失败

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

(use-package daemons
  :defer 1)								; system services/daemons
(use-package htmlize
  :defer 2)								; covert to html


;; On-the-fly spell checker
(unless sys/win32p
  (use-package flyspell
    :ensure nil
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


(provide 'init-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
