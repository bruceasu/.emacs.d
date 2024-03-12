;;; init-ext-package.el --- Initialize Extension Packages configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs Package management configurations.
;;

;;; Code:

(eval-when-compile
  (require '+const)
  (require '+custom)
  (require 'init-package)
  )

;; Persistent the scratch buffer
(use-package persistent-scratch
  :diminish
  :bind (:map persistent-scratch-mode-map
			  ([remap kill-buffer] . (lambda (&rest _)
									   (interactive)
									   (user-error "Scratch buffer cannot be killed")))
			  ([remap revert-buffer] . persistent-scratch-restore)
			  ([remap revert-this-buffer] . persistent-scratch-restore))
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode))
  :init (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
			  persistent-scratch-backup-directory
			  (expand-file-name "var/persistent-scratch" user-emacs-directory)))


;; Process
(use-package proced
  :ensure nil
  :init
  (setq-default proced-format 'verbose)
  (setq proced-auto-update-flag t
        proced-auto-update-interval 3))


;; IRC
(use-package erc
  :ensure nil
  :defines erc-autojoin-channels-alist
  :init (setq erc-interpret-mirc-color t
			  erc-lurker-hide-list '("JOIN" "PART" "QUIT")
			  erc-autojoin-channels-alist '(("freenode.net" "#emacs"))))

;; text mode directory tree
(use-package ztree
  :custom-face
  (ztreep-header-face ((t (:inherit diff-header))))
  (ztreep-arrow-face ((t (:inherit font-lock-comment-face))))
  (ztreep-leaf-face ((t (:inherit diff-index))))
  (ztreep-node-face ((t (:inherit font-lock-variable-name-face))))
  (ztreep-expand-sign-face ((t (:inherit font-lock-function-name-face))))
  (ztreep-diff-header-face ((t (:inherit (diff-header bold)))))
  (ztreep-diff-header-small-face ((t (:inherit diff-file-header))))
  (ztreep-diff-model-normal-face ((t (:inherit font-lock-doc-face))))
  (ztreep-diff-model-ignored-face ((t (:inherit font-lock-doc-face :strike-through t))))
  (ztreep-diff-model-diff-face ((t (:inherit diff-removed))))
  (ztreep-diff-model-add-face ((t (:inherit diff-nonexistent))))
  :pretty-hydra
  ((:title (pretty-hydra-title "Ztree" 'octicon "nf-oct-diff" :face 'nerd-icons-green)
		   :color pink :quit-key ("q" "C-g"))
   ("Diff"
    (("C" ztree-diff-copy "copy" :exit t)
     ("h" ztree-diff-toggle-show-equal-files "show/hide equals" :exit t)
     ("H" ztree-diff-toggle-show-filtered-files "show/hide ignores" :exit t)
     ("D" ztree-diff-delete-file "delete" :exit t)
     ("v" ztree-diff-view-file "view" :exit t)
     ("d" ztree-diff-simple-diff-files "simple diff" :exit t)
     ("r" ztree-diff-partial-rescan "partial rescan" :exit t)
     ("R" ztree-diff-full-rescan "full rescan" :exit t))
    "View"
    (("RET" ztree-perform-action "expand/collapse or view" :exit t)
     ("SPC" ztree-perform-soft-action "expand/collapse or view in other" :exit t)
     ("TAB" ztree-jump-side "jump side" :exit t)
     ("g" ztree-refresh-buffer "refresh" :exit t)
     ("x" ztree-toggle-expand-subtree "expand/collapse" :exit t)
     ("<backspace>" ztree-move-up-in-tree "go to parent" :exit t))))
  :bind (:map ztreediff-mode-map
			  ("C-<f5>" . ztree-hydra/body))
  :init (setq ztree-draw-unicode-lines t
			  ztree-show-number-of-children t))

;; Misc
(use-package disk-usage
  :ensure nil)
(use-package memory-usage
  :ensure nil)

(use-package list-environment
  :init
  (with-no-warnings
    (defun my-list-environment-entries ()
	  "Generate environment variable entries list for tabulated-list."
	  (mapcar (lambda (env)
                (let* ((kv (split-string env "="))
					   (key (car kv))
					   (val (mapconcat #'identity (cdr kv) "=")))
				  (list key (vector
                             `(,key face font-lock-keyword-face)
                             `(,val face font-lock-string-face)))))
			  process-environment))
    (advice-add #'list-environment-entries :override #'my-list-environment-entries)))

(unless sys/win32p
  (use-package daemons)                 ; system services/daemons
  (use-package tldr))

;; 一些我不知道用途的依赖
(use-package slime
  :ensure t)
(use-package anaconda-mode
  :ensure t)

(use-package auto-compile
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

(provide 'init-ext-packages)
