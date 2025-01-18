;; init-treemacs.el --- Initialize treemacs.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Treemacs: A tree layout file explorer.
;;

;;; Code:

;; 设置 Python 可执行文件路径
(when sys/win32p
  (setq python-shell-interpreter "c:/green/Python311/python.exe")
  (setq treemacs-python-executable "c:/green/Python311/python.exe")
  )

;; A tree layout file explorer
(use-package treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-git-mode)
  :custom-face
  (cfrs-border-color ((t (:inherit posframe-border))))
  :bind (([f8]        . treemacs)
         ("M-0"       . treemacs-select-window)
         ("C-x t 1"   . treemacs-delete-other-windows)
         ("C-x t t"   . treemacs)
         ("C-x t b"   . treemacs-bookmark)
         ("C-x t C-t" . treemacs-find-file)
         ("C-x t M-t" . treemacs-find-tag)
         :map treemacs-mode-map
         ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-missing-project-action  'remove
        treemacs-sorting                 'alphabetic-asc
        treemacs-follow-after-init       t
        treemacs-width                   30
        treemacs-no-png-images           (not suk-icon))

  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (when sys/win32p
    (pcase (cons (not (null (executable-find "git.exe")))
                 (not (null (executable-find "c:/green/Python311/python.exe"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))
    )
  (unless sys/win32p
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))
    )

  (with-eval-after-load 'nerd-icons
	(use-package treemacs-nerd-icons
	  :demand t
	  :when (icons-displayable-p)
	  :custom-face
	  (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
	  (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
	  :config (treemacs-load-theme "nerd-icons"))
    )

  (with-eval-after-load 'magit
	(use-package treemacs-magit
	  :hook ((magit-post-commit
	          git-commit-post-finish
	          magit-post-stage
	          magit-post-unstage)
	         . treemacs-magit--schedule-update))
    )


  (use-package treemacs-tab-bar
    :ensure t
    :demand t
    :config (treemacs-set-scope-type 'Tabs)))

(add-hook 'treemacs-mode-hook 'treemacs-project-follow-mode)
(provide 'init-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
