;; init-treemacs.el --- Initialize treemacs. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Treemacs: a tree layout file explorer.
;;

;;; Code:

(require 'basic-function)

(when (and (boundp 'sys/win32p) sys/win32p)
  (require 'init-treemacs-windows nil t))

(defun suk/treemacs-python-executable ()
  "Return a usable Python executable for Treemacs, or nil."
  (if (and (boundp 'sys/win32p) sys/win32p)
      (and (fboundp 'suk/treemacs-windows-python-executable)
           (suk/treemacs-windows-python-executable))
    (seq-some #'executable-find '("python3" "python"))))

(when (locate-library "treemacs")
  (use-package treemacs
    :demand t
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
    (setq treemacs-python-executable (suk/treemacs-python-executable)
          treemacs-collapse-dirs     (if treemacs-python-executable 3 0)
          treemacs-missing-project-action  'remove
          treemacs-sorting                 'alphabetic-asc
          treemacs-follow-after-init       t
          treemacs-width                   30
          treemacs-no-png-images           nil)

    (treemacs-follow-mode t)
    ;; Windows file watching is more fragile, so keep it conservative.
    (if (and (boundp 'sys/win32p) sys/win32p)
        (when (fboundp 'suk/treemacs-windows-git-mode)
          (let ((mode (suk/treemacs-windows-git-mode)))
            (when mode
              (treemacs-git-mode mode))))
      (treemacs-filewatch-mode t)
      (pcase (cons (not (null (executable-find "git")))
                   (not (null (executable-find "python3"))))
        (`(t . t)
         (treemacs-git-mode 'deferred))
        (`(t . _)
         (treemacs-git-mode 'simple))))

    (when (and (fboundp 'icons-displayable-p)
               (icons-displayable-p)
               (locate-library "treemacs-nerd-icons"))
      (use-package treemacs-nerd-icons
        :demand t
        :custom-face
        (treemacs-nerd-icons-root-face ((t (:inherit nerd-icons-green :height 1.3))))
        (treemacs-nerd-icons-file-face ((t (:inherit nerd-icons-dsilver))))
        :config
        (treemacs-load-theme "nerd-icons")))

    (when (locate-library "treemacs-magit")
      (with-eval-after-load 'magit
        (use-package treemacs-magit
          :hook ((magit-post-commit
                  git-commit-post-finish
                  magit-post-stage
                  magit-post-unstage)
                 . treemacs-magit--schedule-update))))

    (when (locate-library "treemacs-tab-bar")
      (use-package treemacs-tab-bar
        :ensure t
        :demand t
        :config
        (treemacs-set-scope-type 'Tabs)))))

(when (fboundp 'treemacs-project-follow-mode)
  (add-hook 'treemacs-mode-hook #'treemacs-project-follow-mode))

(provide 'init-treemacs)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
