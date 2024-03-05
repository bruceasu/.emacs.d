;; Recentf
(use-package recentf
  :ensure nil
  :defer 1
  :init
  (setq recentf-save-file (concat suk-emacs-var-dir "/recentf"))
  ;;(setq recentf-save-file "~/.emacs.d/var/recentf")
  ;;(add-hook 'after-init-hook #'recentf-mode)
  (setq recentf-max-saved-items 500)
  (setq recentf-max-saved-items 17)
  (recentf-mode)
  (recentf-track-opened-file)

  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude ".cache")
  (add-to-list 'recentf-exclude ".cask")
  (add-to-list 'recentf-exclude ".elfeed")
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "cache")
  (add-to-list 'recentf-exclude "persp-confs")
  (add-to-list 'recentf-exclude "recentf")
  (add-to-list 'recentf-exclude "url")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
  (defun suk/recentf-exclude-p (file)
    (let ((file-dir (file-truename (file-name-directory file))))
      (-any-p (lamdba (dir)
                      (string-prefix-p dir file-dir))
              (mapcar 'file-truename (list var package-user-dir)))))
  (add-to-list 'recentf-exclude #'suk/recentf-exclude-p)
  )

(provide 'init-recentf)  