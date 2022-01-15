(setq package-user-dir
      (expand-file-name (format ".cask/%s/elpa" emacs-version)))
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))
(add-to-list 'load-path default-directory)

