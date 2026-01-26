;;; init-org.el --- Entry point for org modules -*- lexical-binding: t; -*-

;;; Code:

(require 'basic-function)
(require 'org)

(defvar org-files-directory
  (expand-file-name "org"
                    (if (boundp 'user-home-dir)
                        user-home-dir
                      user-emacs-directory))
  "Directory that stores Org files.")

(defvar suk-emacs-config-dir
  (expand-file-name "etc" user-emacs-directory)
  "Directory that stores Org-related helper files when not launched from init.el.")

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load-if-exists (expand-file-name "init-org-task.el" dir))
  (load-if-exists (expand-file-name "init-org-shortcuts.el" dir))
  (load-if-exists (expand-file-name "init-org-export.el" dir))
  (load-if-exists (expand-file-name "init-org-nav.el" dir))
  (load-if-exists (expand-file-name "init-org-doc.el" dir))
  (load-if-exists (expand-file-name "init-org-hydra.el" dir))
  (load-if-exists (expand-file-name "init-org-tools.el" dir)))

(provide 'init-org)

;;; init-org.el ends here
