;;; init-coding.el --- Entry point for coding tools -*- lexical-binding: t; -*-

;;; Code:

(require 'basic-function)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load-if-exists (expand-file-name "leader-key.el" dir))
  ;; Reuse the shared coding layers.
  (load-if-exists (expand-file-name "init-coding-web.el" dir))
  (require 'init-treemacs nil t)
  ;; New coding-specific layers.
  (load-if-exists (expand-file-name "init-coding-core.el" dir))
  (load-if-exists (expand-file-name "init-coding-lsp.el" dir))
  (load-if-exists (expand-file-name "init-coding-nav.el" dir))
  (load-if-exists (expand-file-name "init-coding-debug.el" dir))
  (load-if-exists (expand-file-name "init-coding-files.el" dir)))

(provide 'init-coding)

;;; init-coding.el ends here
