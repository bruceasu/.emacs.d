;;; init-coding.el --- Entry point for coding tools -*- lexical-binding: t; -*-

;;; Code:

(require 'basic-function)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (module '("init-coding-core.el"
                    "init-coding-web.el"
                    "init-coding-lsp.el"
                    "init-coding-nav.el"
                    "init-coding-debug.el"
                    "init-coding-files.el"))
    (condition-case err
        (load-if-exists (expand-file-name module dir))
      (error
       (display-warning 'suk
                        (format "Coding module %s failed: %s"
                                module (error-message-string err)))))))

(provide 'init-coding)

;;; init-coding.el ends here
