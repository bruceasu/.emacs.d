;;; basic-function.el --- Loader for shared basic functions -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep the historical feature name `basic-function' as a thin loader.

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "basic-function-core.el" dir) nil t)
  (load (expand-file-name "basic-function-io.el" dir) nil t))

(provide 'basic-function)

;;; basic-function.el ends here
