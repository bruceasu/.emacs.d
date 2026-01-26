;;; init-devtools.el --- Compatibility loader for coding web support -*- lexical-binding: t; -*-

;;; Code:

(require 'basic-function)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load-if-exists (expand-file-name "init-coding-web.el" dir)))

(provide 'init-devtools)

;;; init-devtools.el ends here
