;;; basic-toolkit.el --- Loader for editing toolkit helpers -*- lexical-binding: t; -*-

;;; Code:

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "basic-toolkit-edit.el" dir) nil t)
  (load (expand-file-name "basic-toolkit-buffer.el" dir) nil t))

(provide 'basic-toolkit)

;;; basic-toolkit.el ends here
