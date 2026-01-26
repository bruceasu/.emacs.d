;;; buffer-file.el --- File-oriented buffer helpers -*- lexical-binding: t; -*-

;;; Code:

;;;###autoload
(defun suk/delete-this-file ()
  "Delete the current file and kill its buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?" (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;;;###autoload
(defun suk--rename-or-move (new-path &optional ok-if-exists)
  "Rename or move current file to NEW-PATH."
  (unless (buffer-file-name)
    (user-error "No file is currently being edited"))
  (let* ((old (buffer-file-name))
         (dst (expand-file-name new-path))
         (dst-dir (file-name-directory dst)))
    (unless (file-directory-p dst-dir)
      (make-directory dst-dir t))
    (condition-case _err
        (rename-file old dst ok-if-exists)
      (file-error
       (progn
         (when (and (file-exists-p dst) (not ok-if-exists))
           (user-error "Destination exists: %s" dst))
         (copy-file old dst t)
         (delete-file old))))
    (set-visited-file-name (expand-file-name new-path) t t)
    (rename-buffer (file-name-nondirectory (buffer-file-name)) t)
    (set-buffer-modified-p nil)
    (buffer-file-name)))

;;;###autoload
(defun suk/rename-or-move-file-here (new-path &optional overwrite)
  "Rename or move current file to NEW-PATH."
  (interactive
   (let* ((old (or (buffer-file-name) (user-error "No file is currently being edited")))
          (prompt (format "Move/Rename file to: %s" (file-name-directory old))))
     (list (read-file-name prompt (file-name-directory old) old nil
                           (file-name-nondirectory old))
           current-prefix-arg)))
  (suk--rename-or-move new-path overwrite))

;;;###autoload
(defun suk/move-file-to-dir (dir &optional overwrite)
  "Move current file into DIR."
  (interactive
   (list (read-directory-name "Move to directory: " nil nil t) current-prefix-arg))
  (let* ((name (file-name-nondirectory
                (or (buffer-file-name) (user-error "No file is currently being edited"))))
         (dst (expand-file-name name dir)))
    (suk--rename-or-move dst overwrite)))

;;;###autoload
(defun suk/copy-file-name ()
  "Copy current file name or directory to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied '%s'" filename))
      (warn "Current buffer is not attached to a file!"))))

(provide 'buffer-file)

;;; buffer-file.el ends here
