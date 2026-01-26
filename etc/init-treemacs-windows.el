;;; init-treemacs-windows.el --- Windows helpers for treemacs -*- lexical-binding: t; -*-

;;; Code:

(defun suk/treemacs-windows-python-executable ()
  "Return a usable Python executable for Treemacs on Windows."
  (seq-some
   (lambda (name)
     (when-let ((path (executable-find name)))
       ;; Windows Store app aliases are placeholders and cannot be used by
       ;; Treemacs as child processes.
       (unless (string-match-p "/Microsoft/WindowsApps/"
                               (subst-char-in-string ?\\ ?/ path))
         path)))
   '("python.exe" "python3.exe" "python")))

(defun suk/treemacs-windows-git-mode ()
  "Return a conservative Treemacs Git mode for Windows."
  (cond
   ((executable-find "git.exe") 'simple)
   ((executable-find "git") 'simple)
   (t nil)))

(provide 'init-treemacs-windows)

;;; init-treemacs-windows.el ends here
