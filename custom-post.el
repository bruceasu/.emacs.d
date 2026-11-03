;;( suk/create-scratch-buffer)
(defun org-export-docx ()
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
        (template-file (concat suk-emacs-root-dir "/share/template/template.docx")))
    (shell-command (format "pandoc %s -o %s --reference-doc=%s" (buffer-file-name) docx-file template-file))
    (message "Convert finish: %s" docx-file)))


(global-set-key (kbd "C-S-w") #'org-export-docx)
