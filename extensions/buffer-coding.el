;;; buffer-coding.el --- Buffer encoding and line-ending helpers -*- lexical-binding: t; -*-

;;; Code:

;;;###autoload
(defun set2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

;;;###autoload
(defun set2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;;;###autoload
(defun suk/dos2unix (&optional beg end)
  "Convert DOS CRLF line endings to UNIX LF in the active region or buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (when (and (region-active-p) (not (use-region-p)))
        (activate-mark))
      (when (use-region-p)
        (narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (let ((count 0))
        (while (search-forward "\r" nil t)
          (replace-match "" nil t)
          (setq count (1+ count)))
        (message "Removed %d CR (^M) chars" count))
      (widen)))
  (set-buffer-file-coding-system 'unix t))

;;;###autoload
(defun suk/unix2dos ()
  "Convert the current buffer to DOS CRLF line endings."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match "" nil t)))
  (set-buffer-file-coding-system 'dos t)
  (message "Buffer will be saved with DOS (CRLF) line endings"))

;;;###autoload
(defun suk/toggle-eol ()
  "Toggle current buffer line endings between DOS and UNIX."
  (interactive)
  (if (string-match-p "dos\\'" (symbol-name buffer-file-coding-system))
      (progn
        (suk/dos2unix)
        (message "Converted to UNIX (LF)"))
    (progn
      (suk/unix2dos)
      (message "Converted to DOS (CRLF)"))))

;;;###autoload
(defun suk/revert-with-coding (coding &optional set-for-save)
  "Revert buffer using CODING.  With SET-FOR-SAVE, keep the coding on save."
  (interactive (list (read-coding-system "Revert using coding: ") current-prefix-arg))
  (unless (buffer-file-name)
    (user-error "No file is currently being edited"))
  (let ((coding-system-for-read coding))
    (revert-buffer t t))
  (when set-for-save
    (set-buffer-file-coding-system coding t))
  (message "Reverted with %s%s" coding (if set-for-save ", and set as save coding" "")))

;;;###autoload
(defun suk/revert-utf8 (&optional set-for-save)
  "Revert using UTF-8."
  (interactive "P")
  (suk/revert-with-coding 'utf-8 set-for-save))

;;;###autoload
(defun suk/revert-gb18030 (&optional set-for-save)
  "Revert using GB18030."
  (interactive "P")
  (suk/revert-with-coding 'gb18030 set-for-save))

;;;###autoload
(defun suk/revert-ms932 (&optional set-for-save)
  "Revert using CP932 / Windows-31J."
  (interactive "P")
  (suk/revert-with-coding 'cp932 set-for-save))

;;;###autoload
(defun suk/revert-auto-unicode-cjk (&optional set-for-save)
  "Try several Unicode/CJK encodings until the buffer can be reverted."
  (interactive "P")
  (unless (buffer-file-name)
    (user-error "No file is currently being edited"))
  (let* ((candidates '(utf-8 utf-16 utf-16le utf-16be cp932 gb18030 gbk big5))
         (done nil)
         (used nil))
    (dolist (c candidates)
      (unless done
        (condition-case _err
            (progn
              (let ((coding-system-for-read c))
                (revert-buffer t t))
              (setq done t
                    used c))
          (error nil))))
    (if used
        (progn
          (when set-for-save
            (set-buffer-file-coding-system used t))
          (message "Auto-revert OK with %s%s"
                   used (if set-for-save ", and set as save coding" "")))
      (user-error "Couldn't detect the right encoding; try `M-x revert-buffer-with-coding-system'"))))

(provide 'buffer-coding)

;;; buffer-coding.el ends here
