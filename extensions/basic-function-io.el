;;; basic-function-io.el --- File, process, and string helpers -*- lexical-binding: t; -*-

;;; Code:

;;;###autoload
(defun run-cmd-and-replace-region (cmd)
  "Run CMD on the active region or whole buffer and replace it with output."
  (let* ((orig-point (point))
         (b (if (region-active-p) (region-beginning) (point-min)))
         (e (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region b e cmd nil t)
    (goto-char orig-point)))

;;;###autoload
(defun suk/buffer-str ()
  "Return the current buffer as a plain string."
  (buffer-substring-no-properties (point-min) (point-max)))

;;;###autoload
(defun suk/get-string-from-file (file)
  "Return FILE's content as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;;###autoload
(defun suk/read-lines (file)
  "Return FILE as a list of lines."
  (split-string (suk/get-string-from-file file) "\n" t))

;;;###autoload
(defun path-in-directory-p (file directory)
  "Return FILE when it is inside DIRECTORY, otherwise nil."
  (let* ((pattern (concat "^" (file-name-as-directory directory))))
    (if (string-match pattern file) file)))

;;;###autoload
(defun suk/send-string-to-cli-stdin (string program)
  "Send STRING to PROGRAM's stdin."
  (with-temp-buffer
    (insert string)
    (call-process-region (point-min) (point-max) program)))

;;;###autoload
(defun suk/write-string-to-file (string file)
  "Write STRING to FILE."
  (with-temp-buffer
    (insert string)
    (write-region (point-min) (point-max) file)))

;;;###autoload
(defun suk/async-shell-command (command)
  "Execute COMMAND asynchronously."
  (let ((proc (start-process "Shell" nil shell-file-name shell-command-switch command)))
    (set-process-sentinel
     proc
     `(lambda (process signal)
        (let ((status (process-status process)))
          (when (memq status '(exit signal))
            (unless (string= (substring signal 0 -1) "finished")
              (message "Failed to run \"%s\"." ,command))))))))

(provide 'basic-function-io)

;;; basic-function-io.el ends here
