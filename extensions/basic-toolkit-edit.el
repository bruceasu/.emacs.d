;;; basic-toolkit-edit.el --- Line and text editing helpers -*- lexical-binding: t; -*-

;;; Code:

;;;###autoload
(defun strip-blank-lines ()
  "Strip blank lines in the active region or whole buffer."
  (interactive)
  (strip-regular-expression-string "^[ \t]*\n")
  (message "Have strip blanks line. ^_^"))

;;;###autoload
(defun strip-line-number ()
  "Strip line numbers in the active region or whole buffer."
  (interactive)
  (strip-regular-expression-string "^[0-9]+")
  (message "Have strip line number. ^_^"))

;;;###autoload
(defun strip-regular-expression-string (regular-expression)
  "Strip strings that match REGULAR-EXPRESSION in the active region or buffer."
  (interactive)
  (let ((begin (point-min))
        (end (point-max)))
    (when mark-active
      (setq begin (region-beginning)
            end (region-end)))
    (save-excursion
      (goto-char end)
      (while (and (> (point) begin)
                  (re-search-backward regular-expression nil t))
        (replace-match "" t t)))))

;;;###autoload
(defun indent-buffer ()
  "Indent and clean the current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))))

;;;###autoload
(defun indent-comment-buffer ()
  "Indent comments in the current buffer."
  (interactive)
  (indent-comment-region (point-min) (point-max)))

;;;###autoload
(defun indent-comment-region (start end)
  "Indent comments in region from START to END."
  (interactive "r")
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (while (< (point) end)
      (if (comment-search-forward end t)
          (comment-indent)
        (goto-char end)))))

;;;###autoload
(defun mark-line ()
  "Mark one whole line."
  (interactive)
  (beginning-of-line)
  (if mark-active
      (exchange-point-and-mark)
    (push-mark nil nil t))
  (forward-line)
  (exchange-point-and-mark))

;;;###autoload
(defun kill-and-join-forward (&optional arg)
  "Delete empty line in select region."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn
        (forward-char 1)
        (just-one-space 0)
        (backward-char 1)
        (kill-line arg))
    (kill-line arg)))

;;;###autoload
(defun delete-chars-hungry-forward (&optional reverse)
  "Delete chars forward in a hungry style."
  (interactive)
  (delete-region
   (point)
   (progn
     (if reverse
         (skip-chars-backward " \t\n\r")
       (skip-chars-forward " \t\n\r"))
     (point))))

;;;###autoload
(defun delete-chars-hungry-backward ()
  "Delete chars backward in a hungry style."
  (interactive)
  (delete-chars-hungry-forward t))

;;;###autoload
(defun suk/delete-to-char (char)
  "Delete from point to CHAR on current line."
  (interactive "cDelete to char: ")
  (let ((start (point))
        (end (search-forward (char-to-string char) (line-end-position) t)))
    (if end
        (progn
          (goto-char end)
          (delete-region start (- end 1))
          (message "Deleted from %d to %d" start (- end 1)))
      (progn
        (delete-region start (line-end-position))
        (message "Character '%c' not found. Deleted to end of line." char)))))

;;;###autoload
(defun reverse-chars-in-region (start end)
  "Reverse the region character by character without reversing lines."
  (interactive "r")
  (let ((str (buffer-substring start end)))
    (delete-region start end)
    (dolist (line (split-string str "\n"))
      (let ((chars (mapcar (lambda (c)
                             (or (matching-paren c) c))
                           (reverse (append line nil)))))
        (when chars
          (apply #'insert chars))
        (newline)))))

;;;###autoload
(defun underline-line-with (char)
  "Insert CHAR below the current line."
  (interactive "cType one char: ")
  (save-excursion
    (let ((length (- (point-at-eol) (point-at-bol))))
      (end-of-line)
      (insert "\n")
      (insert (make-string length char)))))

;;;###autoload
(defun goto-column (number)
  "Go to column NUMBER within the current line."
  (interactive "nColumn number: ")
  (move-to-column number t))

;;;###autoload
(defun current-line-move-to-top ()
  "Move current line to top of buffer."
  (interactive)
  (recenter 0))

;;;###autoload
(defun join-lines (n)
  "Join N lines."
  (interactive "p")
  (if (use-region-p)
      (let ((fill-column (point-max)))
        (fill-region (region-beginning) (region-end)))
    (dotimes (_ (abs n))
      (delete-indentation (natnump n)))))

(global-set-key (kbd "C-S-j") #'join-lines)

(provide 'basic-toolkit-edit)

;;; basic-toolkit-edit.el ends here
