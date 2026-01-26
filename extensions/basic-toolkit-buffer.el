;;; basic-toolkit-buffer.el --- Buffer and utility helpers -*- lexical-binding: t; -*-

;;; Code:

(require 'subr-x)

;;;###autoload
(defun unmark-all-buffers ()
  "Unmark all marked buffers."
  (interactive)
  (let ((current-element (current-buffer)))
    (save-excursion
      (dolist (element (buffer-list))
        (set-buffer element)
        (deactivate-mark)))
    (switch-to-buffer current-element)
    (deactivate-mark)))

;;;###autoload
(defun prettyfy-string (string &optional after)
  "Trim STRING and optionally shorten it to AFTER chars."
  (let ((replace-map (list
                      (cons "^[ \t]*" "")
                      (cons "[ \t]*$" "")
                      (cons (concat "^\\(.\\{"
                                    (or (number-to-string after) "10")
                                    "\\}\\).*")
                            "\\1..."))))
    (dolist (replace replace-map)
      (when (string-match (car replace) string)
        (setq string (replace-match (cdr replace) nil nil string))))
    string))

;;;###autoload
(defun forward-button-with-line-begin ()
  "Move to next button with line beginning."
  (interactive)
  (call-interactively 'forward-button)
  (while (not (bolp))
    (call-interactively 'forward-button)))

;;;###autoload
(defun backward-button-with-line-begin ()
  "Move to previous button with line beginning."
  (interactive)
  (call-interactively 'backward-button)
  (while (not (bolp))
    (call-interactively 'backward-button)))

;;;###autoload
(defun goto-percent (percent)
  "Go to PERCENT of buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

;;;###autoload
(defun count-words ()
  "Count words in buffer, including Chinese."
  (interactive)
  (let ((begin (point-min))
        (end (point-max)))
    (when mark-active
      (setq begin (region-beginning)
            end (region-end)))
    (count-ce-words begin end)))

;;;###autoload
(defun count-ce-words (beg end)
  "Count Chinese and English words in BEG to END."
  (interactive "r")
  (let ((cn-word 0)
        (en-word 0)
        (total-word 0)
        (total-byte 0))
    (setq cn-word (count-matches "\\cc" beg end)
          en-word (count-matches "\\w+\\W" beg end))
    (setq total-word (+ cn-word en-word)
          total-byte (+ cn-word (abs (- beg end))))
    (message (format "Total: %d (CN: %d, EN: %d) words, %d bytes."
                     total-word cn-word en-word total-byte))))

;;;###autoload
(defun replace-match+ (object match-str replace-str)
  "Replace MATCH-STR of OBJECT with REPLACE-STR."
  (string-match match-str object)
  (replace-match replace-str nil nil object 0))

;;;###autoload
(defun refresh-file ()
  "Automatic reload current file."
  (interactive)
  (cond ((eq major-mode 'emacs-lisp-mode)
         (indent-buffer)
         (indent-comment-buffer)
         (save-buffer)
         (load-file (buffer-file-name)))
        ((member major-mode '(lisp-mode c-mode perl-mode))
         (indent-buffer)
         (indent-comment-buffer)
         (save-buffer))
        ((member major-mode '(haskell-mode sh-mode))
         (indent-comment-buffer)
         (save-buffer))
        ((derived-mode-p 'scss-mode)
         (require 'css-sort)
         (css-sort))
        (t (message "Current mode is not supported, so not reload"))))

;;;###autoload
(defun cycle-buffer-in-special-mode (special-mode)
  "Cycle through buffers derived from SPECIAL-MODE."
  (require 'cycle-buffer)
  (catch 'done
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p special-mode)
          (throw 'done (switch-to-buffer buffer)))))))

;;;###autoload
(defun kill-unused-buffers ()
  (interactive)
  (ignore-errors
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (when (and (string-prefix-p "*" (buffer-name))
                   (string-suffix-p "*" (buffer-name)))
          (kill-buffer buf))))))

;;;###autoload
(defun suk/xah-narrow-to-region ()
  "Like `narrow-to-region', but use the current block if there is no region."
  (interactive)
  (if (region-active-p)
      (narrow-to-region (region-beginning) (region-end))
    (let ($p1 $p2)
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (goto-char (match-end 0))
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward "\n[ \t]*\n" nil "move")
            (progn (goto-char (match-beginning 0))
                   (setq $p2 (point)))
          (setq $p2 (point))))
      (narrow-to-region $p1 $p2))))

;;;###autoload
(defun suk/fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph or REGION.  With UNFILL, unfill instead."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
    (fill-paragraph nil region)))

(global-set-key (kbd "C-x n N") #'suk/xah-narrow-to-region)
(global-set-key (kbd "M-q") #'suk/fill-or-unfill-paragraph)

(provide 'basic-toolkit-buffer)

;;; basic-toolkit-buffer.el ends here
