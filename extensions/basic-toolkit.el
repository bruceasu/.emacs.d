;;; basic-toolkit.el --- Basic edit toolkit.
;;; Code:

(defun unmark-all-buffers ()
  "Unmark all have marked buffers."
  (interactive)
  (let ((current-element (current-buffer)))
    (save-excursion
      (dolist (element (buffer-list))
        (set-buffer element)
        (deactivate-mark)))
    (switch-to-buffer current-element)
    (deactivate-mark)))

(defun strip-blank-lines()
  "Strip all blank lines in select area of buffer,
if not select any area, then strip all blank lines of buffer."
  (interactive)
  (strip-regular-expression-string "^[ \t]*\n")
  (message "Have strip blanks line. ^_^"))

(defun strip-line-number()
  "Strip all line number in select area of buffer,
if not select any area, then strip all line number of buffer."
  (interactive)
  (strip-regular-expression-string "^[0-9]+")
  (message "Have strip line number. ^_^"))

(defun strip-regular-expression-string (regular-expression)
  "Strip all string that match REGULAR-EXPRESSION in select area of buffer.
If not select any area, then strip current buffer"
  (interactive)
  (let ((begin (point-min))     ;initialization make select all buffer
        (end (point-max)))
    (if mark-active ;if have select some area of buffer, then strip this area
        (setq begin (region-beginning)
              end (region-end)))
    (save-excursion                     ;save position
      (goto-char end)                   ;goto end position
      (while (and (> (point) begin)     ;when above beginning position
                  (re-search-backward regular-expression nil t)) ;and find string that match regular expression
        (replace-match "" t t)))))    ;replace target string with null


(defun indent-buffer ()
  "Automatic format current buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)
    (delete-trailing-whitespace)
    (untabify (point-min) (point-max))))

(defun indent-comment-buffer ()
  "Indent comment of buffer."
  (interactive)
  (indent-comment-region (point-min) (point-max)))

(defun indent-comment-region (start end)
  "Indent region."
  (interactive "r")
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (while (< (point) end)
      (if (comment-search-forward end t)
          (comment-indent)
        (goto-char end)))))


(defun mark-line ()
  "Mark one whole line, similar to `mark-paragraph'."
  (interactive)
  (beginning-of-line)
  (if mark-active
      (exchange-point-and-mark)
    (push-mark nil nil t))
  (forward-line)
  (exchange-point-and-mark))

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

(defun delete-chars-hungry-forward (&optional reverse)
  "Delete chars forward use `hungry' style.
Optional argument REVERSE default is delete forward, if reverse is non-nil delete backward."
  (interactive)
  (delete-region
   (point)
   (progn
     (if reverse
         (skip-chars-backward " \t\n\r")
       (skip-chars-forward " \t\n\r"))
     (point))))

(defun delete-chars-hungry-backward ()
  "Delete chars backward use `hungry' style."
  (interactive)
  (delete-chars-hungry-forward t))

(defun reverse-chars-in-region (start end)
  "Reverse the region character by character without reversing lines."
  (interactive "r")
  (let ((str (buffer-substring start end)))
    (delete-region start end)
    (dolist (line (split-string str "\n"))
      (let ((chars (mapcar (lambda (c)
                             (or (matching-paren c)
                                 c))
                           (reverse (append line nil)))))
        (when chars
          (apply 'insert chars))
        (newline)))))

(defun underline-line-with (char)
  "Insert some char below at current line."
  (interactive "cType one char: ")
  (save-excursion
    (let ((length (- (point-at-eol) (point-at-bol))))
      (end-of-line)
      (insert "\n")
      (insert (make-string length char)))))

(defun prettyfy-string (string &optional after)
  "Strip starting and ending whitespace and pretty `STRING'.
Replace any chars after AFTER with '...'.
Argument STRING the string that need pretty."
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

(defun forward-button-with-line-begin ()
  "Move to next button with line begin."
  (interactive)
  (call-interactively 'forward-button)
  (while (not (bolp))
    (call-interactively 'forward-button)))

(defun backward-button-with-line-begin ()
  "Move to previous button with line begin."
  (interactive)
  (call-interactively 'backward-button)
  (while (not (bolp))
    (call-interactively 'backward-button)))

(defun goto-column (number)
  "Untabify, and go to a column NUMBER within the current line (0 is beginning of the line)."
  (interactive "nColumn number: ")
  (move-to-column number t))


(defun goto-percent (percent)
  "Goto PERCENT of buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

(defun current-line-move-to-top()
  "Move current line to top of buffer."
  (interactive)
  (recenter 0))


(defun count-words ()
  "Count the number of word in buffer, include Chinese."
  (interactive)
  (let ((begin (point-min))
        (end (point-max)))
    (if mark-active
        (setq begin (region-beginning)
              end (region-end)))
    (count-ce-words begin end)))

(defun count-ce-words (beg end)
  "Count Chinese and English words in marked region."
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


(defun replace-match+ (object match-str replace-str)
  "Replace `MATCH-STR' of `OBJECT' with `REPLACE-STR'."
  (string-match match-str object)
  (replace-match replace-str nil nil object 0))

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

(defun cycle-buffer-in-special-mode (special-mode)
  "Cycle in special mode."
  (require 'cycle-buffer)
  (catch 'done
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p special-mode)
          (throw 'done (switch-to-buffer buffer)))))))


(defun kill-unused-buffers ()
  (interactive)
  (ignore-errors
    (save-excursion
      (dolist (buf (buffer-list))
        (set-buffer buf)
        (if (and (string-prefix-p "*" (buffer-name)) (string-suffix-p "*" (buffer-name)))
            (kill-buffer buf))
        ))))

(defun join-lines (n)
  "Join N lines."
  (interactive "p")
  (if (use-region-p)
      (let ((fill-column (point-max)))
        (fill-region (region-beginning) (region-end)))
    (dotimes (_ (abs n))
      (delete-indentation (natnump n)))))
(global-set-key (kbd "C-S-j") #'join-lines)


(defun suk/xah-narrow-to-region ()
  "Same as `narrow-to-region', but if no selection, narrow to the current block.
Version 2022-01-22"
  (interactive)
  (if (region-active-p)
      (progn
        (narrow-to-region (region-beginning) (region-end)))
    (progn
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
        (narrow-to-region $p1 $p2)))))
(global-set-key (kbd "C-x n N") #'suk/xah-narrow-to-region)


;; M-q will fill the paragraph normally, and C-u M-q will unfill it.
;; --------------------------------------------------------------
(defun suk/fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph (or REGION).
With the prefix argument UNFILL, unfill it instead."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
    (fill-paragraph nil region)))
(global-set-key (kbd "M-q") #'suk/fill-or-unfill-paragraph)


(provide 'basic-toolkit)

;;; basic-toolkit.el ends here
