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

(defun insert-line-number (beg end &optional start-line)
  "Insert line numbers into buffer."
  (interactive "r")
  (save-excursion
    (let ((max (count-lines beg end))
          (line (or start-line 1))
          (counter 1))
      (goto-char beg)
      (while (<= counter max)
        (insert (format "%0d " line))
        (beginning-of-line 2)
        (cl-incf line)
        (cl-incf counter)))))

(defun insert-line-number+ ()
  "Insert line number into buffer."
  (interactive)
  (if mark-active
      (insert-line-number (region-beginning) (region-end) (read-number "Start line: "))
    (insert-line-number (point-min) (point-max))))

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

(defun comment-part-move-up (n)
  "Move comment part up."
  (interactive "p")
  (comment-part-move (or (- n) -1)))

(defun comment-part-move-down (n)
  "Move comment part down."
  (interactive "p")
  (comment-part-move (or n 1)))

(defun comment-part-move (&optional n)
  "Move comment part."
  (or n (setq n 1))
  (let (cmt-current cmt-another cs-current cs-another)
    ;; If current line have comment, paste it.
    (setq cmt-current (comment-paste))
    (when cmt-current
      (setq cs-current (current-column)))
    ;; If another line have comment, paste it.
    (forward-line n)
    (setq cmt-another (comment-paste))
    (when cmt-another
      (setq cs-another (current-column)))
    ;; Paste another comment in current line.
    (forward-line (- n))
    (when cmt-another
      (if cs-current
          (move-to-column cs-current t)
        (end-of-line))
      (insert cmt-another))
    ;; Paste current comment in another line.
    (forward-line n)
    (when cmt-current
      (if cs-another
          (move-to-column cs-another t)
        (end-of-line))
      (insert cmt-current))
    ;; Indent comment, from up to down.
    (if (> n 0)
        (progn                          ;comment move down
          (forward-line (- n))
          (if cmt-another (comment-indent))
          (forward-line n)
          (if cmt-current (comment-indent)))
      (if cmt-current (comment-indent)) ;comment move up
      (save-excursion
        (forward-line (- n))
        (if cmt-another (comment-indent))))))

(defun comment-paste ()
  "Paste comment part of current line.
If have return comment, otherwise return nil."
  (let (cs ce cmt)
    (setq cs (comment-on-line-p))
    (if cs                             ;If have comment start position
        (progn
          (goto-char cs)
          (skip-syntax-backward " ")
          (setq cs (point))
          (comment-forward)
          (setq ce (if (bolp) (1- (point)) (point))) ;get comment end position
          (setq cmt (buffer-substring cs ce))        ;get comment
          (kill-region cs ce) ;kill region between comment start and end
          (goto-char cs)      ;revert position
          cmt)
      nil)))

(defun comment-on-line-p ()
  "Whether have comment part on current line.
If have comment return COMMENT-START, otherwise return nil."
  (save-excursion
    (beginning-of-line)
    (comment-search-forward (line-end-position) t)))

(defun comment-dwim-next-line (&optional reversed)
  "Move to next line and comment dwim.
Optional argument REVERSED default is move next line, if reversed is non-nil move previous line."
  (interactive)
  (if reversed
      (call-interactively 'previous-line)
    (call-interactively 'next-line))
  (call-interactively 'comment-dwim))

(defun comment-dwim-prev-line ()
  "Move to previous line and comment dwim."
  (interactive)
  (comment-dwim-next-line 't))

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

(defun upcase-char (arg)
  "Uppercase for character."
  (interactive "P")
  (upcase-region (point) (+ (point) (or arg 1)))
  (forward-char (or arg 1)))

(defun downcase-char (arg)
  "Downcase for character."
  (interactive "P")
  (downcase-region (point) (+ (point) (or arg 1)))
  (forward-char (or arg 1)))

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
  (delete-region
   (point)
   (progn
     (if reverse
         (skip-chars-backward " \t\n\r")
       (skip-chars-forward " \t\n\r"))
     (point))))

(defun delete-chars-hungry-backward ()
  "Delete chars backward use `hungry' style."
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

(defun only-comment-p ()
  "Return t if only comment in current line.
Otherwise return nil."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (search-forward comment-start (line-end-position) t)
        (progn
          (backward-char (length comment-start))
          (equal (point)
                 (progn
                   (back-to-indentation)
                   (point))))
      nil)))

(defun current-line-move-to-top()
  "Move current line to top of buffer."
  (interactive)
  (recenter 0))

(defun remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defun point-stack-push ()
  "Push current point in stack."
  (interactive)
  (message "Location marked.")
  (setq point-stack (cons (list (current-buffer) (point)) point-stack)))

(defun point-stack-pop ()
  "Pop point from stack."
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (switch-to-buffer (caar point-stack))
    (goto-char (cadar point-stack))
    (setq point-stack (cdr point-stack))))

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

(defun goto-percent (percent)
  "Goto PERCENT of buffer."
  (interactive "nGoto percent: ")
  (goto-char (/ (* percent (point-max)) 100)))

(defun replace-match+ (object match-str replace-str)
  "Replace `MATCH-STR' of `OBJECT' with `REPLACE-STR'."
  (string-match match-str object)
  (replace-match replace-str nil nil object 0))

(defun forward-indent ()
  "Backward indent."
  (interactive)
  (insert-string "    "))

(defun backward-indent ()
  "Backward indent."
  (interactive)
  (goto-column (- (current-column) 4)))

(defun scroll-up-one-line()
  "Scroll up one line."
  (interactive)
  (scroll-up 1))

(defun scroll-down-one-line()
  "Scroll down one line."
  (interactive)
  (scroll-down 1))

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

(defun find-file-root (file)
  "Find file with root."
  (interactive "fFind file as sudo: ")
  (require 'tramp)
  (tramp-cleanup-all-connections)
  (find-file (concat "/sudo:root@localhost:" file)))

(defun find-file-smb(file)
  "Access file through samba protocol."
  (interactive "fFind file as samba: ")
  (find-file (concat "/smb:" file)))

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
;; =========================================================
;;;###autoload
(defun suk/indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
;;(global-set-key (kbd "C-S-f")  #'suk/indent-buffer)
;;(global-set-key [S-f7] 'indent-buffer)

;;;###autoload
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

;; =========================================================
;; ¶ÎÂä¸ñÊ½»¯
;; ---------------------------------------------------------
;;;###autoload
(defun suk/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph (or REGION) and make it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

;;(global-set-key (kbd "M-Q" ) #'suk/unfill-paragraph)

;; M-q will fill the paragraph normally, and C-u M-q will unfill it.
;; --------------------------------------------------------------
;;;###autoload
(defun suk/fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph (or REGION).
With the prefix argument UNFILL, unfill it instead."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
    (fill-paragraph nil region)))
;;(global-set-key (kbd "M-q") #'suk/fill-or-unfill-paragraph)


;; @see http://endlessparentheses.com/super-smart-capitalization.html
;;;###autoload
(defun endless/convert-punctuation (rg rp)
  "Look for regexp RG around point, and replace with RP.
   Only applies to text-mode."
  (let* ((f "\\(%s\\)\\(%s\\)")
         (space "?:[[:blank:]\n\r]*"))
    ;; We obviously don't want to do this in prog-mode.
    (if (and (derived-mode-p 'text-mode)
             (or (looking-at (format f space rg))
                 (looking-back (format f rg space) (point-min))))
        (replace-match rp nil nil nil 1))))
        

;;;###autoload
(defun endless/call-subword-cmd (fn)
  (my-ensure 'subword)
  (call-interactively fn))

;;;###autoload
(defun endless/capitalize ()
  "Capitalize region or word.
   Also converts commas to full stops, and kills
   extraneous space at beginning of line."
  (interactive)
  (endless/convert-punctuation "," ".")
  (if (use-region-p)
      (call-interactively 'capitalize-region)
    ;; A single space at the start of a line:
    (when (looking-at "^\\s-\\b")
      ;; get rid of it!
      (delete-char 1))
    (endless/call-subword-cmd 'subword-capitalize)))
;;;###autoload
(defun endless/downcase ()
  "Downcase region or word.
   Also converts full stops to commas."
  (interactive)
  (endless/convert-punctuation "\\." ",")
  (if (use-region-p)
      (call-interactively 'downcase-region)
    (endless/call-subword-cmd 'subword-downcase)))
;;;###autoload
(defun endless/upcase ()
  "Upcase region or word."
  (interactive)
  (if (use-region-p)
      (call-interactively 'upcase-region)
    (endless/call-subword-cmd 'subword-upcase)))

;; these bindings are fine
(global-set-key (kbd "M-c") 'endless/capitalize)
(global-set-key (kbd "M-l") 'endless/downcase)
(global-set-key (kbd "M-u") 'endless/upcase)
(provide 'basic-toolkit)

;;; basic-toolkit.el ends here
