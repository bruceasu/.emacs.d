;;; buffer-life.el --- Buffer lifecycle helpers -*- lexical-binding: t; -*-

;;; Code:

(defvar suk/recently-closed-buffers nil
  "Recently closed buffers, as (NAME . FILE) pairs.")

(defvar suk/recently-closed-buffers-max 40
  "Maximum length of `suk/recently-closed-buffers'.")

;;;###autoload
(defun suk/close-current-buffer ()
  "Close the current buffer with save prompts when needed."
  (interactive)
  (let (($emacs-buff-p
         ($org-p (string-match "^\\*Org Src" (buffer-name)))))
    (setq $emacs-buff-p (if (string-match "^\\*" (buffer-name)) t nil))
    (if (string= major-mode "minibuffer-inactive-mode")
        (minibuffer-keyboard-quit)
      (progn
        (when (and (buffer-modified-p)
                   (not $emacs-buff-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction (widen) (buffer-string)))
                           nil
                         t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        (when (and (buffer-modified-p) $org-p)
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (org-edit-src-save)
            (set-buffer-modified-p nil)))
        (when (buffer-file-name)
          (setq suk/recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name)) suk/recently-closed-buffers))
          (when (> (length suk/recently-closed-buffers) suk/recently-closed-buffers-max)
            (setq suk/recently-closed-buffers
                  (butlast suk/recently-closed-buffers 1))))
        (kill-buffer (current-buffer))))))

;;;###autoload
(defun suk/open-last-closed ()
  "Open the last closed file."
  (interactive)
  (if (> (length suk/recently-closed-buffers) 0)
      (find-file (cdr (pop suk/recently-closed-buffers)))
    (message "No recently close buffer in this session.")))

;;;###autoload
(defun suk/open-recently-closed ()
  "Open a recently closed file interactively."
  (interactive)
  (find-file (ido-completing-read "open:" (mapcar (lambda (f) (cdr f))
                                                   suk/recently-closed-buffers))))

;;;###autoload
(defun suk/list-recently-closed ()
  "List recently closed files."
  (interactive)
  (let ((buf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer buf)
    (mapc (lambda (f) (insert (cdr f) "\n")) suk/recently-closed-buffers)))

;;;###autoload
(defun suk/kill-all-buffers-except-current ()
  "Kill every buffer except the current one."
  (interactive)
  (let ((current-buf (current-buffer)))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (unless (eq current-buf buffer)
        (kill-buffer buffer)))))

;;;###autoload
(defun suk/kill-other-window-buffer ()
  "Kill the buffer in the other window."
  (interactive)
  (other-window +1)
  (kill-this-buffer)
  (other-window -1))

;;;###autoload
(defun suk/new-empty-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (text-mode)
    (setq buffer-offer-save t)
    buf))

;;;###autoload
(defun suk/new-scratch-buffer ()
  "Create or switch to a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(defalias 'suk/create-scratch-buffer #'suk/new-empty-buffer)

(provide 'buffer-life)

;;; buffer-life.el ends here
