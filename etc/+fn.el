;; -*- coding: utf-8; lexical-binding: t; -*-
(eval-when-compile
  (require '+const)
  (require '+custom)
  )

(provide '+fn)


(defmacro suk/timer (&rest body)
  "Measure the time of code BODY running."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))


;;;###autoload
(defun childframe-workable-p ()
  "Whether childframe is workable."
  (not (or noninteractive
           emacs-basic-display
           (not (display-graphic-p)))))
;;;###autoload
(defun childframe-completion-workable-p ()
  "Whether childframe completion is workable."
  (and (eq suk-completion-style 'childframe)
       (childframe-workable-p)))
;;;###autoload
(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (and suk-icon
       (or (featurep 'nerd-icons)
           (require 'nerd-icons nil t))))
;;;###autoload
(defun suk-treesit-available-p ()
  "Check whether tree-sitter is available.
Native tree-sitter is introduced since 29.1."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))
;;;###autoload
(defun too-long-file-p ()
  "Check whether the file is too long."
  (or (> (buffer-size) 100000)
      (and (fboundp 'buffer-line-statistics)
           (> (car (buffer-line-statistics)) 10000))))
;;;###autoload
(defun my-ensure (feature)
  "Make sure FEATURE is required."
  (unless (featurep feature)
    (condition-case nil
        (require feature)
      (error nil))))

;;;###autoload
(defun run-cmd-and-replace-region (cmd)
  "Run CMD in shell on selected region or current buffer.
Then replace the region or buffer with cli output."
  (let* ((orig-point (point))
         (b (if (region-active-p) (region-beginning) (point-min)))
         (e (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region b e cmd nil t)
    (goto-char orig-point)))


;; {{ copied from http://ergoemacs.org/emacs/elisp_read_file_content.html
;;;###autoload
(defun my-get-string-from-file (file)
  "Return FILE's content."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))
;;;###autoload
(defun my-read-lines (file)
  "Return a list of lines of FILE."
  (split-string (my-get-string-from-file file) "\n" t))
;; }}


;;;###autoload
(defun path-in-directory-p (file directory)
  "FILE is in DIRECTORY."
  (let* ((pattern (concat "^" (file-name-as-directory directory))))
    (if (string-match pattern file) file)))

;;;###autoload
(defun my-buffer-str ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defvar my-load-user-customized-major-mode-hook t)
(defvar my-force-buffer-file-temp-p nil)
;;;###autoload
(defun my-buffer-file-temp-p ()
  "If no file or a temp file or HTML file converted from org file."
  (interactive)
  (let* ((f (buffer-file-name)) (rlt t))
    (cond
     ((not my-load-user-customized-major-mode-hook)
      (setq rlt t))

     ((and (buffer-name) (string-match "\\* Org SRc" (buffer-name)))
      ;; org-babel edit inline code block need calling hook
      (setq rlt nil))

     ((null f)
      (setq rlt t))

     ((string-match (concat "^" temporary-file-directory) f)
      ;; file is create from temp directory
      (setq rlt t))

     ((and (string-match "\.html$" f)
           (file-exists-p (replace-regexp-in-string "\.html$" ".org" f)))
      ;; file is a html file exported from org-mode
      (setq rlt t))

     (my-force-buffer-file-temp-p
      (setq rlt t))

     (t
      (setq rlt nil)))
    rlt))

(defvar my-ssh-client-user nil
  "User name of ssh client.")

;;;###autoload
(defun my-send-string-to-cli-stdin (string program)
  "Send STRING to cli PROGRAM's stdin."
  (with-temp-buffer
    (insert string)
    (call-process-region (point-min) (point-max) program)))

;;;###autoload
(defun my-write-string-to-file (string file)
  "Write STRING to FILE."
  (with-temp-buffer
    (insert string)
    (write-region (point-min) (point-max) file)))

;;;###autoload
(defun my-should-use-minimum-resource ()
  "Use minimum resource (no highlight or line number)."
  (and buffer-file-name
       (string-match "\.\\(mock\\|min\\|bundle\\)\.js" buffer-file-name)))

;;;###autoload
(defun my-async-shell-command (command)
  "Execute string COMMAND asynchronously."
  (let* ((proc (start-process "Shell"
                              nil
                              shell-file-name
                              shell-command-switch command)))
    (set-process-sentinel proc `(lambda (process signal)
                                  (let* ((status (process-status process)))
                                    (when (memq status '(exit signal))
                                      (unless (string= (substring signal 0 -1) "finished")
                                        (message "Failed to run \"%s\"." ,command))))))))

;;;###autoload
(defun my-org-babel-execute:python-hack (orig-func &rest args)
  ;; @see https://github.com/Liu233w/.spacemacs.d/issues/6
  (let ((coding-system-for-write 'utf-8))
    (apply orig-func args)))

;;;###autoload
(defun my-setup-language-and-encode (language-name coding-system)
  "Set up LANGUAGE-NAME and CODING-SYSTEM at Windows.
For example,
- \"English\" and 'utf-16-le
- \"Chinese-GBK\" and 'gbk"
  (cond
   ((eq system-type 'windows-nt)
    (set-language-environment language-name)
    (prefer-coding-system 'utf-8)
    (set-terminal-coding-system coding-system)

    (modify-coding-system-alist 'process "*" coding-system)

    (advice-add 'org-babel-execute:python :around #'my-org-babel-execute:python-hack))

   (t
    (set-language-environment "UTF-8")
    (prefer-coding-system 'utf-8))))
;; }}

;;;###autoload
(defun my-skip-white-space (start step)
  "Skip white spaces from START, return position of first non-space character.
If STEP is 1,  search in forward direction, or else in backward direction."
  (let* ((b start)
         (e (if (> step 0) (line-end-position) (line-beginning-position))))
    (save-excursion
      (goto-char b)
      (while (and (not (eq b e)) (memq (following-char) '(9 32)))
        (forward-char step))
      (point))))

;;;###autoload
(defun my-comint-current-input-region ()
  "Region of current shell input."
  (cons (process-mark (get-buffer-process (current-buffer)))
        (line-end-position)))
;;;###autoload
(defun my-comint-kill-current-input ()
  "Kill current input in shell."
  (interactive)
  (let* ((region (my-comint-current-input-region)))
    (kill-region (car region) (cdr region))))
;;;###autoload
(defun my-comint-current-input ()
  "Get current input in shell."
  (let* ((region (my-comint-current-input-region)))
    (string-trim (buffer-substring-no-properties (car region) (cdr region)))))

;;;###autoload
(defun my-rescan-imenu-items (&optional index-function)
  "Get imenu items using INDEX-FUNCTION."
  (my-ensure 'imenu)
  (let* ((imenu-auto-rescan t)
         (imenu-create-index-function (or index-function imenu-create-index-function))
         (imenu-auto-rescan-maxout (buffer-size))
         (items (imenu--make-index-alist t)))
    (delete (assoc "*Rescan*" items) items)))

;;;###autoload
(defun my-create-range (&optional inclusive)
  "Return range by font face.
Copied from 3rd party package evil-textobj."
  (let* ((point-face (my-what-face))
         (pos (point))
         (backward-none-space-point pos) ; last none white space char
         (forward-none-space-point pos) ; last none white space char
         (start pos)
         (end pos))

    ;; check chars backward,
    ;; stop when char is not white space and has different face
    (save-excursion
      (let ((continue t))
        (while (and continue (>= (- (point) 1) (point-min)))
          (backward-char)
          (unless (= 32 (char-after))
            (if (equal point-face (my-what-face))
                (setq backward-none-space-point (point))
              (setq continue nil))))))

    ;; check chars forward,
    ;; stop when char is not white space and has different face
    (save-excursion
      (let ((continue t))
        (while (and continue (< (+ (point) 1) (point-max)))
          (forward-char)
          (let ((forward-point-face (my-what-face)))
            (unless (= 32 (char-after))
              (if (equal point-face forward-point-face)
                  (setq forward-none-space-point (point))
                (setq continue nil)))))))

    (cond
     (inclusive
      (setq start backward-none-space-point)
      (setq end forward-none-space-point))
     (t
      (setq start (1+ backward-none-space-point))
      (setq end (1- forward-none-space-point))))

    (cons start (1+ end))))
;;;###autoload
(defun my-get-char (position)
  "Get character at POSITION."
  (save-excursion
    (goto-char position)
    (following-char)))
;;;###autoload
(defun my-extended-regexp (str)
  "Build regex compatible with pinyin from STR."
  (let* ((len (length str))
         bn)
    (cond
     ;; do nothing
     ((<= (length str) 1))

     ;; If the first character of input in ivy is ":" or ";",
     ;; remaining input is converted into Chinese pinyin regex.
     ((or (and (string-match "[:\|;]" (substring str 0 1))
               (setq str (substring str 1 len)))
          (and (setq bn (buffer-name))
               (or (member bn '("*Org Agenda*"))
                   (string-match ".*EMMS Playlist\\|\\.org$" bn))))
      (my-ensure 'pinyinlib)
      (setq str (pinyinlib-build-regexp-string str)))

     ;; If the first character of input in ivy is "/",
     ;; remaining input is converted to pattern to search camel case word
     ;; For example, input "/ic" match "isController" or "isCollapsed"
     ((string= (substring str 0 1) "/")
      (let* ((rlt "")
             (i 0)
             (subs (substring str 1 len))
             c)
        (when (> len 2)
          (setq subs (upcase subs))
          (while (< i (length subs))
            (setq c (elt subs i))
            (setq rlt (concat rlt (cond
                                   ((and (< c ?a) (> c ?z) (< c ?A) (> c ?Z))
                                    (format "%c" c))
                                   (t
                                    (concat (if (= i 0) (format "[%c%c]" (+ c 32) c)
                                              (format "%c" c))
                                            "[a-z]+")))))
            (setq i (1+ i))))
        (setq str rlt))))
    str))


(defvar my-disable-idle-timer (daemonp)
  "Function passed to `my-run-with-idle-timer' is run immediately.")
(defun my-run-with-idle-timer (seconds func)
  "After SECONDS, run function FUNC once."
  (cond
   (my-disable-idle-timer
    (funcall func))
   (t
    (run-with-idle-timer seconds nil func))))

;;;###autoload
(defun my-imenu-item-position (item)
  "Handle some strange imenu ITEM."
  (if (markerp item) (marker-position item) item))
;;;###autoload
(defun my-closest-imenu-item-internal (cands)
  "Return closest imenu item from CANDS."
  (let* ((pos (point))
         closest)
    (dolist (c cands)
      (let* ((item (cdr c))
             (m (cdr item)))
        (when (and m (<= (my-imenu-item-position m) pos))
          (cond
           ((not closest)
            (setq closest item))
           ((< (- pos (my-imenu-item-position m))
               (- pos (my-imenu-item-position (cdr closest))))
            (setq closest item))))))
    closest))
;;;###autoload
(defun my-mark-to-position (&optional position)
  "Mark text from point to POSITION or end of of line."
  (set-mark (or position (line-end-position)))
  (activate-mark))
;;;###autoload
(defun my-closest-imenu-item ()
  "Return the closest imenu item."
  (my-ensure 'counsel)
  (my-closest-imenu-item-internal (counsel--imenu-candidates)))
;;;###autoload
(defun my-setup-extra-keymap (extra-fn-list hint fn &rest args)
  "Map EXTRA-FN-LIST to new keymap and show HINT after calling FN with ARGS."
  (let ((echo-keystrokes nil))
    (when fn (apply fn args))
    (message hint)
    (set-transient-map
     (let ((map (make-sparse-keymap))
           cmd)
       (dolist (item extra-fn-list)
         (setq cmd (nth 1 item))
         (setq cmd (cond
                    ((commandp cmd)
                     cmd)
                    (t
                     `(lambda ()
                        (interactive)
                        (if (functionp ,cmd) (funcall ,cmd) ,cmd)))))
         (define-key map (kbd (nth 0 item)) cmd))
       map)
     t)))

;; @see http://emacs.stackexchange.com/questions/14129/which-keyboard-shortcut-to-use-for-navigating-out-of-a-string
;;;###autoload
(defun my-font-face-similar-p (f1 f2)
  "Font face F1 and F2 are similar or same."
  ;; (message "f1=%s f2=%s" f1 f2)
  ;; in emacs-lisp-mode, the '^' from "^abde" has list of faces:
  ;;   (font-lock-negation-char-face font-lock-string-face)
  (if (listp f1) (setq f1 (nth 1 f1)))
  (if (listp f2) (setq f2 (nth 1 f2)))

  (or (eq f1 f2)
      ;; C++ comment has different font face for limit and content
      ;; f1 or f2 could be a function object because of rainbow mode
      (and (string-match "-comment-" (format "%s" f1))
           (string-match "-comment-" (format "%s" f2)))))
;;;###autoload
(defun my-font-face-at-point-similar-p (font-face-list)
  "Test if font face at point is similar to any font in FONT-FACE-LIST."
  (let* ((f (get-text-property (point) 'face))
         rlt)
    (dolist (ff font-face-list)
      (if (my-font-face-similar-p f ff) (setq rlt t)))
    rlt))
;;;###autoload
(defun my-pdf-view-goto-page (page)
  "Go to pdf file's specific PAGE."
  (cond
   ((eq major-mode 'pdf-view-mode)
    (pdf-view-goto-page page))
   (t
    (doc-view-goto-page page))))
;;;###autoload
(defun my-focus-on-pdf-window-then-back (fn)
  "Focus on pdf window and call function FN then move focus back."
  (let* ((pdf-window (cl-find-if (lambda (w)
                                   (let ((file (buffer-file-name (window-buffer w))))
                                     (and file (string= (file-name-extension file) "pdf"))))
                                 (my-visible-window-list)))
         (pdf-file (buffer-file-name (window-buffer pdf-window)))
         (original-window (get-buffer-window)))
    (when (and pdf-window pdf-file)
      ;; select pdf-window
      (select-window pdf-window)
      ;; do something
      (funcall fn pdf-file)
      ;; back home
      (select-window original-window))))
;;;###autoload
(defun my-list-windows-in-frame (&optional frame)
  "List windows in FRAME."
  (window-list frame 0 (frame-first-window frame)))
;;;###autoload
(defun my-visible-window-list ()
  "Visible window list."
  (cl-mapcan #'my-list-windows-in-frame (visible-frame-list)))
;;;###autoload
(defun my-lisp-find-file-or-directory (root regexp prefer-directory-p)
  "Find files or directories in ROOT whose names match REGEXP.
If PREFER-DIRECTORY-P is t, return directory; or else, returns file.
This function is written in pure Lisp and slow."
  (my-ensure 'find-lisp)
  (find-lisp-find-files-internal
   root
   (lambda (file dir)
     (let* ((directory-p (file-directory-p (expand-file-name file dir)))
            (rlt (and (if prefer-directory-p directory-p (not directory-p))
                      (not (or (string= file ".") (string= file "..")))
                      (string-match regexp file))))
       rlt))
   'find-lisp-default-directory-predicate))


(defvar my-media-file-extensions
  '("3gp"
    "avi"
    "crdownload"
    "flac"
    "flv"
    "m4v"
    "mid"
    "mkv"
    "mov"
    "mp3"
    "mp4"
    "mpg"
    "ogm"
    "part"
    "rm"
    "rmvb"
    "wav"
    "wmv"
    "webm")
  "File extensions of media files.")

(defvar my-image-file-extensions
  '("gif"
    "jpg"
    "jpeg"
    "tif"
    "png"
    "svg"
    "xpm")
  "File extensions of image files.")

;;;###autoload
(defun my-file-extensions-to-regexp (extensions)
  "Convert file EXTENSIONS to one regex."
  (concat "\\." (regexp-opt extensions t) "$"))
;;;###autoload
(defun my-binary-file-p (file)
  "Test if it's binary FILE."
  (let* ((other-exts '("pyim" "recentf"))
         (exts (append my-media-file-extensions
                       my-image-file-extensions
                       other-exts))
         (regexp (my-file-extensions-to-regexp exts)))
    (string-match regexp file)))
;;;###autoload
(defun my-strip-path (path strip-count)
  "Strip PATH with STRIP-COUNT."
  (let* ((i (1- (length path)))
         str)
    (while (and (> strip-count 0)
                (> i 0))
      (when (= (aref path i) ?/)
        (setq strip-count (1- strip-count)))
      (setq i (1- i)))
    (setq str (if (= 0 strip-count) (substring path (1+ i)) path))
    (replace-regexp-in-string "^/" "" str)))
;;;###autoload
(defun my-goto-line (n)
  "Goto line N."
  (goto-char (point-min))
  (forward-line (1- n)))


;;===================================================
;; Byte Compile
;;===================================================
;;;###autoload
(defun byte-compile-elpa ()
  "Compile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (if (fboundp 'async-byte-recompile-directory)
      (async-byte-recompile-directory package-user-dir)
    (byte-recompile-directory package-user-dir 0 t)))
;;;###autoload
(defun byte-compile-extensions ()
  "Compile packages in extensions directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "extensions")))
    (if (fboundp 'async-byte-recompile-directory)
        (async-byte-recompile-directory dir)
      (byte-recompile-directory dir 0 t))))
;;;###autoload
(defun native-compile-elpa ()
  "Native-compile packages in elpa directory."
  (interactive)
  (if (fboundp 'native-compile-async)
      (native-compile-async package-user-dir t)))
;;;###autoload
(defun native-compile-extensions ()
  "Native compile packages in extensions directory."
  (interactive)
  (let ((dir (locate-user-emacs-file "extensions")))
    (if (fboundp 'native-compile-async)
        (native-compile-async dir t))))
;;;###autoload
(defun suk-set-variable (variable value &optional no-save)
  "Set the VARIABLE to VALUE, and return VALUE.

  Save to option `custom-file' if NO-SAVE is nil."
  (customize-set-variable variable value)
  (when (and (not no-save)
             (file-writable-p custom-file))
    (with-temp-buffer
      (insert-file-contents custom-file)
      (goto-char (point-min))
      (while (re-search-forward
              (format "^[\t ]*[;]*[\t ]*(setq %s .*)" variable)
              nil t)
        (replace-match (format "(setq %s '%s)" variable value) nil nil))
      (write-region nil nil custom-file)
      (message "Saved %s (%s) to %s" variable value custom-file))))
