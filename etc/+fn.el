;; -*- coding: utf-8; lexical-binding: t; -*-
(eval-when-compile
  (require '+const)
  (require '+custom)
  )

(provide '+fn)

(defun suk/wait-for-modules (callback &rest modules)
  "Wait for MODULES to be loaded and then call CALLBACK."
  (let ((all-loaded nil))
    (dolist (module modules)
      (with-eval-after-load module
        (setq all-loaded t)))
    (if all-loaded
        (funcall callback)
      (add-hook 'after-load-functions
                (lambda ()
                  (when (cl-every #'featurep modules)
                    (funcall callback)))))))

;; 使用示例
;;(wait-for-modules
;; 'my-callback-function
;; 'module1
;; 'module2
;; 'module3)


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

(defvar my-disable-idle-timer (daemonp)
  "Function passed to `my-run-with-idle-timer' is run immediately.")
(defun my-run-with-idle-timer (seconds func)
  "After SECONDS, run function FUNC once."
  (cond
   (my-disable-idle-timer
    (funcall func))
   (t
    (run-with-idle-timer seconds nil func))))
