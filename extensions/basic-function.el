;; -*- coding: utf-8; lexical-binding: t; -*-
(provide 'basic-function)

;;;###autoload
(defun suk/wait-for-modules (callback &rest modules)
  "Wait for MODULES to be loaded and then call CALLBACK.
   使用示例
  (suk/wait-for-modules
   'my-callback-function
   'module1
   'module2
   'module3)"
  (let ((pending-modules modules))  ;; 记录未加载的模块
    ;; 定义一个检查所有模块是否已加载的函数
    (defun check-all-loaded ()
      (when (cl-every #'featurep pending-modules)  ;; 检查是否所有模块都加载完毕
        (funcall callback)  ;; 调用回调
        (setq pending-modules nil)))  ;; 清除待处理的模块

    ;; 对每个模块进行处理
    (dolist (module modules)
      (if (featurep module)
          (setq pending-modules (delete module pending-modules))  ;; 如果模块已经加载，就从 pending-modules 删除
        (with-eval-after-load module
          (check-all-loaded))))  ;; 在模块加载后检查是否所有模块都加载完毕

    ;; 如果在处理时所有模块都已加载，则直接调用回调
    (when (not pending-modules)
      (funcall callback))))



;;;###autoload
(defun run-cmd-and-replace-region (cmd)
  "Run CMD in shell on selected region or current buffer.
      Then replace the region or buffer with cli output."
  (let* ((orig-point (point))
         (b (if (region-active-p) (region-beginning) (point-min)))
         (e (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region b e cmd nil t)
    (goto-char orig-point)))



;;;###autoload
(defun suk/buffer-str ()
  (buffer-substring-no-properties (point-min) (point-max)))

;;;###autoload
(defmacro suk/timer (&rest body)
  "Measure the time of code BODY running."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

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

;; {{ copied from http://ergoemacs.org/emacs/elisp_read_file_content.html
;;;###autoload
(defun suk/get-string-from-file (file)
  "Return FILE's content."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

;;;###autoload
(defun suk/read-lines (file)
  "Return a list of lines of FILE."
  (split-string (my-get-string-from-file file) "\n" t))
;; }}


;;;###autoload
(defun path-in-directory-p (file directory)
  "FILE is in DIRECTORY."
  (let* ((pattern (concat "^" (file-name-as-directory directory))))
    (if (string-match pattern file) file)))



;;;###autoload
(defun suk/send-string-to-cli-stdin (string program)
  "Send STRING to cli PROGRAM's stdin."
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
  "Execute string COMMAND asynchronously."
  (let* ((proc (start-process "Shell" nil shell-file-name shell-command-switch command)))
    (set-process-sentinel proc
                          `(lambda (process signal)
                             (let* ((status (process-status process)))
                               (when (memq status '(exit signal))
                                 (unless (string= (substring signal 0 -1) "finished")
                                   (message "Failed to run \"%s\"." ,command))))))))

;;;###autoload
(defvar my-disable-idle-timer (daemonp)
  "Function passed to `my-run-with-idle-timer' is run immediately.")

;;;###autoload
(defun my-run-with-idle-timer (seconds func)
  "After SECONDS, run function FUNC once."
  (cond
   (my-disable-idle-timer
    (funcall func))
   (t
    (run-with-idle-timer seconds nil func))))

;;;###autoload
(defun my-ensure (feature)
  "Make sure FEATURE is required."
  (unless (featurep feature)
    (condition-case nil
        (require feature)
      (error nil))))

;;;###autoload
(defun load-if-exists (f)
  "load the elisp file only if it exists and is readable"
  (if (file-readable-p f)
      (load-file f)))

