;;; basic-function-core.el --- Runtime helpers and feature checks -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

;;;###autoload
(defun suk/wait-for-modules (callback &rest modules)
  "Wait for MODULES to load, then call CALLBACK."
  (let ((pending-modules modules))
    (defun check-all-loaded ()
      (when (cl-every #'featurep pending-modules)
        (funcall callback)
        (setq pending-modules nil)))

    (dolist (module modules)
      (if (featurep module)
          (setq pending-modules (delete module pending-modules))
        (with-eval-after-load module
          (check-all-loaded))))

    (when (not pending-modules)
      (funcall callback))))

;;;###autoload
(defmacro suk/timer (&rest body)
  "Measure the time it takes BODY to run."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;;;###autoload
(defun icons-displayable-p ()
  "Return non-nil if icons are displayable."
  (or (featurep 'nerd-icons)
      (require 'nerd-icons nil t)))

;;;###autoload
(defun suk-treesit-available-p ()
  "Return non-nil when tree-sitter is available."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)))

;;;###autoload
(defun too-long-file-p ()
  "Return non-nil if current buffer looks too large."
  (or (> (buffer-size) 100000)
      (and (fboundp 'buffer-line-statistics)
           (> (car (buffer-line-statistics)) 10000))))

;;;###autoload
(defun load-if-exists (f)
  "Load F if it exists and is readable."
  (when (file-readable-p f)
    (load-file f)))

(provide 'basic-function-core)

;;; basic-function-core.el ends here
