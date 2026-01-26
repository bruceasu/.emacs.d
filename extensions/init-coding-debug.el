;;; init-coding-debug.el --- Coding debug helpers -*- lexical-binding: t; -*-
(require 'leader-key)

(defun suk/coding-compile-dwim ()
  "Build from the current project root."
  (interactive)
  (let ((default-directory (suk/coding-project-root)))
    (compile (suk/coding-build-command))))
(defun suk/coding-test-project ()
  "Run tests from the current project root."
  (interactive)
  (let ((default-directory (suk/coding-project-root)))
    (compile (suk/coding-test-command))))
(defun suk/coding-recompile () (interactive) (recompile))
(defun suk/coding-toggle-debug-error () (interactive) (toggle-debug-on-error))
(defun suk/coding-toggle-debug-quit () (interactive) (toggle-debug-on-quit))
(defun suk/coding-python-repl () (interactive) (run-python nil nil t))
(defun suk/coding-python-send-buffer ()
  (interactive)
  (if (derived-mode-p 'python-mode)
      (python-shell-send-buffer) (user-error "Not in a Python buffer")))
(defun suk/coding-gdb () (interactive) (call-interactively #'gdb))

(with-eval-after-load 'leader-key
  (keymap-set leader-key-map "d c" #'suk/coding-compile-dwim)
  (keymap-set leader-key-map "d t" #'suk/coding-test-project)
  (keymap-set leader-key-map "d r" #'suk/coding-recompile)
  (keymap-set leader-key-map "d e" #'suk/coding-toggle-debug-error)
  (keymap-set leader-key-map "d q" #'suk/coding-toggle-debug-quit)
  (keymap-set leader-key-map "d p" #'suk/coding-python-send-buffer)
  (keymap-set leader-key-map "d g" #'suk/coding-gdb)
  (keymap-set leader-key-map "d R" #'suk/coding-python-repl))
(provide 'init-coding-debug)
