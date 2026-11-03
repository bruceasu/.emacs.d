;;; buffer-extension.el --- Loader for buffer-related helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Keep the historical feature name `buffer-extension' as a thin loader.

;;; Code:

(require 'basic-toolkit)
(require 'leader-key)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (load (expand-file-name "buffer-coding.el" dir) nil t)
  (load (expand-file-name "buffer-life.el" dir) nil t)
  (load (expand-file-name "buffer-file.el" dir) nil t))

;;; Buffer key bindings stay here as glue.
(keymap-set leader-key-map "b n" #'next-buffer)
(keymap-set leader-key-map "b p" #'previous-buffer)
(keymap-set leader-key-map "b d" #'suk/close-current-buffer)
(keymap-set leader-key-map "b D" #'suk/delete-this-file)
(keymap-set leader-key-map "b k" #'suk/kill-all-buffers-except-current)
(keymap-set leader-key-map "b K" #'suk/kill-other-window-buffer)
(keymap-set leader-key-map "b c" #'suk/copy-file-name)
(keymap-set leader-key-map "b s" #'suk/new-scratch-buffer)
(keymap-set leader-key-map "b N" #'suk/new-empty-buffer)
(keymap-set leader-key-map "b r" #'suk/open-recently-closed)
(keymap-set leader-key-map "b R" #'suk/list-recently-closed)
(keymap-set leader-key-map "b l" #'suk/open-last-closed)
(keymap-set leader-key-map "b L" #'suk/toggle-eol)
(keymap-set leader-key-map "b e" #'suk/revert-auto-unicode-cjk)
(keymap-set leader-key-map "b M" #'suk/rename-or-move-file-here)
(keymap-set leader-key-map "b m" #'suk/move-file-to-dir)

(provide 'buffer-extension)

;;; buffer-extension.el ends here
