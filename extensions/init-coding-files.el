;;; init-coding-files.el --- Coding file navigation helpers -*- lexical-binding: t; -*-
(require 'leader-key)
(with-eval-after-load 'leader-key
  (keymap-set leader-key-map "f j" #'dired-jump)
  (keymap-set leader-key-map "f i" #'ibuffer)
  (keymap-set leader-key-map "f t" #'suk/coding-open-file-tree))
(provide 'init-coding-files)
