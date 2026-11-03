;;; init-coding-files.el --- Coding file navigation helpers -*- lexical-binding: t; -*-
(require 'leader-key)
(with-eval-after-load 'leader-key
  (keymap-set leader-key-map "f j" #'dired-jump)
  (keymap-set leader-key-map "f i" #'ibuffer))
(provide 'init-coding-files)
