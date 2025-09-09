;;; leader-key.el --- Minimal Leader Key setup using M-m -*- lexical-binding: t; -*-

;;; Commentary:
;; A simple Leader key setup without general.el.
;; Use `M-m` as the prefix key (like Spacemacs).
;; Grouped into: files, buffers, windows, search, project.

;;; Code:

;; Step 1: 保留原本 M-m 的功能（back-to-indentation）
(global-set-key (kbd "M-i") #'back-to-indentation)

;; Step 2: 定义 Leader key 前缀
;;(define-prefix-command 'leader-key)
;;(global-set-key (kbd "M-m") 'leader-key)

(defvar leader-key-map (make-sparse-keymap)
  "Keymap for leader key bindings.")

(global-set-key (kbd "M-m") leader-key-map)

;; ------------------------------------------------------------
;; 文件相关 (f)
;; ------------------------------------------------------------
(keymap-set leader-key-map "f f" #'find-file)
(keymap-set leader-key-map "f s" #'save-buffer)
(keymap-set leader-key-map "f r" #'recentf-open-files) ;; 打开最近文件
(keymap-set leader-key-map "f d" #'dired)              ;; 打开 dired


;; ------------------------------------------------------------
;; 窗口相关 (w)
;; ------------------------------------------------------------
(keymap-set leader-key-map "w v" #'split-window-right)
(keymap-set leader-key-map "w s" #'split-window-below)
(keymap-set leader-key-map "w d" #'delete-window)
(keymap-set leader-key-map "w o" #'delete-other-windows)
(keymap-set leader-key-map "w m" #'toggle-frame-maximized) ;; 最大化窗口
(keymap-set leader-key-map "w f" #'toggle-frame-fullscreen) ;; full screen

;; ------------------------------------------------------------
;; 搜索相关 (s)
;; ------------------------------------------------------------
(keymap-set leader-key-map "s s" #'isearch-forward)
(keymap-set leader-key-map "s r" #'query-replace)
(keymap-set leader-key-map "s g" #'rgrep) ;; 如果你装了 rg.el，可以绑定 rg-dwim

;; ------------------------------------------------------------
;; 项目相关 (p)
;; ------------------------------------------------------------
(when (require 'project nil t)
  (keymap-set leader-key-map "p f" #'project-find-file)
  (keymap-set leader-key-map "p p" #'project-switch-project)
  (keymap-set leader-key-map "p s" #'project-search))

;; ------------------------------------------------------------
;; 杂项 (SPC)
;; ------------------------------------------------------------
(keymap-set leader-key-map "SPC" #'execute-extended-command) ;; M-m SPC 等于 M-x

(provide 'leader-key)

;;;leader-key.el ends here
