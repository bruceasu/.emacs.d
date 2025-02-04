(provide 'init-vterm)
;; 配置 vterm
(use-package vterm
  :ensure t
  :commands vterm
  :config
  (setq vterm-shell "/bin/bash"))  ;; 根据需要设置默认 shell


;; 定义 toggle-vterm 函数
(defun toggle-vterm ()
  "Toggle a vterm window at the bottom of the current window.
If the vterm window is already open, it will be hidden.
Otherwise, a new vterm will be opened in a split below, with the current directory set to the buffer's directory."
  (interactive)
  (let* ((vterm-buffer-name "*vterm*")
         (existing-window (get-buffer-window vterm-buffer-name))
         (target-dir (if (and (buffer-file-name)
                              (file-directory-p (file-name-directory (buffer-file-name))))
                         (file-name-directory (buffer-file-name))
                       default-directory)))
    (if existing-window
        ;; 如果 vterm 窗口已经存在，则关闭它
        (delete-window existing-window)
      ;; 否则，创建一个新的 vterm 窗口
      (let ((default-directory target-dir))
        ;;(split-window-below)
        ;;(other-window 1)
        (vterm)
        ;;(vterm-other-window)
        ;; 切换到目标目录
        (vterm-send-string (concat "cd " (shell-quote-argument target-dir)))
        (vterm-send-return)
        ))
    ;; 保持原窗口焦点
    ;;(other-window 1)
    ))

;; 绑定快捷键
(global-set-key (kbd "C-x C-t") 'toggle-vterm)
