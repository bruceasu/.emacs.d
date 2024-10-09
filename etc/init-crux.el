;;; init-crux -- set the key for crux
;;; Commentary:

;;; Code:
(eval-when-compile
  (require '+const)
  (require '+custom)
  (require '+fn)
  (require 'lazy-load)
  (require 'init-package))

(require-package 'crux)

(lazy-load-global-keys
'(
  ;;文件操作:
  ;;("C-c r" . crux-rename-file) ; 重命名当前文件或目录。
  ("C-c k" . crux-rename-file-and-buffer)
  ;;("C-c r" . crux-recentf-find-file)
  ("C-c D"  . crux-delete-file-and-buffer) ;  删除当前文件并关闭相关缓冲区。
  ;; 行/区域操作:
  ;;crux-move-beginning-of-line: 将光标移动到行的开头。
  ;;crux-move-end-of-line: 将光标移动到行的末尾。
  ;;crux-top-join-line: 将当前行与上一行合并。

  ("C-K" . crux-kill-whole-line) ;; 剪切整行。
  ;;("C-J" .crux-kill-and-join-forward) ;;除当前行尾的空白字符，并与下一行合并。
  ;;复制/剪切/粘贴操作:
  ;;("C-l" . crux-smart-copy-line-above); 在当前行上方复制当前行。
  ;;("C-o" . crux-smart-copy-line-below);  在当前行下方复制当前行。
   ;;   缩进操作:

  ("C-c TAB" . crux-indent-defun) ;; 对当前函数或代码块重新缩进。
  ;; crux-cleanup-buffer-or-region ;; 清理缓冲区中选定区域或整个缓冲区中的尾随空格和空行。
  ;; 查找/替换操作:
  ;; crux-find-user-init-file ;; 快速打开 Emacs 用户配置文件。
  ;; crux-view-url ;; 在浏览器中查看当前 URL。
  ;; 其他实用功能:

  ("C-c ;" . crux-kill-other-buffers) ;;关闭所有除当前缓冲区外的其他缓冲区。
  ("C-k" . crux-kill-line-backwards) ;;向后删除整行内容（包括行尾换行符）。
  ;; crux-reopen-as-root-mode: 以 root 身份重新打开当前文件。

  )
"crux"
 )
(provide 'init-crux)
;;; init-crux.el ends here
