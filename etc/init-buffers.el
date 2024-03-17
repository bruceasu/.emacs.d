;;; init-buffers.el --- Initialize buffers configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;
;; buffers configuration.
;;
;;; Code:
(eval-when-compile
  (require '+const)
  (require '+custom)
  (require 'buffer-extension))

(suk-set-key-bindings
 'global-set-key
 (list
  ;;(list (kbd "C-x b l") #'suk/count-brf-lines)

  (list (kbd "C-x x x") #'suk/switch-major-mode)
  (list (kbd "C-x x X") #'suk/get-mode-name)
  (list (kbd "C-x x n") #'suk/new-empty-buffer)
  (list (kbd "C-x x s") #'suk/create-scratch-buffer)
  (list (kbd "C-x x o") #'suk/create-scratch-org)
  (list (kbd "C-x x m") #'suk/switch-to-minibuffer)
  (list (kbd "C-x x c") #'copy-buffer-file-name-as-kill)
  (list (kbd "C-x x t") #'suk/toggle-margin-right)
  (list (kbd "C-x k")   #'suk/close-current-buffer)
  (list (kbd "C-x C-k")   #'suk/kill-all-buffers-except-current)
  (list (kbd "C-x K")  #'suk/kill-other-window-buffer) ;关闭其他窗口的
  (list (kbd "C-x x u") #'suk/revert-buffer-with-utf8)
  (list (kbd "C-x x g") #'suk/revert-buffer-with-gbk)
  ;;'([C-t]               transpose-chars)
  ;;'([S-f6]              hs-minor-mode)
  ;;'([S-f5]              toggle-truncate-lines)
  (list (kbd "C-S-t") #'suk/open-last-closed)
  (list (kbd "C-x R") #'recentf-open)
  (list (kbd "C-S-<f6>") #'suk/move-buffer-file)
  (list (kbd "C-S-<f2>")  #'suk/rename-file-and-buffer)
  ))



(provide 'init-buffers)
;;; init-buffers.el ends here
