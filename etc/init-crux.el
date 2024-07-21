;;; init-crux -- set the key for crux
;;; Commentary:

;;; Code:
(eval-when-compile
  (require '+const)
  (require '+custom)
  (require '+fn)
  (require 'init-package))

(require-package 'crux)
(use-package crux
  :ensure t
  :bind
  (("C-c k" . crux-rename-file-and-buffer)
   ;;("C-c r" . crux-recentf-find-file))
  :config
  ;; 可以在这里添加额外的配置或自定义设置
  )

(provide 'init-crux)
;;; init-crux.el ends here
