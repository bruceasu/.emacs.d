(provide 'init-rectangle)

(use-package hydra
  :ensure t)

(defhydra hydra-rectangle (:hint nil)
  "
  ^Rectangle^
  -----------------------------------------------------
  _c_ Copy Rectangle     (C-x r M-w) _d_ Delete Rectangle   (C-x r d)
  _y_ Yank Rectangle     (C-x r y)   _i_ Insert Rectangle   (C-x r i)
  _x_ Kill Rectangle     (C-x r k)   _n_ Insert Line Number (C-x r N)
  _s_ String Rectangle   (C-x r t)   _C_ Clear Rectangle    (C-x r c)
  _o_ Insert blank space (C-x r o)   _m_  Mark Rectangle     (C-x SPC)
  "
  ("c" rectangle-mark-mode)       ;; 启动矩形选择 (C-x r r)
  ("y" yank-rectangle)            ;; 粘贴矩形内容 (C-x r y)
  ("x" kill-rectangle)            ;; 删除矩形内容 (C-x r k)
  ("d" delete-rectangle)          ;; 删除矩形内容 (C-x r d)
  ("i" string-insert-rectangle)   ;; 插入矩形字符串 (C-x r i)
  ("n" rectangle-number-lines)    ;; 插入行号 (C-x r N)
  ("s" string-rectangle)          ;; 替换 (C-x r t <string> RET)
  ("C" clear-rectangle)           ;; 清除矩形内容 (C-x r c)
  ("o" open-rectangle)            ;;
  ("m" rectangle-mark-mode)       ;; 开始
  ("q" nil "Quit")                ;; 退出
  )

;; 设置快捷键以便调用矩形 Hydra
(global-set-key (kbd "C-z r") 'hydra-rectangle/body)
