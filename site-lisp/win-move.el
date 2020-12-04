;;; win-move -- move the window

;;; code:

;; 下面这两个键模拟Vi的光标不动屏幕动效果, 我很喜欢, 几乎总在使用.
;;(global-set-key [(meta N)] 'window-move-up)        
;;(global-set-key [(meta P)] 'window-move-DOWN)

;; 同上, 但是是另一个buffer窗口上下移动. 常常查看帮助用这个.
;;(global-set-key [(control N)] 'other-window-move-up)
;;(global-set-key [(control P)] 'other-window-move-down) 

;; ------------------------------下面是上面用到的函数定义------------------------------
(defun window-move-up (&optional arg)
  "Current window move-up 1 lines."
  (interactive "P")
  (if arg
	  (scroll-up arg)
	(scroll-up 1)))

(defun window-move-down (&optional arg)
  "Current window move-down 1 lines."
  (interactive "P")
  (if arg
	  (scroll-down arg)
	(scroll-down 1)))

(defun other-window-move-up (&optional arg)
  "Other window move-up 1 lines."
  (interactive "p")
  (scroll-other-window arg))

(defun other-window-move-down (&optional arg)
  "Other window move-down 1 lines."
  (interactive "P")
  (if arg
	  (scroll-other-window-down arg)
	(scroll-other-window-down 1)))
	
(provide 'win-move)
