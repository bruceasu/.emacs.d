 

;; 窗口管理器
(use-package windmove 
  :ensure t 
  :init (windmove-default-keybindings))

;; 跳转窗口
(use-package ace-window
   :ensure t
   :init
   (progn
     (global-set-key [remap other-window] 'ace-window)
 	;; 设置标记
     (custom-set-faces
      '(aw-leading-char-face
        ((t (:inherit ace-jump-face-foreground :height 3.0 :foreground "magenta")))))))

(provide 'init-window)
