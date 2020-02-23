(provide 'load-tabbar)
(require 'tabbar)
(tabbar-mode)   
; c-c c-<up>
;(global-set-key (kbd "S-M-<up>") 'tabbar-backward-group)
; c-c c-<down>
;(global-set-key (kbd "S-M-<down>") 'tabbar-forward-group)
; c-c c-<left>
;(global-set-key (kbd "S-M-<left>") 'tabbar-backward-tab)
; c-c c-<right>
;(global-set-key (kbd "S-M-<right>") 'tabbar-forward-tab)

;;;; 设置tabbar外观
;; 设置默认主题: 字体, 背景和前景颜色，大小
(set-face-attribute 'tabbar-default nil
                    :family "Vera Sans YuanTi Mono"
                    :background "gray80"
                    :foreground "gray30"
                    :height 1.0
                    )
;; 设置左边按钮外观：外框框边大小和颜色
(set-face-attribute 'tabbar-button nil
                    :inherit 'tabbar-default
                    :box '(:line-width 1 :color "gray30")
                    )
;; 设置当前tab外观：颜色，字体，外框大小和颜色
(set-face-attribute 'tabbar-selected nil
                    :inherit 'tabbar-default
                    :foreground "DarkGreen"
                    :background "LightGoldenrod"
                    :box '(:line-width 2 :color "DarkGoldenrod")
                    ;; :overline "black"
                    ;; :underline "black"
                    :weight 'bold
                    )
;; 设置非当前tab外观：外框大小和颜色
(set-face-attribute 'tabbar-unselected nil
                    :inherit 'tabbar-default
                    :box '(:line-width 2 :color "gray70")
                    )

; from aquamacs-tabbar.el
;; change faces for better-looking tabs (and more obvious selected tab!)
;; full face specification to avoid inheriting from the frame font
;; or from mode-line
(set-face-attribute 'tabbar-default nil
		    :inherit nil
		    :height 110
		    :weight 'normal
		    :width 'normal
		    :slant 'normal
		    :underline nil
		    :strike-through nil
;; inherit from frame		    :inverse-video
		    :stipple nil
		    :background "gray80"
		    :foreground "black"
;;		    :box '(:line-width 2 :color "white" :style nil)
		    :box nil
		    :family "Lucida Grande")

(set-face-attribute 'tabbar-selected nil
		    :background "gray95"
		    :foreground "DarkGreen"
		    :inherit 'tabbar-default
		    :box '(:line-width 1 :color "grey80" :style nil))

(set-face-attribute 'tabbar-unselected nil
		    :inherit 'tabbar-default
		    :background "gray80"
            :foreground "black"
		    :box '(:line-width 1 :color "grey80" :style nil))

(defface tabbar-selected-highlight '((t
		    :foreground "black"
		    :background "gray95"))
  "Face for selected, highlighted tabs."
  :group 'tabbar)

(defface tabbar-unselected-highlight '((t
            :background "grey75"
		    :box (:line-width 1 :color "grey75" :style nil)
					))
  "Face for unselected, highlighted tabs."
  :group 'tabbar)

(set-face-attribute 'tabbar-button nil
		    :inherit 'tabbar-default
		    :box nil)

(set-face-attribute 'tabbar-separator nil
		    :background "grey80"
 		    :foreground "grey80"
		    :height 1.0)

(setq tabbar-separator '(1)) ;; set tabbar-separator size to 1 pixel

(defface tabbar-selected-modified
  '((t
     :inherit tabbar-selected
     :weight bold
     :height 110
     ))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-unselected-modified
  '((t
     :inherit tabbar-unselected
     :weight bold
     :height 110
     ))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-key-binding '((t
				 :foreground "white"))
    "Face for unselected, highlighted tabs."
    :group 'tabbar)

