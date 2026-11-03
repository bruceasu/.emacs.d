;; Optimization
(setq idle-update-delay 1.0)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; GUI Environment
(when (display-graphic-p)
  ;; Don't use GTK+ tooltip
  (when (boundp 'x-gtk-use-system-tooltips)
    (setq x-gtk-use-system-tooltips nil))
  ;; scroll-bar
  (set-scroll-bar-mode 'right)
  ;; 隐藏垂直滚动条。
  ;;(modify-all-frames-parameters '((vertical-scroll-bars)))
  )
(provide 'suk-ui-gui)
