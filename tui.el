;; Terminal-specific behavior only. Optional packages and visual styling
;; are configured by their own modules.
(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      frame-inhibit-implied-resize t)
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
(column-number-mode 1)
(setq select-enable-clipboard t)
(provide 'suk-ui-tui)
