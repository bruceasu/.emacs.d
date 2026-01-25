(defconst suk/is-tui (not (display-graphic-p)))
;; =========================
;; TUI
;; =========================
;; 启动干净
(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      make-backup-files nil
      auto-save-default nil
      frame-inhibit-implied-resize t)
;; 终端友好 UI
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; UTF-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
;; 行为
(column-number-mode 1)
;; 剪贴板（TUI 下也有用）
(setq select-enable-clipboard t)
(fset 'yes-or-no-p 'y-or-n-p)

;; =========================
;; Git（TUI 下也要好用）
;; =========================
(unless (package-installed-p 'magit)
  (package-install 'magit))
(global-set-key (kbd "C-x g") #'magit-status)
;; =========================
;; 搜索 / 跳转（TUI 必备）
;; =========================
(dolist (pkg '(vertico consult))
  (unless (package-installed-p pkg)
    (package-install pkg)))
(vertico-mode 1)
(global-set-key (kbd "C-c o") #'consult-outline)
(global-set-key (kbd "C-c s") #'consult-ripgrep)

;;(load "C:/green/emacs-suk/.emacs.d/share/themes/lazycat-theme/lazycat-theme.el")
;;Switch to dark theme:
;;(lazycat-theme-load-dark)
;; Switch to light theme:
;; (lazycat-theme-load-light)
;; (lazycat-theme-toggle)
;;(require 'lazycat-theme)
;;(lazycat-theme-load)
;;(run-with-idle-timer 1 nil #'lazycat-theme-load-with-sunrise)

(when suk/is-tui  
  ;; only conole packages
  ;; 行号在小屏可能太挤，可关
  (global-display-line-numbers-mode 0)
  (setq-default line-spacing 0.2)
  ;; 视觉换行（写书很重要）
  (global-visual-line-mode 1)
  ;; 光标明显一点
  (setq cursor-type 'bar)
  ;; 主题（终端最稳） 颜色（终端友好）
  (load-theme 'tango-dark t)
  ;; 滚动更顺
  (setq scroll-margin 3
        scroll-conservatively 101)

  ;; =========================
  ;; Org（写书核心）
  ;; =========================
  (require 'org)
  (setq 
   ;; 不折叠太狠
   org-startup-folded 'content
   ;; 写书时更像文本
   org-hide-emphasis-markers t
   ;; Org 美化不要太激进
   org-ellipsis " ▼")
  (defun my/org-writing-mode ()
    "TUI 下写书专用模式"
    (interactive)
    (org-content)
    (visual-line-mode 1)
    (setq-local line-spacing 0.2))
  (global-set-key (kbd "C-c w") #'my/org-writing-mode)
  (global-set-key (kbd "C-c j") #'org-goto)


  )
