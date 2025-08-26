;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Search tool
(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :init
  (when (executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")))


;; Fast search tool `ripgrep'
(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
              ("c" . rg-dwim-current-dir)
              ("f" . rg-dwim-current-file)
              ("m" . rg-menu))
  :init (setq rg-group-result t
              rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases))

(provide 'init-tools)

;; Display available keybindings in popup

(use-package which-key
  :diminish
  :bind (("C-h M-m" . which-key-show-major-mode)
         (:map help-map ("C-h" . which-key-C-h-dispatch)))

  :hook (after-init . which-key-mode)
  ;;:custom
  ;; 弹出方式，底部弹出
  ;;(which-key-popup-type 'side-window)
  :init (setq which-key-max-description-length 30
              which-key-lighter nil
              which-key-show-remaining-keys t)
  :config
  (which-key-mode))

(use-package which-key-posframe
  :after (which-key posframe)
  :config
  (which-key-posframe-mode 1))

;;; ### Company en words ###
;;; --- 英文助手
(lazy-load-global-keys
 '(
   ("M-r" . toggle-company-english-helper) ;英文助手
   )
 "company-english-helper")

(lazy-load-global-keys
 '(
   ("<f8>" . treemacs)
  )
"init-treemacs")

;; 设置打开 NeoTree 树形列表展示
;;(require-package 'neotree)
;;(use-package neotree
;;  :commands (projectile-switch-project neotree-dir)
;;  :config
;;  (setq neo-theme 'ascii           ; NeoTree 图标的样式
;;        neo-window-width 35
;;        neo-window-fixed-size nil) ; 设置 NeoTree 窗口的宽度可以使用鼠标调整
;;  :bind ("C-c o" . projectile-switch-project))

(use-package markdown-mode)

;;;; The emacsclient call depends on the daemon or `server-mode' (I use the latter)
(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))
