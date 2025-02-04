(use-package company
  :defer 2
  :init
  (setq company-idle-delay nil)  ;; 禁用自动弹出补全
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :hook (after-init . global-company-mode)
  :init (setq company-tooltip-align-annotations t
              company-minimum-prefix-length 3
              company-require-match nil
              company-dabbrev-ignore-case nil
              company-dabbrev-downcase nil
              company-show-numbers t)
  :config
  ;;(setq switch-window-input-style 'minibuffer)
  (setq company-minimum-prefix-length 3)
  (setq company-show-quick-access t)
  :bind
  (:map company-active-map
        ("C-n" . #'company-select-next)
        ("C-p" . #'company-select-previous)
        ("TAB" . company-complete-selection)
        ("M-h" . company-complete-selection)
        ("M-H" . company-complete-common)
        ("M-s" . company-search-candidates)
        ("M-S" . company-filter-candidates)
        ("M-n" . company-select-next)
        ("M-p" . company-select-previous))
  (:map leader-key
        ("y" . #'company-yasnippet))
  )
(use-package company-box
  :ensure nil)
(provide 'init-completion-company)
