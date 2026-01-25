(provide 'init-completion-company)
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
  (:map company-mode-map
        ("M-/" .#'company-complete))
  (:map company-active-map
        ("TAB" . #'company-complete-selection)
        ("C-j" . company-complete-selection)
        ("RET" . company-complete-selection)
        ("<return>" . company-complete-selection)
        ("M-h" . company-complete-selection)
        ("M-H" . company-complete-common)
        ("M-s" . company-search-candidates)
        ("M-S" . company-filter-candidates)
        ("M-n" . company-select-next)
        ("M-p" . company-select-previous)
        ("C-g" . company-abort))
  (:map leader-key-map
        ("y" . #'company-yasnippet))
  )
(use-package company-box
  :ensure nil)

(defun my/company-complete-dwim ()
"Do-What-I-Mean 补全：优先用 company，没激活时用 dabbrev。"
(interactive)
(if (and (boundp 'company-mode) company-mode)
    (call-interactively #'company-complete)
  (call-interactively #'dabbrev-expand)))

(global-set-key (kbd "M-/") #'my/company-complete-dwim)
