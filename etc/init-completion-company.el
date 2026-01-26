;;; init-completion-company.el --- Completion support -*- lexical-binding: t; -*-

;;; Code:

(provide 'init-completion-company)

(defun my/company-complete-dwim ()
  "Complete with company when available, otherwise fall back to dabbrev."
  (interactive)
  (if (and (boundp 'company-mode) company-mode)
      (call-interactively #'company-complete)
    (call-interactively #'dabbrev-expand)))

(when (locate-library "company")
  (use-package company
    :defer 2
    :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
    :init
    (setq company-idle-delay nil
          company-tooltip-align-annotations t
          company-minimum-prefix-length 3
          company-require-match nil
          company-dabbrev-ignore-case nil
          company-dabbrev-downcase nil
          company-show-numbers t)
    :config
    (unless (bound-and-true-p global-corfu-mode)
      (global-company-mode 1))
    (setq company-minimum-prefix-length 3)
    (setq company-show-quick-access t)
    :bind
    (:map company-mode-map
          ("M-/" . company-complete))
    (:map company-active-map
          ("TAB" . company-complete-selection)
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
          ("y" . company-yasnippet))))

(when (locate-library "company-box")
  (use-package company-box
    :ensure nil))

(global-set-key (kbd "M-/") #'my/company-complete-dwim)

;;; init-completion-company.el ends here
