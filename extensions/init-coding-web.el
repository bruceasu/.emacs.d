;;; init-coding-web.el --- Web and completion support for coding -*- lexical-binding: t; -*-

;;; Code:

(require 'basic-function)

;; Formatting
(when (locate-library "format-all")
  (use-package format-all
    :ensure t
    :hook (prog-mode . format-all-ensure-formatter)
    :bind ("C-c f" . format-all-buffer)))

;; Web / JavaScript
(when (locate-library "js2-mode")
  (use-package js2-mode
    :mode (("\\.js\\'" . js2-mode)
           ("\\.mjs\\'" . js2-mode)
           ("\\.cjs\\'" . js2-mode))))

(when (locate-library "web-mode")
  (use-package web-mode
    :mode (("\\.\\(php\\|[gj]sp\\|html?\\|vue\\|tsx?\\|jsx?\\)\\'" . web-mode))
    :config
    (setq web-mode-enable-css-colorization t
          web-mode-markup-indent-offset 2
          web-mode-code-indent-offset 2
          web-mode-css-indent-offset 2)))

(when (locate-library "add-node-modules-path")
  (use-package add-node-modules-path
    :hook ((web-mode js-mode js2-mode js-ts-mode) . add-node-modules-path)))

;; Completion
(when (locate-library "corfu")
  (use-package corfu
    :custom
    (corfu-cycle t)
    (corfu-auto t)
    (corfu-auto-prefix 2)
    (corfu-auto-delay 1)
    (corfu-popupinfo-delay '(0.5 . 0.2))
    (corfu-preview-current 'insert)
    (corfu-preselect 'prompt)
    (corfu-on-exact-match nil)
    :bind (:map corfu-map
                ("TAB" . corfu-next)
                ([tab] . corfu-next)
                ("C-n" . corfu-next)
                ("S-TAB" . corfu-previous)
                ([backtab] . corfu-previous)
                ("C-p" . corfu-previous)
                ("S-<return>" . corfu-insert)
                ("RET" . nil)
                ("<return>" . corfu-mode)
                ("C-y" . corfu-yank)
                ("C-g" . corfu-abort))
    :init
    (global-corfu-mode)
    (corfu-history-mode)))

(when (and (display-graphic-p) (locate-library "nerd-icons-corfu"))
  (use-package nerd-icons-corfu
    :after corfu
    :init
    (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

(when (locate-library "orderless")
  (use-package orderless
    :custom
    (completion-styles '(orderless basic))
    (completion-category-overrides '((file (styles basic partial-completion))))
    (orderless-component-separator #'orderless-escapable-split-on-space)))

(when (and (locate-library "pinyinlib") (locate-library "orderless"))
  (use-package pinyinlib
    :after orderless
    :autoload pinyinlib-build-regexp-string
    :init
    (defun completion--regex-pinyin (str)
      (orderless-regexp (pinyinlib-build-regexp-string str)))
    (add-to-list 'orderless-matching-styles 'completion--regex-pinyin)))

(when (and (display-graphic-p) (locate-library "nerd-icons-completion"))
  (use-package nerd-icons-completion
    :when (icons-displayable-p)
    :hook (vertico-mode . nerd-icons-completion-mode)))

(provide 'init-coding-web)

;;; init-coding-web.el ends here
