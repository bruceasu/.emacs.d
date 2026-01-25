(provide 'init-devtools)

;; format all, formatter for almost languages
;; great for programmers
(use-package format-all)
(use-package format-all
  :ensure t
  :hook (prog-mode . format-all-ensure-formatter)
  :bind ("C-c f" . #'format-all-buffer))

;; JSON
(unless (fboundp 'js-json-mode)
  (use-package json-mode))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-minor-mode)
         ("\\.jsx\\'" . js2-minor-jsx-mode))
  :interpreter (("node" . js2-minor-mode)
                ("node" . js2-minor-jsx-mode))
  :hook ((js2-minor-mode . (lambda()  (js2-imenu-extras-mode)
                             (js2-highlight-unused-variables-mode)
                             )))
  )

(use-package nvm)
;; Adds node_modules/.bin directory to `exec_path'
(use-package add-node-modules-path
  :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

(use-package web-mode)
(use-package web-mode
  :mode "\\.\\(php\\|[gj]sp\\|html?\\|vue\\|tsx?\\|jsx?\\)$"
  :config
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-imenu-regexp-list
	'(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
	  ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
	  ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
	  ;; angular imenu
	  (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "="))))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-auto-delay 1)
  (corfu-popupinfo-delay '(0.5 . 0.2))
  (corfu-preview-current 'insert)
  (corfu-preselect 'prompt)
  (corfu-on-exeact-match nil)
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
  (corfu-history-mode))

(when (display-graphic-p)
  (use-package nerd-icons-corfu
    :after corfu
    :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)))

;; Optionally use the `orderless' completion style.)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))
;; Support Pinyin
(use-package pinyinlib
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(when (display-graphic-p)
  (use-package nerd-icons-completion
    :when (icons-displayable-p)
    :hook (vertico-mode . nerd-icons-completion-mode)))
