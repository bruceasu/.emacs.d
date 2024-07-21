
;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-ide.el --- IDE configuration


(provide 'init-ide)

(eval-when-compile
  (require 'init-package))

;; 设置行号
;; builtin
(require 'display-line-numbers)
;;(global-display-line-numbers-mode 1)
;; Alternatively, to use it only in programming modes:
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Line numbers are not displayed when large files are used.
(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 1000)

; Parentheses
(use-package paren
  :ensure nil
  :config (setq-default show-paren-style 'mixed
                        show-paren-when-point-inside-paren t
                        show-paren-when-point-in-periphery t)
  :hook (prog-mode . show-paren-mode))


;; 语法检查包
(require-package 'flycheck)
(use-package flycheck
  :ensure t
  :defer 3)

;; Flymake
;; 配置 Python 使用 flymake-pyflakes 后端
(require 'flymake)
(when (executable-find "pyflakes")
  (flymake-python-pyflakes-load))

;; 配置 JavaScript 使用 flymake-eslint 后端
(when (executable-find "eslint")
  (setq flymake-eslint-executable "eslint")
  (add-hook 'js-mode-hook 'flymake-eslint-load))

;; 配置 Shell 使用 flymake-shellcheck 后端
(when (executable-find "shellcheck")
  (setq flymake-shellcheck-excluded-linters '("SC2162" "SC2164"))
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;; 配置 C/C++ 使用 flymake-proc 后端（默认后端）
(add-hook 'c-mode-hook 'flymake-mode)
(add-hook 'c++-mode-hook 'flymake-mode)
(add-hook 'prog-mode-hook 'flymake-mode)


;; format all, formatter for almost languages
;; great for programmers
(require-package 'format-all)
(use-package format-all
  :ensure t
  :hook (prog-mode . format-all-ensure-formatter)
  :bind ("C-c f" . #'format-all-buffer))

;; 折叠和收缩代码
;; builtin
(use-package hideshow
  :diminish hs-minor-mode
  :pretty-hydra
  ((:title (pretty-hydra-title "HideShow" 'octicon "nf-oct-fold")
           :color amaranth :quit-key ("q" "C-g"))
   ("Fold"
    (("t" hs-toggle-all "toggle all")
     ("a" hs-show-all "show all")
     ("i" hs-hide-all "hide all")
     ("g" hs-toggle-hiding "toggle hiding")
     ("c" hs-cycle "cycle block")
     ("s" hs-show-block "show block")
     ("h" hs-hide-block "hide block")
     ("l" hs-hide-level "hide level"))
    "Move"
    (("C-a" mwim-beginning-of-code-or-line "⭰")
     ("C-e" mwim-end-of-code-or-line "⭲")
     ("C-b" backward-char "←")
     ("C-n" next-line "↓")
     ("C-p" previous-line "↑")
     ("C-f" forward-char "→")
     ("C-v" pager-page-down "↘")
     ("M-v" pager-page-up "↖")
     ("M-<" beginning-of-buffer "⭶")
     ("M->" end-of-buffer "⭸"))))
  :bind
  (:map hs-minor-mode-map
              ("C-~" . hideshow-hydra/body)
              ("C-S-<escape>" . hideshow-hydra/body)
              ("C-c ." . hs-toggle-hiding)
              ("C-c ," . hs-show-all)
              )
  :hook (prog-mode . hs-minor-mode)
  :config
  ;;代码折叠
	(add-hook 'c-mode-common-hook   'hs-minor-mode)
	(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
	(add-hook 'java-mode-hook       'hs-minor-mode)
	(add-hook 'ess-mode-hook        'hs-minor-mode)
	(add-hook 'perl-mode-hook       'hs-minor-mode)
	(add-hook 'sh-mode-hook         'hs-minor-mode)
	(add-hook 'python-mode-hook     'hs-minor-mode)
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (if (char-displayable-p ?⏷) "⏷" "...")
                     'face 'shadow)
                    (propertize
                     (format " (%d lines)"
                             (count-lines (overlay-start ov)
                                          (overlay-end ov)))
                     'face '(:inherit shadow :height 0.8))
                    " "))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts))



;; 代码片段

(require-package 'yasnippet)
(require-package 'auto-yasnippet)
(require 'yasnippet)
(with-eval-after-load 'yasnippet
  (setq yas-snippet-dirs '("~/.emacs.d/share/snippets"))
  (yas-global-mode 1)
  (autoload 'yas-minor-mode-on "yasnippet")
  (dolist (x '(org-mode-hook prog-mode-hook snippet-mode-hook))
	  (add-hook x #'yas-minor-mode-on))

)


(require 'init-treemacs)
(require 'init-lang-web)

(require-package 'projectile)
(use-package projectile
  :ensure t
  :when (< emacs-major-version 28)
  :diminish " Proj."
  :init (add-hook 'after-init-hook 'projectile-mode)
  :config
  ;;(setq projectile-completion-system 'ido)
  ;;(setq ido-enable-flex-matching t)
  (setq projectile-completion-system 'ivy)
  ;; Eanble Projectile globally
  ;;(projectile-mode 1)
  ;; Set akeybinding for projectile commands
  ;;(global-set-key (kbd "C-c p") 'projectile-commander)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

;; 设置打开 NeoTree 树形列表展示
(require-package 'neotree)
(use-package neotree
  :commands (projectile-switch-project neotree-dir)
  :config
  (setq neo-theme 'ascii           ; NeoTree 图标的样式
        neo-window-width 35
        neo-window-fixed-size nil) ; 设置 NeoTree 窗口的宽度可以使用鼠标调整
  :bind ("C-c o" . projectile-switch-project))

;;Show function arglist or variable docstring
(use-package eldoc
    :ensure nil
    :diminish
    :config
    (when (childframe-workable-p)
      (use-package eldoc-box
        :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
        :custom
        (eldoc-box-lighter nil)
        (eldoc-box-only-multi-line t)
        (eldoc-box-clear-with-C-g t)
        :custom-face
        (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
        (eldoc-box-body ((t (:inherit tooltip))))
        :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
        :config
        ;; Prettify `eldoc-box' frame
        (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
              (alist-get 'right-fringe eldoc-box-frame-parameters) 8))))

;; Cross-referencing commands
(use-package xref
    :bind (("M-g ." . xref-find-definitions)
           ("M-g ," . xref-go-back))
    :init
    ;; Use faster search tool
    (when (executable-find "rg")
      (setq xref-search-program 'ripgrep))

    ;; Select from xref candidates in minibuffer
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read
          xref-show-xrefs-function #'xref-show-definitions-completing-read))
;; Jump to definition
(use-package dumb-jump
    :pretty-hydra
    ((:title (pretty-hydra-title "Dump Jump" 'faicon "nf-fa-anchor")
      :color blue :quit-key ("q" "C-g"))
     ("Jump"
      (("j" dumb-jump-go "Go")
       ("o" dumb-jump-go-other-window "Go other window")
       ("e" dumb-jump-go-prefer-external "Go external")
       ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
      "Other"
      (("i" dumb-jump-go-prompt "Prompt")
       ("l" dumb-jump-quick-look "Quick look")
       ("b" dumb-jump-back "Back"))))
    :bind (("C-M-j" . dumb-jump-hydra/body))
    :init
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
    (setq dumb-jump-selector 'completing-read))

;; Tree-sitter support
(when (suk-treesit-available-p)
    (use-package treesit-auto
      :hook (after-init . global-treesit-auto-mode)
      :init (setq treesit-auto-install 'prompt))
	(global-tree-sitter-mode)
    )



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for LSP MODE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package eglot
;;   :hook ((c-mode c++-mode go-mode java-mode js-mode python-mode rust-mode web-mode) . eglot-ensure)
;;   :bind (("C-c e f" . #'eglot-format)
;;          ("C-c e i" . #'eglot-code-action-organize-imports)
;;          ("C-c e q" . #'eglot-code-action-quickfix))
;;   :config
;;   ;; (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
;;   (defun eglot-actions-before-save()
;;     (add-hook 'before-save-hook (lambda ()
;;                                   (call-interactively #'eglot-format)
;;                                   (call-interactively #'eglot-code-action-organize-imports))))
;;   (add-to-list 'eglot-server-programs '(web-mode "vls"))
;;   (add-hook 'eglot--managed-mode-hook #'eglot-actions-before-save))


(setenv "PATH" (concat (getenv "PATH") ";c:/green/python311/"))
(add-to-list 'exec-path "c:/green/python311")
;; https://github.com/manateelazycat/lsp-bridge
(require 'lsp-bridge)
(global-lsp-bridge-mode)
(add-hook 'prog-mode-hook 'lsp-bridge-mode)
;; set the python interpreter path if it's different from default
(setq lsp-bridge-python-command "C:\\green\Python311\\python.exe")

(require 'lsp-bridge-jdtls) ;; 根据项目自动生成自定义配置，添加必要的启动参数
(setq lsp-bridge-enable-auto-import t) ;; 开启自动导入依赖，目前没有code action。补全时可以通过这个导入相应的依赖，建议开启。
(setq lsp-bridge-jdtls-jvm-args '("-javaagent:c:/User/suk/.m2/repository/org/projectlombok/lombok/1.18.32/lombok-1.18.32.jar"))
(custom-set-variables '(lsp-bridge-get-workspace-folder 'my-lsp-bridge-workspace))
(add-hook 'java-mode-hook (lambda ()
                            (setq-local lsp-bridge-get-lang-server-by-project 'lsp-bridge-get-jdtls-server-by-project)))


(defun my-lsp-bridge-workspace (proj)
  (let* ((proj-2-workspace
          '(("D:/03_projects/suk/word-process-src" . "file://D:/03_projects/suk")
            ("D:/03_projects/suk/anysql" . "file://D:/03_projects/suk")


          ))
         (kv (assoc proj proj-2-workspace)))
    (when kv
        (cdr kv))))



;;(setq copilot-node-executable "C:\\green\\node-v20.10.0-win-x64\\node.exe")
;;(add-to-list 'load-path "C:\\green\\emacs-suk\\.emacs.d\\extensions\\copilot\\copilot.el")
;;(require 'copilot)
;;(add-hook 'prog-mode-hook 'copilot-mode)
;; To customize the behavior of copilot-mode, please check copilot-enable-predicates and copilot-disable-predicates.
;; You need to bind copilot-complete to some key and call copilot-clear-overlay inside post-command-hook.
;;(define-key copilot-completion-map
;;            (kbd "<tab>")
;;            'copilot-accept-completion)
;;(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
;; (add-to-list 'copilot-major-mode-alist '("jsonl" . "json"))
;; Login to Copilot by M-x copilot-login. You can also check the status by M-x copilot-diagnose (NotAuthorized means you don't have a valid subscription).

;; Misc. programming modes
(use-package csv-mode)
(use-package yaml-mode)

(require 'fingertip) ;; M-x treesit-install-language-grammar RET
(dolist (hook (list
                'c-mode-common-hook
                'c-mode-hook
                'c++-mode-hook
                'java-mode-hook
                'haskell-mode-hook
                'emacs-lisp-mode-hook
                'lisp-interaction-mode-hook
                'lisp-mode-hook
                'maxima-mode-hook
                'ielm-mode-hook
                'sh-mode-hook
                'makefile-gmake-mode-hook
                'php-mode-hook
                'python-mode-hook
                'js-mode-hook
                'go-mode-hook
                'qml-mode-hook
                'jade-mode-hook
                'css-mode-hook
                'ruby-mode-hook
                'coffee-mode-hook
                'rust-mode-hook
                'rust-ts-mode-hook
                'qmake-mode-hook
                'lua-mode-hook
                'swift-mode-hook
                'web-mode-hook
                'markdown-mode-hook
                'llvm-mode-hook
                'conf-toml-mode-hook
                'nim-mode-hook
                'typescript-mode-hook
                'c-ts-mode-hook
                'c++-ts-mode-hook
                'cmake-ts-mode-hook
                'toml-ts-mode-hook
                'css-ts-mode-hook
                'js-ts-mode-hook
                'json-ts-mode-hook
                'python-ts-mode-hook
                'bash-ts-mode-hook
                'typescript-ts-mode-hook
                ))
   (add-hook hook #'(lambda () (fingertip-mode 1))))

(define-key fingertip-mode-map (kbd "(") 'fingertip-open-round)
(define-key fingertip-mode-map (kbd "[") 'fingertip-open-bracket)
(define-key fingertip-mode-map (kbd "{") 'fingertip-open-curly)
(define-key fingertip-mode-map (kbd ")") 'fingertip-close-round)
(define-key fingertip-mode-map (kbd "]") 'fingertip-close-bracket)
(define-key fingertip-mode-map (kbd "}") 'fingertip-close-curly)
(define-key fingertip-mode-map (kbd "=") 'fingertip-equal)
(define-key fingertip-mode-map (kbd "（") 'fingertip-open-chinese-round)
(define-key fingertip-mode-map (kbd "「") 'fingertip-open-chinese-bracket)
(define-key fingertip-mode-map (kbd "【") 'fingertip-open-chinese-curly)
(define-key fingertip-mode-map (kbd "）") 'fingertip-close-chinese-round)
(define-key fingertip-mode-map (kbd "」") 'fingertip-close-chinese-bracket)
(define-key fingertip-mode-map (kbd "】") 'fingertip-close-chinese-curly)

(define-key fingertip-mode-map (kbd "%") 'fingertip-match-paren)
(define-key fingertip-mode-map (kbd "\"") 'fingertip-double-quote)
(define-key fingertip-mode-map (kbd "'") 'fingertip-single-quote)

;(define-key fingertip-mode-map (kbd "SPC") 'fingertip-space)
;(define-key fingertip-mode-map (kbd "RET") 'fingertip-newline)

(define-key fingertip-mode-map (kbd "M-o") 'fingertip-backward-delete)
(define-key fingertip-mode-map (kbd "C-d") 'fingertip-forward-delete)
(define-key fingertip-mode-map (kbd "C-k") 'fingertip-kill)

(define-key fingertip-mode-map (kbd "M-\"") 'fingertip-wrap-double-quote)
(define-key fingertip-mode-map (kbd "M-'") 'fingertip-wrap-single-quote)
(define-key fingertip-mode-map (kbd "M-[") 'fingertip-wrap-bracket)
(define-key fingertip-mode-map (kbd "M-{") 'fingertip-wrap-curly)
(define-key fingertip-mode-map (kbd "M-(") 'fingertip-wrap-round)
(define-key fingertip-mode-map (kbd "M-)") 'fingertip-unwrap)

(define-key fingertip-mode-map (kbd "M-p") 'fingertip-jump-right)
(define-key fingertip-mode-map (kbd "M-n") 'fingertip-jump-left)
(define-key fingertip-mode-map (kbd "M-:") 'fingertip-jump-out-pair-and-newline)

(define-key fingertip-mode-map (kbd "C-j") 'fingertip-jump-up)


(dolist (hook (list
               'c-mode-common-hook
               'c-mode-hook
               'emacs-lisp-mode-hook
               'lisp-interaction-mode-hook
               'lisp-mode-hook
               'java-mode-hook
               'asm-mode-hook
               'haskell-mode-hook
               'rcirc-mode-hook
               'erc-mode-hook
               'sh-mode-hook
               'makefile-gmake-mode-hook
               'python-mode-hook
               'js-mode-hook
               'html-mode-hook
               'css-mode-hook
               'tuareg-mode-hook
               'go-mode-hook
               'coffee-mode-hook
               'qml-mode-hook
               'markdown-mode-hook
               'slime-repl-mode-hook
               'package-menu-mode-hook
               'cmake-mode-hook
               'php-mode-hook
               'web-mode-hook
               'coffee-mode-hook
               'sws-mode-hook
               'jade-mode-hook
               'vala-mode-hook
               'rust-mode-hook
               'ruby-mode-hook
               'qmake-mode-hook
               'lua-mode-hook
               'swift-mode-hook
               'llvm-mode-hook
               'conf-toml-mode-hook
               'nxml-mode-hook
               'nim-mode-hook
               'typescript-mode-hook
               'elixir-mode-hook
               'clojure-mode-hook
               'dart-mode-hook
               'zig-mode-hook

               'c-ts-mode-hook
               'c++-ts-mode-hook
               'cmake-ts-mode-hook
               'toml-ts-mode-hook
               'css-ts-mode-hook
               'js-ts-mode-hook
               'json-ts-mode-hook
               'python-ts-mode-hook
               'bash-ts-mode-hook
               'typescript-ts-mode-hook
               'rust-ts-mode-hook
               'java-ts-mode-hook
               'kotlin-mode-hook
               'prog-mode-hook
               'yaml-mode-hook
               'conf-mode-hook
               ))
  (add-hook hook (lambda () (display-line-numbers-mode))))
