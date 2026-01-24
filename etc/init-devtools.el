(provide 'init-devtools)

;; programming modes
(use-package yaml-mode)
(use-package web-mode)
(use-package yaml-mode)
(use-package js2-mode)
(use-package nvm)

;; format all, formatter for almost languages
;; great for programmers
(use-package format-all)
(use-package format-all
  :ensure t
  :hook (prog-mode . format-all-ensure-formatter)
  :bind ("C-c f" . #'format-all-buffer))

(when suk-copilot-enable

  (when sys/win32p 
    (setq copilot-node-executable "C:\\green\\node-v20.10.0-win-x64\\node.exe")
    (add-to-list 'load-path "C:\\green\\emacs-suk\\.emacs.d\\extensions\\copilot\\copilot.el")

    )
  (unless sys/win32p
    (setq copilot-node-executable "~/.nvm/versions/node/v22.13.0/bin/node")
    (add-to-list 'load-path "~/.emacs.d/extensions/copilot/copilot.el")
    )


  (use-package copilot)
  (with-eval-after-load 'copilot

    ;; 全局设置 Copilot 的缩进偏移量为 4
    (setq copilot-indent-offset 4)
    ;; 设置 Copilot 缩进偏移量
    (let ((copilot-offsets
	   '((python-mode . 4)
	     (c-mode . 2)
	     (js-mode . 2)
	     (bat-mode . 2))))
      (dolist (pair copilot-offsets)
	(let ((mode (car pair))
	      (offset (cdr pair)))
	  (add-hook mode (lambda () (setq copilot-indent-offset offset))))))
    (add-hook 'prog-mode-hook 'copilot-mode)

    ;; To customize the behavior of copilot-mode, please check copilot-enable-predicates and copilot-disable-predicates.
    ;; You need to bind copilot-complete to some key and call copilot-clear-overlay inside post-command-hook.
    (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
    (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
    (add-to-list 'copilot-major-mode-alist '("jsonl" . "json"))
    ;; Increase the maximum character limit for Copilot
    (setq copilot-max-char 200000) ;; Adjust the value as needed
    ;; Disable Copilot file size warnings
    (setq copilot-disable-size-check t) ;; Hypothetical variable
    ;;;; 忽略 Copilot 的特定警告
    ;;(add-to-list 'warning-suppress-types '(copilot quail--infer-indentation-offset found no mode-specific indentation offset))
    ;;;; 设置 Emacs 的最低警告级别为 warning`，忽略 `emergency 级别以下的警告
    ;; (setq warning-minimum-level :warning)
    ;; 闭嘴
    (setq copilot-indent-offset-warning-disable t)
    )

  ;; Login to Copilot by M-x copilot-login. You can also check the
  ;; status by M-x copilot-diagnose (NotAuthorized means you don't have
  ;; a valid subscription).
  )

(use-package flycheck :ensure t :init (global-flycheck-mode))
(use-package dap-mode
  :ensure t
  :after (lsp-mode)
  :functions dap-hydra/nil
  :config
  (require 'dap-java)
  :bind (:map lsp-mode-map
	      ("<f5>" . dap-debug)
	      ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
	 (dap-session-created . (lambda (&_rest) (dap-hydra)))
	 (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))

(use-package dap-java :ensure nil)

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-delay 1.5
	      lsp-ui-doc-position 'bottom
	      lsp-ui-doc-max-width 100
	      ))


(use-package lsp-mode
  :ensure t
  :hook (
	 (lsp-mode . lsp-enable-which-key-integration)
	 (java-mode . #'lsp-deferred)
	 )
  :init (setq
	 lsp-keymap-prefix "C-c l"              ; this is for which-key integration documentation, need to use lsp-mode-map
	 lsp-enable-file-watchers nil
	 read-process-output-max (* 1024 1024)  ; 1 mb
	 lsp-completion-provider :capf
	 lsp-idle-delay 0.500
	 )
  :config
  (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
  (with-eval-after-load 'lsp-intelephense
    (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  )

(use-package lsp-java
  :ensure t
  :config (add-hook 'java-mode-hook 'lsp))

(setenv "JAVA_HOME" "C:/green/jdk-21.0.2")

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
