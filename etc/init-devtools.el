(provide 'init-devtools)

;; programming modes
(use-package yaml-mode)
(use-package web-mode)
(use-package yaml-mode)
(use-package js2-mode)
(use-package rjsx-mode)
(use-package typescript-mode)
(use-package nvm)

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
  :bind
  (:map hs-minor-mode-map
        ("C-c ." . hs-toggle-hiding)
        ("C-c ," . hs-show-all)
        )
  :hook (prog-mode . hs-minor-mode)
  :config
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
  (setq hs-set-up-overlay #'hs-display-code-line-counts)
  )

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

(setenv "JAVA_HOME" "D:/green/jdk-21.0.2")

;; {{ typescript
(use-package typescript-mode
  :load-path "~/.emacs.d/extensions/typescript"
  :hook ((typescript-mode . (typescript-mode-hook-setup)))
  :config
  (defun typescript-mode-hook-setup ()
    "Set up `typescript-mode'."
    (when (my-use-tags-as-imenu-function-p)
      ;; use ctags to calculate imenu items
      (setq imenu-create-index-function
            'counsel-etags-imenu-default-create-index-function)))

  (defun my-typescript-beginning-of-defun-hack (orig-func &rest args)
    "Overwrite typescript beginning detection."
    (ignore orig-func)
    (ignore args)
    (when (my-use-tags-as-imenu-function-p)
      (let* ((closest (my-closest-imenu-item)))
        (when closest
          (imenu closest)))))
  (advice-add 'typescript-beginning-of-defun
              :around #'my-typescript-beginning-of-defun-hack)
  )

;; JSON
(unless (fboundp 'js-json-mode)
  (use-package json-mode
    :load-path "~/.emacs.d/extensions/json-mode"))

;; JavaScript
(use-package js)

(with-eval-after-load 'js-mode
  ;; '$' is part of variable name like '$item'
  (modify-syntax-entry ?$ "w" js-mode-syntax-table))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-minor-mode)
         ("\\.jsx\\'" . js2-minor-jsx-mode))
  :interpreter (("node" . js2-minor-mode)
                ("node" . js2-minor-jsx-mode))
  :hook ((js2-minor-mode . (lambda()  (js2-imenu-extras-mode)
                             (js2-highlight-unused-variables-mode)

                             )))
  :config
  ;;Latest rjsx-mode does not have indentation issue
  ;;@see https://emacs.stackexchange.com/questions/33536/how-to-edit-jsx-react-files-in-emacs
  (setq-default js2-additional-externs
                '("$"
                  "$A" ; salesforce lightning component
                  "$LightningApp" ; salesforce
                  "AccessifyHTML5"
                  "Blob"
                  "FormData"
                  "KeyEvent"
                  "Raphael"
                  "React"
                  "URLSearchParams"
                  "__dirname" ; Node
                  "_content" ; Keysnail
                  "after"
                  "afterEach"
                  "angular"
                  "app"
                  "assert"
                  "assign"
                  "before"
                  "beforeEach"
                  "browser"
                  "by"
                  "clearInterval"
                  "clearTimeout"
                  "command" ; Keysnail
                  "content" ; Keysnail
                  "decodeURI"
                  "define"
                  "describe"
                  "display" ; Keysnail
                  "documentRef"
                  "element"
                  "encodeURI"
                  "expect"
                  "ext" ; Keysnail
                  "fetch"
                  "gBrowser" ; Keysnail
                  "global"
                  "goDoCommand" ; Keysnail
                  "hook" ; Keysnail
                  "inject"
                  "isDev"
                  "it"
                  "jest"
                  "jQuery"
                  "jasmine"
                  "key" ; Keysnail
                  "ko"
                  "log"
                  "mockStore"
                  "module"
                  "mountWithTheme"
                  "plugins" ; Keysnail
                  "process"
                  "require"
                  "setInterval"
                  "setTimeout"
                  "shell" ; Keysnail
                  "tileTabs" ; Firefox addon
                  "util" ; Keysnail
                  "utag") )
  )

(use-package rjsx-mode
  :load-path "~/.emacs.d/extensions/rjsx-mode"
  :mode ("\\.js\\'")
  :hook ((rjsx-mode .  (lambda()
                         (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
                         (flycheck-select-checker 'javascript-eslint))))
  ;;:config
  ;;(add-hook 'rjsx-mode-hook 'setup)

  )


(require-package 'emmet-mode)
(use-package emmet-mode
  :defer 3
  :init (setq emmet-expand-jsx-className? t)
  :hook (web-mode typescript-mode js-mode js2-mode rjsx-mode css-mode scss-mode sgml-mode))


;; Major mode for editing web templates
(use-package web-mode
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-closing t) ; enable auto close tag in text-mode
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-auto-close-style 2)
  (setq web-mode-enable-css-colorization t)
  (setq web-mode-imenu-regexp-list
        '(("<\\(h[1-9]\\)\\([^>]*\\)>\\([^<]*\\)" 1 3 ">" nil)
          ("^[ \t]*<\\([@a-z]+\\)[^>]*>? *$" 1 " id=\"\\([a-zA-Z0-9_]+\\)\"" "#" ">")
          ("^[ \t]*<\\(@[a-z.]+\\)[^>]*>? *$" 1 " contentId=\"\\([a-zA-Z0-9_]+\\)\"" "=" ">")
          ;; angular imenu
          (" \\(ng-[a-z]*\\)=\"\\([^\"]+\\)" 1 2 "="))))


;; Adds node_modules/.bin directory to `exec_path'
(use-package add-node-modules-path
   :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

(setq-default js2-use-font-lock-faces t
              js2-mode-must-byte-compile nil
              ;; {{ comment indention in modern frontend development
              javascript-indent-level 2
              js-indent-level 2
              css-indent-offset 2
              typescript-indent-level 2
              ;; }}
              js2-strict-trailing-comma-warning nil ; it's encouraged to use trailing comma in ES6
              js2-idle-timer-delay 0.5 ; NOT too big for real time syntax check
              js2-auto-indent-p nil
              js2-indent-on-enter-key nil ; annoying instead useful
              js2-skip-preprocessor-directives t
              js2-strict-inconsistent-return-warning nil ; return <=> return null
              js2-enter-indents-newline nil
              js2-bounce-indent-p t)
