
;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-ide.el --- IDE configuration


(provide 'init-ide)


;; M-x legalese-insert-license
(require-package 'web-mode)
;;(require-package 'lua-mode)
(require-package 'yaml-mode)
(require-package 'js2-mode)
;; (require-package 'rjsx-mode) ; use my package in extensions
(require-package 'csv-mode)
(require-package 'emmet-mode)
;;(require-package 'groovy-mode)
;; magit sometime use packages which not released yet
;; so we place it at the end to make sure other packages are installed first
(require-package 'magit)
;;(require-package 'graphql-mode)
(require-package 'auto-yasnippet)
(require-package 'typescript-mode)
(require-package 'nvm)
;;(require-package 'lsp-mode)
;;(require-package 'elpy) ;; python
;;(require-package 'request) ;; a http client
;;(require-package 'websocket) ; for debug debugging of browsers
;;(require-package 'simple-httpd)
;;(require-package 'highlight-symbol)
;;(require-package 'cpputils-cmake)
;;(require-package 'rust-mode)

;; 设置行号
;; builtin
(require 'display-line-numbers)
;;(global-display-line-numbers-mode 1)
;; Alternatively, to use it only in programming modes:
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Line numbers are not displayed when large files are used.
(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 1000)

;;(require 'init-awesome-pair)
;; Parentheses
(use-package paren
  :ensure nil
  :config (setq-default show-paren-style 'mixed
                        show-paren-when-point-inside-paren t
                        show-paren-when-point-in-periphery t)
  :hook (prog-mode . show-paren-mode)
  )


;; 语法检查包
(require-package 'flycheck)
(use-package flycheck
  :ensure t
  :defer 3)

;; Flymake
;; 配置 Python 使用 flymake-pyflakes 后端
;; (require 'flymake)
;; (when (executable-find "pyflakes")
;;   (flymake-python-pyflakes-load))

;; 配置 JavaScript 使用 flymake-eslint 后端
;; (when (executable-find "eslint")
;;   (setq flymake-eslint-executable "eslint")
;;   (add-hook 'js-mode-hook 'flymake-eslint-load))

;; 配置 Shell 使用 flymake-shellcheck 后端
;; (when (executable-find "shellcheck")
;;   (setq flymake-shellcheck-excluded-linters '("SC2162" "SC2164"))
;;   (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;; 配置 C/C++ 使用 flymake-proc 后端（默认后端）
;; (add-hook 'c-mode-hook 'flymake-mode)
;; (add-hook 'c++-mode-hook 'flymake-mode)
;; (add-hook 'prog-mode-hook 'flymake-mode)


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
;;(require-package 'neotree)
;;(use-package neotree
;;  :commands (projectile-switch-project neotree-dir)
;;  :config
;;  (setq neo-theme 'ascii           ; NeoTree 图标的样式
;;        neo-window-width 35
;;        neo-window-fixed-size nil) ; 设置 NeoTree 窗口的宽度可以使用鼠标调整
;;  :bind ("C-c o" . projectile-switch-project))

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
;; (when (suk-treesit-available-p)
;;   (require-package 'treesit-auto)
;;   (use-package treesit-auto
;;     :ensure t
;;     :hook (after-init . global-treesit-auto-mode)
;;     :init (setq treesit-auto-install 'prompt))
;; 	(global-treesit-auto-mode)
;;     )


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


;;;###autoload
(defun run-cmd-and-replace-region (cmd)
  "Run CMD in shell on selected region or current buffer.
Then replace the region or buffer with cli output."
  (let* ((orig-point (point))
         (b (if (region-active-p) (region-beginning) (point-min)))
         (e (if (region-active-p) (region-end) (point-max))))
    (shell-command-on-region b e cmd nil t)
    (goto-char orig-point)))


;;;###autoload
(defun my-buffer-str ()
  (buffer-substring-no-properties (point-min) (point-max)))

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


;;;###autoload
(defun my-use-tags-as-imenu-function-p ()
  "Can use tags file to build imenu function"
  (my-ensure 'counsel-etags)
  (and (locate-dominating-file default-directory "TAGS")
       ;; latest universal ctags has built in parser for javascript/typescript
       (counsel-etags-universal-ctags-p "ctags")
       (memq major-mode '(typescript-mode js-mode javascript-mode))))

;; }}


;; CSS
(use-package css-mode
  :init (setq css-indent-offset 2))

;; SCSS
(use-package scss-mode
  :init (setq scss-compile-at-save nil))

;; LESS
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; JSON
(unless (fboundp 'js-json-mode)
  (use-package json-mode
    :load-path "~/.emacs.d/extensions/json-mode"))

;; JavaScript
(use-package js
  :init (setq js-indent-level 2))

(with-eval-after-load 'js-mode
  ;; '$' is part of variable name like '$item'
  (modify-syntax-entry ?$ "w" js-mode-syntax-table))

;; {{ js-beautify
(defun my-js-beautify (&optional indent-size)
  "Beautify selected region or whole buffer with js-beautify.
INDENT-SIZE decide the indentation level.
`sudo pip install jsbeautifier` to install js-beautify.'"
  (interactive "P")
  (let* ((executable (if (executable-find "js-beautify") "js-beautify"
                       "jsbeautify")))
    ;; detect indentation level
    (unless indent-size
      (setq indent-size
            (cond
             ((memq major-mode '(js-mode javascript-mode))
              js-indent-level)

             ((memq major-mode '(web-mode))
              web-mode-code-indent-offset)

             ((memq major-mode '(typescript-mode))
              typescript-indent-level)

             (t
              2))))
    ;; do it!
    (run-cmd-and-replace-region (concat executable
                                        " --stdin "
                                        " --jslint-happy --brace-style=end-expand --keep-array-indentation "
                                        (format " --indent-size=%d " indent-size)))))
;; }}




(use-package js2-mode
  :mode (("\\.js\\'" . js2-minor-mode)
         ("\\.jsx\\'" . js2-minor-jsx-mode))
  :interpreter (("node" . js2-minor-mode)
                ("node" . js2-minor-jsx-mode))
  :hook ((js2-minor-mode . (lambda()  (js2-imenu-extras-mode)
                             (js2-highlight-unused-variables-mode)

                             )))
  :config
  (defun my-validate-json-or-js-expression (&optional not-json-p)
    "Validate buffer or select region as JSON.
If NOT-JSON-P is not nil, validate as Javascript expression instead of JSON."
    (interactive "P")
    (let* ((json-exp (if (region-active-p) (my-selected-str)
                       (my-buffer-str)))
           (jsbuf-offet (if not-json-p 0 (length "var a=")))
           errs
           first-err
           (first-err-pos (if (region-active-p) (region-beginning) 0)))
      (unless not-json-p
        (setq json-exp (format "var a=%s;"  json-exp)))
      (with-temp-buffer
        (insert json-exp)
        (my-ensure 'js2-mode)
        (js2-parse)
        (setq errs (js2-errors))
        (cond
         ((not errs)
          (message "NO error found. Good job!"))
         (t
          ;; yes, first error in buffer is the last element in errs
          (setq first-err (car (last errs)))
          (setq first-err-pos (+ first-err-pos (- (cadr first-err) jsbuf-offet)))
          (message "%d error(s), first at buffer position %d: %s"
                   (length errs)
                   first-err-pos
                   (js2-get-msg (caar first-err))))))
      (if first-err (goto-char first-err-pos))))

  (defun my-print-json-path (&optional hardcoded-array-index)
    "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is replaced with it."
    (interactive "P")
    (cond
     ((memq major-mode '(js2-mode))
      (js2-print-json-path hardcoded-array-index))
     (t
      (let* ((cur-pos (point))
             (str (my-buffer-str)))
        (when (string= "json" (file-name-extension buffer-file-name))
          (setq str (format "var a=%s;" str))
          (setq cur-pos (+ cur-pos (length "var a="))))
        (my-ensure 'js2-mode)
        (with-temp-buffer
          (insert str)
          (js2-init-scanner)
          (js2-do-parse)
          (goto-char cur-pos)
          (js2-print-json-path))))))
  (defun my-print-json-path (&optional hardcoded-array-index)
    "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is replaced with it."
    (interactive "P")
    (cond
     ((memq major-mode '(js2-mode))
      (js2-print-json-path hardcoded-array-index))
     (t
      (let* ((cur-pos (point))
             (str (my-buffer-str)))
        (when (string= "json" (file-name-extension buffer-file-name))
          (setq str (format "var a=%s;" str))
          (setq cur-pos (+ cur-pos (length "var a="))))
        (my-ensure 'js2-mode)
        (with-temp-buffer
          (insert str)
          (js2-init-scanner)
          (js2-do-parse)
          (goto-char cur-pos)
          (js2-print-json-path))))))


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

;; @see https://github.com/felipeochoa/rjsx-mode/issues/33
(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil))

(require-package 'emmet-mode)
(use-package emmet-mode
  :defer 3
  :init (setq emmet-expand-jsx-className? t)
  :hook (web-mode typescript-mode js-mode js2-mode rjsx-mode css-mode scss-mode sgml-mode))


;; Format HTML, CSS and JavaScript/JSON
;; Install: npm -g install prettier
(when (executable-find "prettier")
  (use-package prettier
    :diminish
    :hook ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
    :init (setq prettier-pre-warm 'none)))

(use-package prettier-js
  :ensure t
  :defer 3
  :hook ((css-mode web-mode typescript-mode js-mode json-mode js2-mode) . prettier-js-mode))



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

(with-eval-after-load 'js-mode
  ;; '$' is part of variable name like '$item'
  (modify-syntax-entry ?$ "w" js-mode-syntax-table))

(defun my-validate-json-or-js-expression (&optional not-json-p)
  "Validate buffer or select region as JSON.
If NOT-JSON-P is not nil, validate as Javascript expression instead of JSON."
  (interactive "P")
  (let* ((json-exp (if (region-active-p) (my-selected-str)
                     (my-buffer-str)))
         (jsbuf-offet (if not-json-p 0 (length "var a=")))
         errs
         first-err
         (first-err-pos (if (region-active-p) (region-beginning) 0)))
    (unless not-json-p
      (setq json-exp (format "var a=%s;"  json-exp)))
    (with-temp-buffer
      (insert json-exp)
      (my-ensure 'js2-mode)
      (js2-parse)
      (setq errs (js2-errors))
      (cond
       ((not errs)
        (message "NO error found. Good job!"))
       (t
        ;; yes, first error in buffer is the last element in errs
        (setq first-err (car (last errs)))
        (setq first-err-pos (+ first-err-pos (- (cadr first-err) jsbuf-offet)))
        (message "%d error(s), first at buffer position %d: %s"
                 (length errs)
                 first-err-pos
                 (js2-get-msg (caar first-err))))))
    (if first-err (goto-char first-err-pos))))

(defun my-print-json-path (&optional hardcoded-array-index)
  "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is replaced with it."
  (interactive "P")
  (cond
   ((memq major-mode '(js2-mode))
    (js2-print-json-path hardcoded-array-index))
   (t
    (let* ((cur-pos (point))
           (str (my-buffer-str)))
      (when (string= "json" (file-name-extension buffer-file-name))
        (setq str (format "var a=%s;" str))
        (setq cur-pos (+ cur-pos (length "var a="))))
      (my-ensure 'js2-mode)
      (with-temp-buffer
        (insert str)
        (js2-init-scanner)
        (js2-do-parse)
        (goto-char cur-pos)
        (js2-print-json-path))))))

(with-eval-after-load 'js2-mode
  ;; I hate the hotkeys to hide things
  (define-key js2-mode-map (kbd "C-c C-e") nil)
  (define-key js2-mode-map (kbd "C-c C-s") nil)
  (define-key js2-mode-map (kbd "C-c C-f") nil)
  (define-key js2-mode-map (kbd "C-c C-t") nil)
  (define-key js2-mode-map (kbd "C-c C-o") nil)
  (define-key js2-mode-map (kbd "C-c C-w") nil))
;; }}

(defun my-js2-mode-setup()
  "Set up javascript."
  ;; if use node.js we need nice output
  (js2-imenu-extras-mode)
  (setq mode-name "JS2")
  ;; counsel/ivy is more generic and powerful for refactoring
  ;; js2-mode has its own syntax linter

  ;; call js-doc commands through `counsel-M-x'!

  ;; @see https://github.com/mooz/js2-mode/issues/350
  (setq forward-sexp-function nil))

(add-hook 'js2-mode-hook 'my-js2-mode-setup)

;; @see https://github.com/felipeochoa/rjsx-mode/issues/33
(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil))

;; {{ js-beautify
(defun my-js-beautify (&optional indent-size)
  "Beautify selected region or whole buffer with js-beautify.
INDENT-SIZE decide the indentation level.
`sudo pip install jsbeautifier` to install js-beautify.'"
  (interactive "P")
  (let* ((executable (if (executable-find "js-beautify") "js-beautify"
                       "jsbeautify")))
    ;; detect indentation level
    (unless indent-size
      (setq indent-size
            (cond
             ((memq major-mode '(js-mode javascript-mode))
              js-indent-level)

             ((memq major-mode '(web-mode))
              web-mode-code-indent-offset)

             ((memq major-mode '(typescript-mode))
              typescript-indent-level)

             (t
              2))))
    ;; do it!
    (run-cmd-and-replace-region (concat executable
                                        " --stdin "
                                        " --jslint-happy --brace-style=end-expand --keep-array-indentation "
                                        (format " --indent-size=%d " indent-size)))))
;; }}


;; Latest rjsx-mode does not have indentation issue
;; @see https://emacs.stackexchange.com/questions/33536/how-to-edit-jsx-react-files-in-emacs
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
                "utag"))

(defun my-typescript-beginning-of-defun-hack (orig-func &rest args)
  "Overwrite typescript beginning detection."
  (ignore orig-func)
  (ignore args)
  (when (my-use-tags-as-imenu-function-p)
    (let* ((closest (my-closest-imenu-item)))
      (when closest
        (imenu closest)))))
(advice-add 'typescript-beginning-of-defun :around #'my-typescript-beginning-of-defun-hack)

;; {{ typescript
(defun typescript-mode-hook-setup ()
  "Set up `typescript-mode'."
  (when (my-use-tags-as-imenu-function-p)
    ;; use ctags to calculate imenu items
    (setq imenu-create-index-function
          'counsel-etags-imenu-default-create-index-function)))
(add-hook 'typescript-mode-hook 'typescript-mode-hook-setup)
;; }}


;; Flexible text folding
(use-package hideshow
  :ensure t
  :diminish hs-minor-mode
  :pretty-hydra
  ((:title (pretty-hydra-title "HideShow" 'octicon "nf-oct-fold")
           :color amaranth
           :quit-key ("q" "C-g"))
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
     ("M-<" beginning-of-buffer "⭶"))
    ))
  :bind (:map hs-minor-mode-map
              ("C-~" . hideshow-hydra/body)
              ("C-S-<escape>" . hideshow-hydra/body))
  :hook (prog-mode . hs-minor-mode)
  :config
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
        ))
    )
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
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; settings for LSP
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :hook ((c-mode c++-mode go-mode java-mode js-mode python-mode rust-mode web-mode) . eglot-ensure)
  :bind (("C-c e f" . #'eglot-format)
         ("C-c e i" . #'eglot-code-action-organize-imports)
         ("C-c e q" . #'eglot-code-action-quickfix))
  :config
  ;; (setq eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (defun eglot-actions-before-save()
    (add-hook 'before-save-hook (lambda ()
                                  (call-interactively #'eglot-format)
                                  (call-interactively #'eglot-code-action-organize-imports))))
  (add-to-list 'eglot-server-programs '(web-mode "vls"))
  (add-hook 'eglot--managed-mode-hook #'eglot-actions-before-save))


(setenv "PATH" (concat (getenv "PATH") ";c:/green/python311/"))
(add-to-list 'exec-path "c:/green/python311")
