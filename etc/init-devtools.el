(provide 'init-devtools)

;; programming modes
(use-package csv-mode)
(use-package yaml-mode)
(use-package web-mode)
(use-package yaml-mode)
(use-package js2-mode)
(use-package rjsx-mode)
(use-package csv-mode)
(use-package typescript-mode)
(use-package nvm)

;; format all, formatter for almost languages
;; great for programmers
(require-package 'format-all)
(use-package format-all
  :ensure t
  :hook (prog-mode . format-all-ensure-formatter)
  :bind ("C-c f" . #'format-all-buffer))

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


(use-package apheleia
  :hook (prog-mode . apheleia-mode)
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath)))

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
  (with-eval-after-load 'pretty-hydra
    (pretty-hydra-define+ hideshow-hydra
      (:title (pretty-hydra-title "HideShow" 'octicon "nf-oct-fold")
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
        ("M->" end-of-buffer "⭸"))
       )
      ))
  (keymap-global-set "S-<f6>" #'hideshow-hydra/body)
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

;; Jump to definition
(use-package dumb-jump
  :config
  (with-eval-after-load 'pretty-hydra
    (pretty-hydra-define+ dumb-jump-hydra
      (:title (pretty-hydra-title "Dump Jump" 'faicon "nf-fa-anchor")
              :color blue :quit-key ("q" "C-g"))
      ("Jump"
       (("j" dumb-jump-go "Go")
        ("o" dumb-jump-go-other-window "Go other window")
        ("e" dumb-jump-go-prefer-external "Go external")
        ("x" dumb-jump-go-prefer-external-other-window "Go external other window"))
       "Other"
       (("i" dumb-jump-go-prompt "Prompt")
        ("l" dumb-jump-quick-look "Quick look")
        ("b" dumb-jump-back "Back")))
      )
    (keymap-global-set "C-M-j" #'dumb-jump-hydra/body)
    )
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq dumb-jump-selector 'completing-read))

;; Tree-sitter support
(when sys/linuxp
  (require-package 'treesit-auto)
  (use-package treesit-auto
    :ensure t
    :hook (after-init . global-treesit-auto-mode)
    :init
    (setq treesit-auto-install 'prompt)
    (setq treesit-language-source-alist
          '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
            (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
            (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
            (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
            (cmake      . ("https://github.com/uyha/tree-sitter-cmake"))
            (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
            (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
            (elisp      . ("https://github.com/Wilfred/tree-sitter-elisp"))
            (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
            (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
            (html       . ("https://github.com/tree-sitter/tree-sitter-html"))
            (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
            (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
            (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
            (lua        . ("https://github.com/Azganoth/tree-sitter-lua"))
            (make       . ("https://github.com/alemuller/tree-sitter-make"))
            (markdown   . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
            (ocaml      . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
            (org        . ("https://github.com/milisims/tree-sitter-org"))
            (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
            (php        . ("https://github.com/tree-sitter/tree-sitter-php"))
            (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
            (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
            (ruby       . ("https://github.com/tree-sitter/tree-sitter-ruby"))
            (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
            (sql        . ("https://github.com/m-novikov/tree-sitter-sql"))
            (vue        . ("https://github.com/merico-dev/tree-sitter-vue"))
            (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
            (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
            (zig        . ("https://github.com/GrayJack/tree-sitter-zig"))))
    (add-to-list 'major-mode-remap-alist '(sh-mode         . bash-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c-mode          . c-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c++-mode        . c++-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c-or-c++-mode   . c-or-c++-ts-mode))
    (add-to-list 'major-mode-remap-alist '(css-mode        . css-ts-mode))
    (add-to-list 'major-mode-remap-alist '(js-mode         . js-ts-mode))
    (add-to-list 'major-mode-remap-alist '(java-mode       . java-ts-mode))
    (add-to-list 'major-mode-remap-alist '(js-json-mode    . json-ts-mode))
    (add-to-list 'major-mode-remap-alist '(makefile-mode   . cmake-ts-mode))
    (add-to-list 'major-mode-remap-alist '(python-mode     . python-ts-mode))
    (add-to-list 'major-mode-remap-alist '(ruby-mode       . ruby-ts-mode))
    (add-to-list 'major-mode-remap-alist '(conf-toml-mode  . toml-ts-mode))
    (add-to-list 'auto-mode-alist '("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
    (add-to-list 'auto-mode-alist '("/go\\.mod\\'" . go-mod-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
    (add-to-list 'auto-mode-alist '("\\.y[a]?ml\\'" . yaml-ts-mode))
    )
  (global-treesit-auto-mode))

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

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :defer 2
  :hook ((after-init . global-aggressive-indent-mode)
         ;; NOTE: Disable in large files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (when (too-long-file-p)
                          (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(gitconfig-mode
                  asm-mode web-mode html-mode css-mode go-mode
                  scala-mode shell-mode term-mode vterm-mode
                  prolog-inferior-mode))
    (add-to-list 'aggressive-indent-excluded-modes mode))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands
               #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p
                      'c-mode 'c++-mode 'csharp-mode
                      'java-mode 'go-mode 'swift-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         )))))

;; 使用 built-in project library
(require 'project)

;; 设置项目根目录识别方式（默认是 Git、Mercurial、Bazaar、Subversion、Dockerfile 等）
(setq project-find-functions '(project-try-vc))

;; 可选：自定义项目根目录识别
(defun my/project-root (args)
  "Define additional ways to recognize project root."
  (or (locate-dominating-file default-directory "package.json")
      (locate-dominating-file default-directory "pom.xml")
      (locate-dominating-file default-directory "setup.py"))
  )

(add-to-list 'project-find-functions #'my/project-root)
