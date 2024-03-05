;;; init-ide.el --- IDE configuration

;;; Commentary:
;;
;; IDE configuration
;;

;;; Code:



(eval-when-compile
  (require '+const)
  (require '+custom)
  (require 'init-package))

;; 语法检查包
(use-package flycheck
  :ensure t
  :defer 3)

;; format all, formatter for almost languages
;; great for programmers
(use-package format-all
  :hook (prog-mode . format-all-ensure-formatter)
  :bind ("C-c f" . #'format-all-buffer))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; 折叠和收缩代码
(use-package hideshow
  :ensure nil
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

;;代码折叠
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'ess-mode-hook        'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)

;; 代码片段
(require 'yasnippet)
(use-package yasnippet
  :load-path "~/.emacs.d/extensions/yasnippet"
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/share/snippets"))
  (yas-global-mode 1)
  (autoload 'yas-minor-mode-on "yasnippet")
  )


(dolist (x '(org-mode-hook prog-mode-hook snippet-mode-hook))
  (add-hook x #'yas-minor-mode-on))


(use-package projectile
  :ensure t
  :config
  ;;(setq projectile-completion-system 'ido)
  ;;(setq ido-enable-flex-matching t)
  (setq projectile-completion-system 'ivy)
  ;; Eanble Projectile globally
  (projectile-mode 1)
  ;; Set akeybinding for projectile commands
  (global-set-key (kbd "C-c p") 'projectile-commander))

;;(setq copilot-node-executable "C:\\green\\node-v20.10.0-win-x64\\node.exe")
;;(add-to-list 'load-path "C:\\green\\emacs-29.1\\.emacs.d\\extensions\\copilot\\copilot.el")

;;(require 'copilot)
;;(add-hook 'prog-mode-hook 'copilot-mode)

;; To customize the behavior of copilot-mode, please check copilot-enable-predicates and copilot-disable-predicates.
;; You need to bind copilot-complete to some key and call copilot-clear-overlay inside post-command-hook.
;;(define-key copilot-completion-map
;;            (kbd "<tab>")
;;            'copilot-accept-completion)
;;(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
;; (add-to-list 'copilot-major-mode-alist '("c" . "cpp" . "css" . "go" . "java" . "html" . "javascript" . "javascriptreact" . "json" . "python" . "sql" . "shellscript"))
;; Login to Copilot by M-x copilot-login. You can also check the status by M-x copilot-diagnose (NotAuthorized means you don't have a valid subscription).

;; Prettify Symbols
;; e.g. display “lambda” as “λ”
(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :init
  (setq-default prettify-symbols-alist suk-prettify-symbols-alist)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

;; Tree-sitter support
(when (suk-treesit-available-p)
  (use-package treesit-auto
    :hook (after-init . global-treesit-auto-mode)
    :init (setq treesit-auto-install 'prompt)))


;; Show function arglist or variable docstring
;; (use-package eldoc
;;   :ensure nil
;;   :diminish
;;   :config
;;   (when (childframe-workable-p)
;;     (use-package eldoc-box
;;       :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
;;       :custom
;;       (eldoc-box-lighter nil)
;;       (eldoc-box-only-multi-line t)
;;       (eldoc-box-clear-with-C-g t)
;;       :custom-face
;;       (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
;;       (eldoc-box-body ((t (:inherit tooltip))))
;;       :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
;;       :config
;;       ;; Prettify `eldoc-box' frame
;;       (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
;;             (alist-get 'right-fringe eldoc-box-frame-parameters) 8))))


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

;; Misc. programming modes
(use-package csv-mode)
(unless emacs/>=29p
  (use-package csharp-mode))
(use-package cmake-mode)
(use-package powershell)
(use-package vimrc-mode)
(use-package yaml-mode)
(use-package typescript-mode
 :load-path "~/.emacs.d/extensions/typescript")

(require 'init-treemacs)
(require 'init-lang-vcs)
(require 'init-lang-lsp)
(require 'init-lang-check)
(require 'init-lang-dap)
(require 'init-lang-web)
(require 'init-lang-elisp)
(require 'init-lang-c)
(require 'init-lang-python)
(provide 'init-ide)

;;; init-ide.el ends here
