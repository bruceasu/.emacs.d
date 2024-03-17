;;; init-completion.el --- Initialize completion configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Modern completion configuration.
;;

;;; Code:

(provide 'init-completion)
(use-package emacs
  :init
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)

  ;; Only list the commands of the current modes
  (when (boundp 'read-extended-command-predicate)
    (setq read-extended-command-predicate
          #'command-completion-default-include-p))

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

;; 著名的Emacs补全框架, 为 LSP 提供额外的功能，如自动补全
(use-package company
  :defer 2
  :hook (after-init . global-company-mode)
  :init (setq company-tooltip-align-annotations t
              company-idle-delay 0 company-echo-delay 0
              company-minimum-prefix-length 1
              company-require-match nil
              company-dabbrev-ignore-case nil
              company-dabbrev-downcase nil
              company-show-numbers t)
  :config
  (setq switch-window-input-style 'minibuffer)
  (setq switch-window-increase 4)
  (setq switch-window-threshold 2)
  (setq switch-window-shortcut-sytle 'querty)
  (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l"))
  (global-company-mode)
  :bind (:map company-active-map
              ("C-n" . #'company-select-next)
              ("C-p" . #'company-select-previous)
              ("TAB" . company-complete-selection)
              ("M-h" . company-complete-selection)
              ("M-H" . company-complete-common)
              ("M-s" . company-search-candidates)
              ("M-S" . company-filter-candidates)
              ("M-n" . company-select-next)
              ("M-p" . company-select-previous))
  (:map leader-key
        ("c s" . #'company-yasnippet
         ))
  )
(use-package company-box
  :ensure nil)


;; Optionally use the `orderless' completion style.
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

(use-package vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(when (childframe-completion-workable-p)
  (use-package vertico-posframe
    :hook (vertico-mode . vertico-posframe-mode)
    :init (setq vertico-posframe-poshandler
                #'posframe-poshandler-frame-center-near-bottom
                vertico-posframe-parameters
                '((left-fringe  . 8)
                  (right-fringe . 8)))
    )
  )

(use-package nerd-icons-completion
  :when (icons-displayable-p)
  :hook (vertico-mode . nerd-icons-completion-mode))


;; 增强了搜索功能
;; (use-package swiper
;;   :bind
;;   (
;;    ("C-x M-s" . swiper)
;;    ("C-x C-F"  . counsel-find-file)
;;    ("C-x C-M-x" . counsel-M-x)
;;    )
;;   :config
;;   (progn
;;     (ivy-mode 1)
;;     (setq ivy-use-virtual-buffers t)
;;     (setq ivy-display-style 'fancy)
;;     ;;(define-key read-expression-map (kbd "C-r") 'counsel-expression-history))
;;     ))


(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ("C-c h"   . consult-history)
         ("C-c k"   . consult-kmacro)
         ("C-c m"   . consult-man)
         ("C-c i"   . consult-info)
         ("C-c r"   . consult-ripgrep)
         ("C-c T"   . consult-theme)
         ("C-."     . consult-imenu)

         ;;("C-c c e" . consult-colors-emacs)
         ;;("C-c c w" . consult-colors-web)
         ;;("C-c c f" . describe-face)
         ;;("C-c c t" . consult-theme)

         ([remap Info-search]        . consult-info)
         ([remap isearch-forward]    . consult-line)
         ([remap recentf-open-files] . consult-recent-file)

         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#"     . consult-register-load)
         ("M-'"     . consult-register-store)        ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#"   . consult-register)
         ;; Other custom bindings
         ("M-y"     . consult-yank-pop)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e"   . consult-compile-error)
         ("M-g g"   . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o"   . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d"   . consult-find)
         ("M-s D"   . consult-locate)
         ("M-s g"   . consult-grep)
         ("M-s G"   . consult-git-grep)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-s k"   . consult-keep-lines)
         ("M-s u"   . consult-focus-lines)
         ;; Isearch integration
         ("M-s e"   . consult-isearch-history)
         :map isearch-mode-map
         ("M-e"     . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s e"   . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l"   . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L"   . consult-line-multi)            ;; needed by consult-line to detect isearch

         ;; Minibuffer history
         :map minibuffer-local-map
         ("C-s" . (lambda ()
                    "Insert the selected region or current symbol at point."
                    (interactive)
                    (insert (with-current-buffer
                                (window-buffer (minibuffer-selected-window))
                              (or (and transient-mark-mode mark-active (/= (point) (mark))
                                       (buffer-substring-no-properties (point) (mark)))
                                  (thing-at-point 'symbol t)
                                  "")))))
         ("M-s" . consult-history) ;;orig. next-matching-history-element
         ("M-r" . consult-history))   ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))

  ;; More utils

  (use-package consult-flyspell
    :bind ("M-g s" . consult-flyspell))

  (use-package consult-yasnippet
    :bind ("M-g y" . consult-yasnippet)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-completion.el ends here
