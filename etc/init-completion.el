(provide 'init-completion)

(when (display-graphic-p)
  (use-package vertico
    :bind (:map vertico-map
                ("RET"   . vertico-directory-enter)
                ("DEL"   . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word))
    :hook ((after-init . vertico-mode)
           (rfn-eshadow-update-overlay . vertico-directory-tidy))
    )
   (use-package vertico-posframe
      :ensure t
      :after (posframe vertico)
      :hook (vertico-mode . vertico-posframe-mode)
      :init (setq vertico-posframe-parameters '((left-fringe  . 8) (right-fringe . 8)))
      )
  )

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

(use-package nerd-icons-corfu
  :after corfu
  :init (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;;据说跟 lsp-bridge 冲突
(setq company-idle-delay nil) 
;; (global-set-key (kbd "M-/") 'company-complete)  ;; 绑定 M-/ 为 company-complete
;; (global-set-key (kbd "M-?") 'dabbrev-expand)    ;; 绑定 M-? 为 dabbrev-expand
(lazy-load-global-keys
 '(
   ("M-/" . company-complete)
   ("M-?" . dabbrev-expand)

   )
 "init-completion-company"
 )

;; Optionally use the `orderless' completion style.
(require-package 'orderless)
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

(use-package consult
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
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
         ("M-'"     . consult-register-store)      ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#"   . consult-register)
         ;; Other custom bindings
         ("M-y"     . consult-yank-pop)            ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e"   . consult-compile-error)
         ("M-g g"   . consult-goto-line)           ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o"   . consult-outline)             ;; Alternative: consult-org-heading
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g i"   . consult-imenu)
         ("M-g I"   . consult-imenu-multi)
         ;; ("M-g M"   . consult-man)
         ;; ("M-g I"   . consult-info)
         ("M-g t"   . consult-theme)
         ("M-g h"   . consult-history)
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
         ("C-r" .  consult-line)
         :map isearch-mode-map
         ("M-e"     . consult-isearch-history)
         ("M-s e"   . consult-isearch-history)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ) 

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
  :config
  (use-package consult-flyspell
    :bind ("M-g s" . consult-flyspell))

  (use-package consult-yasnippet
    :bind ("M-g y" . consult-yasnippet))
  ;; Use Consult to select xref locations with preview
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))
  )

(use-package nerd-icons-completion
  :when (icons-displayable-p)
  :hook (vertico-mode . nerd-icons-completion-mode))

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

;; More utils
(use-package shackle)

(unless (display-graphic-p)
  ;; only conole packages
  )
