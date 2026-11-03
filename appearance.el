(when (display-graphic-p)
  (with-eval-after-load 'ibuffer
    (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
    (use-package nerd-icons-ibuffer
      :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
      :init (setq nerd-icons-ibuffer-icon t))))

;; Graphical title treatment is optional; the package itself is loaded by core.el.
(when (display-graphic-p)
  (setq pretty-hydra-default-title-body-format-spec " %s%s")
  (cl-defun pretty-hydra-title
      (title &optional icon-type icon-name &key face height v-adjust)
    "Add an optional icon and face to TITLE."
    (let ((face (or face (list :inherit highlight :reverse-video t)))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (fboundp 'icons-displayable-p)
                  (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face)))))
(when (display-graphic-p)
  (use-package which-key-posframe
    :after (which-key posframe)
    :config
    (which-key-posframe-mode 1))
  (lazy-load-global-keys
   '(("t" . toggles-hydra/body))
   "init-toggles"
   "M-m"))
(with-eval-after-load 'hydra
  (with-eval-after-load 'posframe
    (setq hydra-hint-display-type 'posframe)
    (defun hydra-set-posframe-show-params ()
      "Set Hydra's optional posframe style."
      (setq hydra-posframe-show-params
            (list :left-fringe 8
                  :right-fringe 8
                  :internal-border-width 2
                  :internal-border-color (face-background 'posframe-border nil t)
                  :background-color (face-background 'tooltip nil t)
                  :foreground-color (face-foreground 'tooltip nil t)
                  :lines-truncate t))
      (add-hook 'after-load-theme-hook #'hydra-set-posframe-show-params t))
    (hydra-set-posframe-show-params)))

;; 在 Emacs 中，shackle 是一个专门用于控制弹出窗口（pop-up windows）和
;; 缓冲区显示方式的包。它可以帮助你为特定类型的 buffer 制定“在哪个窗口
;; 打开”、“打开时如何分割窗口”、“窗口大小”等规则，从而摆脱 Emacs 默认
;; 情况下可能随机或难以预测的弹出方式。与较早的 popwin 类似，但
;; shackle 提供了更灵活、细粒度的控制。
(use-package shackle
  :demand t
  :config
  (shackle-mode 1)
  (setq shackle-rules
      '((compilation-mode :select t :align 'below :size 0.3)
	("*Help*" :select t :align 'below :size 0.3)
	("*Messages*" :select nil :align 'below :size 0.3)
	("^\\*magit.*\\*$" :regexp t :align 'right :size 0.4 :select t))))

(when (display-graphic-p)
  (use-package posframe
    :demand t
    :hook (after-load-theme . posframe-delete-all)
    :init
    (defface posframe-border `((t (:inherit region)))
      "Face used by the `posframe' border."
      :group 'posframe)
    (defvar posframe-border-width 2
      "Default posframe border width.")
    :config
    (posframe-delete-all)))

(when (display-graphic-p)
  (use-package vertico-posframe
    :demand t
    :hook (vertico-mode . vertico-posframe-mode)
    :init (setq vertico-posframe-parameters '((left-fringe  . 8) (right-fringe . 8)))
    :config
    ;; Appearance loads after Vertico has already started during init.
    (when (bound-and-true-p vertico-mode)
      (vertico-posframe-mode 1))
    )
  )

(when (display-graphic-p)
  (use-package org-modern
    :init
    ;; Emacs 30 cannot run org-modern byte-code compiled with Emacs 31.
    ;; Load the source so `without-restriction' expands for this Emacs.
    (when (< emacs-major-version 31)
      (load "org-modern.el" nil t t))
    :custom
    (prettify-symbols-alist nil)
    :hook ((org-mode . org-modern-mode)
           (org-agenda-finalize . org-modern-agenda))))

(when (display-graphic-p)
  (use-package doom-themes
    :ensure t
    :custom
    (doom-themes-enable-bold t)
    (doom-themes-enable-italic t)
    ;; 加载一个主题，DOOM One 是 DOOM Emacs 的默认主题，非常美观
    :init
    (load-theme 'doom-one t)
    )


  (use-package doom-modeline
    :hook (after-init . doom-modeline-mode)
    :init
    (setq doom-modeline-icon t
          doom-modeline-minor-modes t)
    :config
    (column-number-mode 1)
    :custom
    (doom-modeline-height 30)
    (doom-modeline-window-width-limit nil)
    (doom-modeline-buffer-file-name-style 'truncate-with-project)
    (doom-modeline-icon t)
    (doom-modeline-time t)
    (doom-modeline-vcs-max-leghth 50)
    ;; Windows下记得加上
    (if sys/win32p (setq inhibit-compacting-font-caches t))
    )

  (use-package hide-mode-line
    :hook (((treemacs-mode
             eshell-mode shell-mode
             term-mode vterm-mode
             embark-collect-mode
             lsp-ui-imenu-mode
             pdf-annot-list-mode) . turn-on-hide-mode-line-mode)
           (dired-mode . (lambda()
                           (and (bound-and-true-p hide-mode-line-mode)
                                (turn-off-hide-mode-line-mode))))))

  ;; A minor-mode menu for mode-line
  (use-package minions
    :hook (doom-modeline-mode . minions-mode))
  )

(when (display-graphic-p)
  ;; 字体
  (lazy-load-set-keys
   '(("C--" . text-scale-decrease)
     ("C-=" . text-scale-increase)
     ("C-x C-0" . text-scale-adjust)))


  (defun font-installed-p (font-name)
    "Check if font with FONT-NAME is available."
    (find-font (font-spec :name font-name)))


;; Use fixed pitch where it's sensible
  ;;  (use-package mixed-pitch :diminish)
  (require 'load-set-font))

(when (display-graphic-p)
  (use-package centaur-tabs
    :demand
    :init
    ;; Set the tab style.
    (setq centaur-tabs-style "bar")
    ;; Use the Nerd Icons family consistently; avoid All The Icons as a second stack.
    (setq centaur-tabs-set-icons nil)
    :config
    (centaur-tabs-mode t)
    :bind
    ("C-<prior>" . centaur-tabs-backward)  ;; Ctrl PgUp
    ("C-<next>"  . centaur-tabs-forward))  ;; Ctrl PgDn
  )

(when (display-graphic-p)
  ;; Icons
  (use-package nerd-icons
    :config
    (when (and (display-graphic-p)
	       (not (font-installed-p nerd-icons-font-family)))
      (nerd-icons-install-fonts t)))

  )

(when (display-graphic-p)
  (use-package hl-line
    :ensure nil
    :demand t
    :hook ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
           (lambda () (setq-local global-hl-line-mode nil)))
    :config (global-hl-line-mode 1)))

(when (display-graphic-p)
  ;; Frame transparency
  (use-package transwin)
  (lazy-load-global-keys
   '(("C-M-9" . transwin-inc)
     ("C-M-8" . transwin-dec)
     ("C-M-7" . transwin-toggle))
   "transwin")

  (with-eval-after-load 'transwin
    (setq transwin-parameter-alpha 'alpha-background)))

(unless (display-graphic-p)
  (global-display-line-numbers-mode 0)
  (setq-default line-spacing 0.2)
  (global-visual-line-mode 1)
  (setq cursor-type 'bar)
  (unless (cl-some #'display-graphic-p (frame-list))
    (load-theme 'tango-dark t))
  (setq scroll-margin 3
        scroll-conservatively 101))
