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

;; 在 Emacs 中，shackle 是一个专门用于控制弹出窗口（pop-up windows）和
;; 缓冲区显示方式的包。它可以帮助你为特定类型的 buffer 制定“在哪个窗口
;; 打开”、“打开时如何分割窗口”、“窗口大小”等规则，从而摆脱 Emacs 默认
;; 情况下可能随机或难以预测的弹出方式。与较早的 popwin 类似，但
;; shackle 提供了更灵活、细粒度的控制。
(use-package shackle
  :init
  (shackle-mode 1)
  :config
  (setq shackle-rules
	'((compilation-mode :select t :align 'below :size 0.3)
	  ("*Help*" :select t :align 'below :size 0.3)
	  ("*Messages*" :select nil :align 'below :size 0.3)
	  ("^\\*magit.*\\*$" :regexp t :align 'right :size 0.4 :select t))))

;;据说跟 lsp-bridge 冲突
(setq company-idle-delay nil) 
;; (global-set-key (kbd "M-/") 'company-complete)  ;; 绑定 M-/ 为 company-complete
;; (global-set-key (kbd "M-?") 'dabbrev-expand)    ;; 绑定 M-? 为 dabbrev-expand
(lazy-load-global-keys
 '(
   ;;("M-/" . company-complete)
   ("M-/"  . my/company-complete-dwim)
   ("M-?" . dabbrev-expand)

   )
 "init-completion-company"
 )

(when sys/is-gui
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

;; 字体
(use-package all-the-icons)

(lazy-load-set-keys
 '(
   ("C--" . text-scale-decrease)        ;减小字体大小
   ("C-=" . text-scale-increase)        ;增加字体大小
   ("C-x C-0" . text-scale-adjust)
   ))


(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))


;; Use fixed pitch where it's sensible
;;  (use-package mixed-pitch :diminish)
(require 'load-set-font)

(when (display-graphic-p)
  (use-package centaur-tabs
    :demand
    :init
    ;; Set the style to rounded with icons
    (setq centaur-tabs-style "bar")
    (setq centaur-tabs-set-icons t)
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

  ;; 图标支持
  (use-package all-the-icons
    ;; :ensure t
    :load-path "~/.emacs.d/extensions/all-the-icons"
    :if (display-graphic-p))
  )

(run-with-idle-timer
 9
 nil
 #'(lambda()
     ;;(use-package highlight-symbol)
     ;; Highlight the current line
     (use-package hl-line
       :ensure nil
       :hook ((after-init . global-hl-line-mode)
              ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
               (lambda () (setq-local global-hl-line-mode nil)))))
     ))

;; Frame transparence
(use-package transwin)
(lazy-load-global-keys
 '(
   ("C-M-9" . transwin-inc)
   ("C-M-8" . transwin-dec)
   ("C-M-7" . transwin-toggle)
   )
 "transwin"
 )

(with-eval-after-load 'transwin
  (setq transwin-parameter-alpha 'alpha-background))

(lazy-load-global-keys
'(
  ("C-x C-h" . my-hydra-describe/body)
  )
"init-description")

;; Optimization
(setq idle-update-delay 1.0)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; (when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; GUI Environment
(when (display-graphic-p)
  ;; Don't use GTK+ tooltip
  (when (boundp 'x-gtk-use-system-tooltips)
    (setq x-gtk-use-system-tooltips nil))
  ;; scroll-bar
  (set-scroll-bar-mode 'right)
  ;; 隐藏垂直滚动条。
  ;;(modify-all-frames-parameters '((vertical-scroll-bars)))
  )
