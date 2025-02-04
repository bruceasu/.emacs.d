(provide 'init-ui)

;;(require 'lazycat-theme)
;;(lazycat-theme-load-dark)
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
  (setq doom-modeline-icon suk-icon
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

;; 字体
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
     ;;(require-package 'highlight-symbol)
     ;; Highlight the current line
     (use-package hl-line
       :ensure nil
       :hook ((after-init . global-hl-line-mode)
              ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
               (lambda () (setq-local global-hl-line-mode nil)))))
     ))

;; setup hydra
(use-package hydra
  :hook (emacs-lisp-mode . hydra-add-imenu)
  :config
  (with-eval-after-load 'posframe
    (setq hydra-hint-display-type 'posframe)
    (defun hydra-set-posframe-show-params ()
      "Set hydra-posframe style."
      (setq hydra-posframe-show-params
            `(
              :left-fringe 8
              :right-fringe 8
              :internal-border-width 2
              :internal-border-color ,(face-background 'posframe-border nil t)
              :background-color ,(face-background 'tooltip nil t)
              :foreground-color ,(face-foreground 'tooltip nil t)
              :lines-truncate t
              )))
    (hydra-set-posframe-show-params)
    (add-hook 'after-load-theme-hook #'hydra-set-posframe-show-params t))
  )

(use-package pretty-hydra
  :requires hydra
  :custom (pretty-hydra-default-title-body-format-spec " %s%s")
  :hook (emacs-lisp-mode . (lambda ()
                             (add-to-list
                              'imenu-generic-expression
                              '("Hydras" "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)" 2))))
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:inherit highlight :reverse-video t)))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat (apply f (list icon-name :face face :height height :v-adjust v-adjust)) " "))))
       (propertize title 'face face)))))

(when (display-graphic-p)
  (use-package posframe
    :hook (after-load-theme . posframe-delete-all)
    :init
    (defface posframe-border `((t (:inherit region)))
      "Face used by the `posframe' border."
      :group 'posframe)
    (defvar posframe-border-width 2
      "Default posframe border width.")
    )

  :config
  (posframe-delete-all)
  )

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
