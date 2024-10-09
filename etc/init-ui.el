;;; init-ui.el --- Initialize UI configurations.	-*- lexical-binding: t -*-


(provide 'init-ui)

(eval-when-compile
  (require '+const)
  (require '+custom)
  (require '+fn)
  (require 'init-package)
  )

;; Optimization
(setq idle-update-delay 1.0)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1)) ; ģánbèi gunggêi lán.
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ;ģánbèi coidán lán
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ;ģánbèi ģandung tiu
;; Compatibility
(use-package compat :demand t)

;; 字体
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))


;; Display ugly ^L page breaks as tidy horizontal lines
(require-package 'page-break-lines)
(use-package page-break-lines
  :diminish
  :hook (after-init . global-page-break-lines-mode))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

;; Display icons for buffers
(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :init (setq nerd-icons-ibuffer-icon suk-icon))

(require-package 'hydra)
(use-package hydra
  :hook (emacs-lisp-mode . hydra-add-imenu)
  :init
  (when (childframe-completion-workable-p)
    (setq hydra-hint-display-type 'posframe)
    (with-eval-after-load 'posframe
      (defun hydra-set-posframe-show-params ()
        "Set hydra-posframe style."
        (setq hydra-posframe-show-params
              `(:left-fringe 8
                :right-fringe 8
                :internal-border-width 2
                :internal-border-color ,(face-background 'posframe-border nil t)
                :background-color ,(face-background 'tooltip nil t)
                :foreground-color ,(face-foreground 'tooltip nil t)
                :lines-truncate t
                :poshandler posframe-poshandler-frame-center-near-bottom)))
      (hydra-set-posframe-show-params)
      (add-hook 'after-load-theme-hook #'hydra-set-posframe-show-params t)))
)

;; @see https://github.com/abo-abo/hydra
;; color could: red, blue, amaranth, pink, teal


;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

(require-package 'centaur-tabs)
(use-package centaur-tabs
  :demand
  :init
  ;; Set the style to rounded with icons
  (setq centaur-tabs-style "bar")
  (setq centaur-tabs-set-icons t)
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward) ;; Ctrl PgUp
  ("C-<next>" . centaur-tabs-forward))  ;; Ctrl PgDn

;; ===================================
;; Theme 主题设置
;; -----------------------------------
;;(require 'lazycat-theme)
;;(lazycat-theme-load-dark)

(require-package 'doom-themes)
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  ;; 加载一个主题，DOOM One 是 DOOM Emacs 的默认主题，非常美观
  :init
  (load-theme 'doom-one t)
  )

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

;;;###autoload
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(require-package 'doom-modeline)
(use-package doom-modeline
  ;;  :load-path "~/.emacs.d/extensions/doom-modeline"
  :hook (after-init . doom-modeline-mode)
  :init
  ;;(doom-modeline-mode 1)
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

(require-package 'hide-mode-line)
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
(require-package 'minions)
(use-package minions
  :hook (doom-modeline-mode . minions-mode))
;; 切换buffer焦点时高亮动画
(require-package 'beacon)
(use-package beacon
  :ensure t
  :hook (after-init . beacon-mode))

(unless sys/win32p

  ;; Mouse & Smooth Scroll
  ;; Scroll one line at a time (less "jumpy" than defaults)
  (when (display-graphic-p)
    (setq mouse-wheel-scroll-amount '(1 ((shift) . hscroll))
          mouse-wheel-scroll-amount-horizontal 1
          mouse-wheel-progressive-speed nil))
  (setq scroll-step 1
        scroll-margin 0
        scroll-conservatively 100000
        auto-window-vscroll nil
        scroll-preserve-screen-position t)

  ;; Good pixel line scrolling
  (if (fboundp 'pixel-scroll-precision-mode)
      (pixel-scroll-precision-mode t)
    (unless sys/macp
      (use-package good-scroll
        :diminish
        :hook (after-init . good-scroll-mode)
        :bind (([remap next] . good-scroll-up-full-screen)
               ([remap prior] . good-scroll-down-full-screen)))))

  ;; Smooth scrolling over images
  (unless emacs/>=30p
    (use-package iscroll
      :diminish
      :hook (image-mode . iscroll-mode)))

  ;; Use fixed pitch where it's sensible
  (use-package mixed-pitch
    :diminish)
  )


;; GUI Environment
(when (display-graphic-p)
  ;; scroll-bar
  (set-scroll-bar-mode 'right)
  ;; 隐藏垂直滚动条。
  ;;(modify-all-frames-parameters '((vertical-scroll-bars)))

  ;; Easily adjust the font size in all frames
  (require-package 'default-text-scale)
  (use-package default-text-scale
	:ensure t
	:hook (after-init . default-text-scale-mode)
	:bind (:map default-text-scale-mode-map
	            ("s-="   . default-text-scale-increase)
	            ("s--"   . default-text-scale-decrease)
	            ("s-0"   . default-text-scale-reset)
	            ("C-s-=" . default-text-scale-increase)
	            ("C-s--" . default-text-scale-decrease)
	            ("C-s-0" . default-text-scale-reset)))

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

  (use-package vertico-posframe
	:ensure t
	:custom
	(vertico-posframe-parameters
	 '((left-fringe . 8)
	   (right-fringe . 8))
     ))
  ;; Child frame
  (when (childframe-workable-p)
	(use-package posframe
	  :hook (after-load-theme . posframe-delete-all)
	  :init
	  (defface posframe-border
	    `((t (:inherit region)))
	    "Face used by the `posframe' border."
	    :group 'posframe)
	  (defvar posframe-border-width 2
	    "Default posframe border width.")
	  :config
	  (with-no-warnings
	    (defun my-posframe--prettify-frame (&rest _)
	      (set-face-background 'fringe nil posframe--frame))
	    (advice-add #'posframe--create-posframe :after #'my-posframe--prettify-frame)

	    (defun posframe-poshandler-frame-center-near-bottom (info)
	      (cons (/ (- (plist-get info :parent-frame-width)
	                  (plist-get info :posframe-width))
	               2)
	            (/ (+ (plist-get info :parent-frame-height)
	                  (* 2 (plist-get info :font-height)))
	               2))))))

  (use-package vertico-posframe
	:ensure t
	:custom
	(vertico-posframe-parameters
	 '((left-fringe . 8)
	   (right-fringe . 8))
     ))
  ;; Ligatures support
  (when (and emacs/>=28p (not suk-prettify-symbols-alist))
	(use-package composite
	  :ensure nil
	  :init (defvar composition-ligature-table (make-char-table nil))
	  :hook (((prog-mode
	           conf-mode nxml-mode markdown-mode help-mode
	           shell-mode eshell-mode term-mode vterm-mode)
	          . (lambda () (setq-local composition-function-table composition-ligature-table))))
	  :config
	  ;; support ligatures, some toned down to prevent hang
	  (let ((alist
	         '((33  . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
	           (35  . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
	           (36  . ".\\(?:\\(>\\)>?\\)")
	           (37  . ".\\(?:\\(%\\)%?\\)")
	           (38  . ".\\(?:\\(&\\)&?\\)")
	           (42  . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
	           ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
	           (43  . ".\\(?:\\([>]\\)>?\\)")
	           ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
	           (45  . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
	           ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
	           (46  . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
	           (47  . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
	           ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
	           (48  . ".\\(?:x[a-zA-Z]\\)")
	           (58  . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
	           (59  . ".\\(?:\\(;\\);?\\)")
	           (60  . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
	           (61  . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
	           (62  . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
	           (63  . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
	           (91  . ".\\(?:\\(|\\)[]|]?\\)")
	           ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
	           (94  . ".\\(?:\\(=\\)=?\\)")
	           (95  . ".\\(?:\\(|_\\|[_]\\)_?\\)")
	           (119 . ".\\(?:\\(ww\\)w?\\)")
	           (123 . ".\\(?:\\(|\\)[|}]?\\)")
	           (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
	           (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
	    (dolist (char-regexp alist)
	      (set-char-table-range composition-ligature-table (car char-regexp)
	                            `([,(cdr char-regexp) 0 font-shape-gstring]))))
	  (set-char-table-parent composition-ligature-table composition-function-table)))

  ;; Frame transparence
  (require-package 'transwin)
  (use-package transwin
	:bind (("C-M-9" . transwin-inc)
	       ("C-M-8" . transwin-dec)
	       ("C-M-7" . transwin-toggle))
	:init
	(when sys/linux-x-p
	  (setq transwin-parameter-alpha 'alpha-background)))
  )

(require 'load-set-font)
