;;; init-ui.el --- Initialize UI configurations.	-*- lexical-binding: t -*-


(provide 'init-ui)

(eval-when-compile
  (require '+const)
  (require '+custom)
  (require '+func)
  (require 'init-package)
  )

;; Optimization
(setq idle-update-delay 1.0)


;; Compatibility
(use-package compat :demand t)


;; 字体
(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))


(display-time-mode -1)

(setq column-number-mode t)
(setq-default fill-column 80)
(setq column-number-mode t)

(require 'display-line-numbers)
;; 设置行号
;;(global-display-line-numbers-mode 1)
;; Alternatively, to use it only in programming modes:
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Line numbers are not displayed when large files are used.
(setq line-number-display-limit large-file-warning-threshold)
(setq line-number-display-limit-width 1000)

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

;; Easily adjust the font size in all frames
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


;; Display ugly ^L page breaks as tidy horizontal lines
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
      (add-hook 'after-load-theme-hook #'hydra-set-posframe-show-params t))))

;; -*- coding: utf-8; lexical-binding: t; -*-

;; @see https://github.com/abo-abo/hydra
;; color could: red, blue, amaranth, pink, teal

;; 最常用的功能
(defhydra my-hydra-launcher (:color blue)
  "
^Misc^                    ^Study^                    ^Emms^
-------------------------------------------------------------------
[_ss_] Save workgroup     [_vv_] Pronounce word      [_R_] Random
[_ll_] Load workgroup     [_W_] Big word list        [_n_] Next
[_B_] New bookmark        [_vi_] Play word's video   [_p_] Previous
[_m_] Goto bookmark       [_im_] Image of word       [_P_] Pause
[_bb_] Switch Gnus buffer [_w_] Select big word      [_S_] Stop
[_e_] Erase buffer        [_s1_] Pomodoro tiny task  [_O_] Open
[_r_] Erase this buffer   [_s2_] Pomodoro big task   [_L_] Playlist
[_f_] Recent file         [_st_] Pomodoro stop       [_K_] Search
[_d_] Recent directory    [_sr_] Pomodoro resume     [_F_] filter
[_z_] Jump around (z.sh)  [_sp_] Pomodoro pause      [_E_] replay
[_bh_] Bash history       [_as_] Ascii table
[_hh_] Favorite theme     [_T_] Typewriter on/off
[_hr_] Random theme       [_V_] Old typewriter
[_ka_] Kill other buffers
[_ii_] Imenu
[_id_] Insert date string
[_aa_] Adjust subtitle
[_q_] Quit
"
  ("aa" my-srt-offset-subtitles-from-point)
  ("B" my-bookmark-set)
  ("m" my-bookmark-goto)
  ("f" my-counsel-recentf)
  ("d" my-recent-directory)
  ("bh" my-insert-bash-history)
  ("hh" my-random-favorite-color-theme)
  ("hr" my-random-healthy-color-theme)
  ("ii" my-counsel-imenu)
  ("ka" my-kill-all-but-current-buffer)
  ("id" my-insert-date)
  ("as" my-ascii-table)
  ("ss" wg-create-workgroup)
  ("ll" wg-open-workgroup)
  ("e" shellcop-erase-buffer)
  ("r" shellcop-reset-with-new-command)
  ("z" shellcop-jump-around)
  ("T" my-toggle-typewriter)
  ("V" twm/toggle-sound-style)

  ;; {{pomodoro
  ("s1" (pomodoro-start 15))
  ("s2" (pomodoro-start 60))
  ("st" pomodoro-stop)
  ("sr" pomodoro-resume)
  ("sp" pomodoro-pause)
  ;; }}

  ;; {{emms
  ("R" (progn (emms-shuffle) (emms-random)))
  ("F" my-emms-playlist-filter)
  ("K" my-emms-playlist-random-track)
  ("E" (emms-seek-to 0))
  ("p" emms-previous)
  ("P" emms-pause)
  ("S" emms-stop)
  ("O" emms-play-playlist)
  ("n" emms-next)
  ("L" emms-playlist-mode-go)
  ;; }}

  ("vv" mybigword-pronounce-word)
  ("w" mybigword-big-words-in-current-window)
  ("im" mybigword-show-image-of-word)
  ("W" my-lookup-bigword-definition-in-buffer)
  ("vi" mybigword-play-video-of-word-at-point)
  ("bb" dianyou-switch-gnus-buffer)
  ("q" nil :color red))

;; Because in message-mode/article-mode we've already use `y' as hotkey
(global-set-key (kbd "C-c C-y") 'my-hydra-launcher/body)
(defun org-mode-hook-hydra-setup ()
  (local-set-key (kbd "C-c C-y") 'my-hydra-launcher/body))
(add-hook 'org-mode-hook 'org-mode-hook-hydra-setup)





;; ;; increase and decrease font size in GUI emacs
;; ;; @see https://oremacs.com/download/london.pdf
;; (when (display-graphic-p)
;;   ;; Since we already use GUI Emacs, f2 is definitely available
;;   (defhydra my-hydra-zoom (global-map "<f2>")
;;     "Zoom"
;;     ("g" text-scale-increase "in")
;;     ("l" text-scale-decrease "out")
;;     ("r" (text-scale-set 0) "reset")
;;     ("q" nil "quit")))


(defhydra my-hydra-ebook ()
  "
[_v_] Pronounce word
[_;_] Jump to word
[_w_] Display bigword in current window
"
  ("v" mybigword-pronounce-word)
  (";" avy-goto-char-2)
  ("w" mybigword-big-words-in-current-window)
  ("q" nil))



(use-package pretty-hydra
  :custom (pretty-hydra-default-title-body-format-spec " %s%s")
  :bind ("<f6>" . toggles-hydra/body)
  :hook (emacs-lisp-mode . (lambda ()
                             (add-to-list
                              'imenu-generic-expression
                              '("Hydras"
                                "^.*(\\(pretty-hydra-define\\) \\([a-zA-Z-]+\\)"
                                2))))
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:inherit highlight :reverse-video t)))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and (icons-displayable-p) icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))

  ;; Global toggles
  (with-no-warnings
    (pretty-hydra-define+ toggles-hydra (:title (pretty-hydra-title "Toggles" 'faicon "nf-fa-toggle_on")
                                        :color amaranth :quit-key ("q" "C-g"))
      ("Basic"
       (("n" (cond ((fboundp 'display-line-numbers-mode)
                    (display-line-numbers-mode (if display-line-numbers-mode -1 1)))
                   ((fboundp 'gblobal-linum-mode)
                    (global-linum-mode (if global-linum-mode -1 1))))
         "line number"
         :toggle (or (bound-and-true-p display-line-numbers-mode)
                     (bound-and-true-p global-linum-mode)))
        ("i" global-aggressive-indent-mode "aggressive indent" :toggle t)
        ("d" global-hungry-delete-mode "hungry delete" :toggle t)
        ("e" electric-pair-mode "electric pair" :toggle t)
        ("c" flyspell-mode "spell check" :toggle t)
        ("s" prettify-symbols-mode "pretty symbol" :toggle t)
        ("l" global-page-break-lines-mode "page break lines" :toggle t)
        ("B" display-battery-mode "battery" :toggle t)
        ("T" display-time-mode "time" :toggle t)
        ("a" abbrev-mode "abrev" :toggle t)
        ("F" auto-fill-mode "auto fill" :toggle t)
        ("m" doom-modeline-mode "modern mode-line" :toggle t)
        ("t" toggle-truncate-lines "truncate lines" :toggle t)
        ("u" toggle-company-ispell "Company Ispell" :toggle t))
       "Highlight"
       (("h l" global-hl-line-mode "line" :toggle t)
        ("h p" show-paren-mode "paren" :toggle t)
        ("h s" symbol-overlay-mode "symbol" :toggle t)
        ("h r" rainbow-mode "rainbow" :toggle t)
        ("h w" (setq-default show-trailing-whitespace (not show-trailing-whitespace))
         "whitespace" :toggle show-trailing-whitespace)
        ("h d" rainbow-delimiters-mode "delimiter" :toggle t)
        ("h i" highlight-indent-guides-mode "indent" :toggle t)
        ("h t" global-hl-todo-mode "todo" :toggle t))
       "Program"
       (("f" flymake-mode "flymake" :toggle t)
        ("O" hs-minor-mode "hideshow" :toggle t)
        ("U" subword-mode "subword" :toggle t)
        ("w" whitespace-mode "whitespace" :toggle t)
        ("W" which-function-mode "which function" :toggle t)
        ("E" toggle-debug-on-error "debug on error" :toggle (default-value 'debug-on-error))
        ("Q" toggle-debug-on-quit "debug on quit" :toggle (default-value 'debug-on-quit))
        ("v" global-diff-hl-mode "gutter" :toggle t)
        ("V" diff-hql-flydiff-mode "live gutter" :toggle t)
        ("M" diff-hl-margin-mode "margin gutter" :toggle t)
        ("D" diff-hl-dired-mode "dired gutter" :toggle t))
       ))))

(use-package hydra-posframe
  :load-path "~/.emacs.d/extensions/hydra-posframe/hydra-posframe.el"
  :defer 1
  :hook (after-init . hydra-posframe-mode))

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


;; Don't use GTK+ tooltip
(when (boundp 'x-gtk-use-system-tooltips)
  (setq x-gtk-use-system-tooltips nil))

(use-package vertico-posframe
  :ensure t
  :custom
  (vertico-posframe-parameters
   '((left-fringe . 8)
     (right-fringe . 8))))

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

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  ;; 加载一个主题，DOOM One 是 DOOM Emacs 的默认主题，非常美观
  :init
  (load-theme 'doom-one t)
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; WORKAROUND: Visual bell on 29+
  ;; @see https://github.com/doomemacs/themes/issues/733
  (with-no-warnings
    (defun my-doom-themes-visual-bell-fn ()
      "Blink the mode-line red briefly. Set `ring-bell-function' to this to use it."
      (let ((buf (current-buffer))
            (cookies (mapcar (lambda (face)
                               (face-remap-add-relative face 'doom-themes-visual-bell))
                             (if (facep 'mode-line-active)
                                 '(mode-line-active solaire-mode-line-active-face)
                               '(mode-line solaire-mode-line-face)))))
        (force-mode-line-update)
        (run-with-timer 0.15 nil
                        (lambda ()
                          (with-current-buffer buf
                            (mapc #'face-remap-remove-relative cookies)
                            (force-mode-line-update))))))
    (advice-add #'doom-themes-visual-bell-fn :override #'my-doom-themes-visual-bell-fn))
  )

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

;;;###autoload
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

(use-package doom-modeline
;;  :load-path "~/.emacs.d/extensions/doom-modeline"
  :hook (after-init . doom-modeline-mode)
  :init
  ;;(doom-modeline-mode 1)
  (setq doom-modeline-icon suk-icon
        doom-modeline-minor-modes t)
  :bind (:map doom-modeline-mode-map
         ("C-<f6>" . doom-modeline-hydra/body))
  :config
  (column-number-mode 1)
  :custom
  (doom-modeline-height 30)
  (doom-modeline-window-width-limit nil)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  ;;(doom-modeline-major-mode-icon nil)
  ;;(doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  ;;(doom-modeline-buffer-encoding nil)
  (doom-modeline-buffer-modification-icon t)
  ;;(doom-modeline-env-python-executeable "python")
  ;; needs display-time-mode to be one
  (doom-modeline-time t)
  (doom-modeline-vcs-max-leghth 50)
  ;; Windows下记得加上
  (if sys/win32p (setq inhibit-compacting-font-caches t))
  :pretty-hydra
  ((:title (pretty-hydra-title "Mode Line" 'sucicon "nf-custom-emacs" :face 'nerd-icons-purple)
    :color amaranth
    :quit-key ("q" "C-g"))
   ("Icon"
    (("i" (setq doom-modeline-icon (not doom-modeline-icon))
      "display icons" :toggle doom-modeline-icon)
     ("u" (setq doom-modeline-unicode-fallback (not doom-modeline-unicode-fallback))
      "unicode fallback" :toggle doom-modeline-unicode-fallback)
     ("m" (setq doom-modeline-major-mode-icon (not doom-modeline-major-mode-icon))
      "major mode" :toggle doom-modeline-major-mode-icon)
     ("c" (setq doom-modeline-major-mode-color-icon (not doom-modeline-major-mode-color-icon))
      "colorful major mode" :toggle doom-modeline-major-mode-color-icon)
     ("s" (setq doom-modeline-buffer-state-icon (not doom-modeline-buffer-state-icon))
      "buffer state" :toggle doom-modeline-buffer-state-icon)
     ("o" (setq doom-modeline-buffer-modification-icon (not doom-modeline-buffer-modification-icon))
      "modification" :toggle doom-modeline-buffer-modification-icon)
     ("x" (setq doom-modeline-time-icon (not doom-modeline-time-icon))
      "time" :toggle doom-modeline-time-icon)
     ("v" (setq doom-modeline-modal-icon (not doom-modeline-modal-icon))
      "modal" :toggle doom-modeline-modal-icon))
    "Segment"
    (("g h" (setq doom-modeline-hud (not doom-modeline-hud))
      "hud" :toggle doom-modeline-hud)
     ("g m" (setq doom-modeline-minor-modes (not doom-modeline-minor-modes))
      "minor modes" :toggle doom-modeline-minor-modes)
     ("g w" (setq doom-modeline-enable-word-count (not doom-modeline-enable-word-count))
      "word count" :toggle doom-modeline-enable-word-count)
     ("g e" (setq doom-modeline-buffer-encoding (not doom-modeline-buffer-encoding))
      "encoding" :toggle doom-modeline-buffer-encoding)
     ("g i" (setq doom-modeline-indent-info (not doom-modeline-indent-info))
      "indent" :toggle doom-modeline-indent-info)
     ("g c" (setq doom-modeline-display-misc-in-all-mode-lines (not doom-modeline-display-misc-in-all-mode-lines))
      "misc info" :toggle doom-modeline-display-misc-in-all-mode-lines)
     ("g l" (setq doom-modeline-lsp (not doom-modeline-lsp))
      "lsp" :toggle doom-modeline-lsp)
     ("g k" (setq doom-modeline-workspace-name (not doom-modeline-workspace-name))
      "workspace" :toggle doom-modeline-workspace-name)
     ("g g" (setq doom-modeline-github (not doom-modeline-github))
      "github" :toggle doom-modeline-github)
     ("g n" (setq doom-modeline-gnus (not doom-modeline-gnus))
      "gnus" :toggle doom-modeline-gnus)
     ("g u" (setq doom-modeline-mu4e (not doom-modeline-mu4e))
      "mu4e" :toggle doom-modeline-mu4e)
     ("g r" (setq doom-modeline-irc (not doom-modeline-irc))
      "irc" :toggle doom-modeline-irc)
     ("g f" (setq doom-modeline-irc-buffers (not doom-modeline-irc-buffers))
      "irc buffers" :toggle doom-modeline-irc-buffers)
     ("g s" (progn
              (setq doom-modeline-check-simple-format (not doom-modeline-check-simple-format))
              (and (bound-and-true-p flycheck-mode) (flycheck-buffer)))
      "simple check format" :toggle doom-modeline-check-simple-format)
     ("g t" (setq doom-modeline-time (not doom-modeline-time))
      "time" :toggle doom-modeline-time)
     ("g v" (setq doom-modeline-env-version (not doom-modeline-env-version))
      "version" :toggle doom-modeline-env-version))
    "Style"
    (("a" (setq doom-modeline-buffer-file-name-style 'auto)
      "auto"
      :toggle (eq doom-modeline-buffer-file-name-style 'auto))
     ("b" (setq doom-modeline-buffer-file-name-style 'buffer-name)
      "buffer name"
      :toggle (eq doom-modeline-buffer-file-name-style 'buffer-name))
     ("f" (setq doom-modeline-buffer-file-name-style 'file-name)
      "file name"
      :toggle (eq doom-modeline-buffer-file-name-style 'file-name))
     ("t u" (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
      "truncate upto project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-project))
     ("t f" (setq doom-modeline-buffer-file-name-style 'truncate-from-project)
      "truncate from project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-from-project))
     ("t w" (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
      "truncate with project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-with-project))
     ("t e" (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
      "truncate except project"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-except-project))
     ("t r" (setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
      "truncate upto root"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-root))
     ("t a" (setq doom-modeline-buffer-file-name-style 'truncate-all)
      "truncate all"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-all))
     ("t n" (setq doom-modeline-buffer-file-name-style 'truncate-nil)
      "truncate none"
      :toggle (eq doom-modeline-buffer-file-name-style 'truncate-nil))
     ("r f" (setq doom-modeline-buffer-file-name-style 'relative-from-project)
      "relative from project"
      :toggle (eq doom-modeline-buffer-file-name-style 'relative-from-project))
     ("r t" (setq doom-modeline-buffer-file-name-style 'relative-to-project)
      "relative to project"
      :toggle (eq doom-modeline-buffer-file-name-style 'relative-to-project)))
    "Project Detection"
    (("p a" (setq doom-modeline-project-detection 'auto)
      "auto"
      :toggle (eq doom-modeline-project-detection 'auto))
     ("p f" (setq doom-modeline-project-detection 'ffip)
      "ffip"
      :toggle (eq doom-modeline-project-detection 'ffip))
     ("p i" (setq doom-modeline-project-detection 'projectile)
      "projectile"
      :toggle (eq doom-modeline-project-detection 'projectile))
     ("p p" (setq doom-modeline-project-detection 'project)
      "project"
      :toggle (eq doom-modeline-project-detection 'project))
     ("p n" (setq doom-modeline-project-detection nil)
      "disable"
      :toggle (eq doom-modeline-project-detection nil)))
    "Misc"
    (("n" (progn
            (message "Fetching GitHub notifications...")
            (run-with-timer 300 nil #'doom-modeline--github-fetch-notifications)
            (browse-url "https://github.com/notifications"))
      "github notifications" :exit t)
     ("e" (cond ((bound-and-true-p flycheck-mode)
                 (flycheck-list-errors))
                ((bound-and-true-p flymake-mode)
                 (flymake-show-diagnostics-buffer)))
      "list errors" :exit t)
     ("w" (if (bound-and-true-p grip-mode)
              (grip-browse-preview)
            (message "Not in preview"))
      "browse preview" :exit t)
     ("z h" (read-from-minibuffer
             "Eval: "
             (format "(setq %s %s)"
                     'doom-modeline-height
                     (symbol-value 'doom-modeline-height)))
      "set height" :exit t)
     ("z w" (read-from-minibuffer
             "Eval: "
             (format "(setq %s %s)"
                     'doom-modeline-bar-width
                     (symbol-value 'doom-modeline-bar-width)))
      "set bar width" :exit t)
     ("z g" (read-from-minibuffer
             "Eval: "
             (format "(setq %s %s)"
                     'doom-modeline-github-interval
                     (symbol-value 'doom-modeline-github-interval)))
      "set github interval" :exit t)
     ("z n" (read-from-minibuffer
             "Eval: "
             (format "(setq %s %s)"
                     'doom-modeline-gnus-timer
                     (symbol-value 'doom-modeline-gnus-timer)))
      "set gnus interval" :exit t))))
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

;; Frame transparence
(use-package transwin
  :bind (("C-M-9" . transwin-inc)
         ("C-M-8" . transwin-dec)
         ("C-M-7" . transwin-toggle))
  :init
  (when sys/linux-x-p
    (setq transwin-parameter-alpha 'alpha-background)))




;;===================================================
;; Frame
;;===================================================
;; Title
(setq frame-title-format
      '("Suk's Emacs - "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b")))
      icon-title-format frame-title-format
      )

(defvar suk/frame--geometry nil)
;;;###autoload
(defun suk/frame--save-geometry ()
  "Save current frame's geometry."
  (setq suk/frame--geometry
        `((left   . ,(frame-parameter nil 'left))
          (top    . ,(frame-parameter nil 'top))
          (width  . ,(frame-parameter nil 'width))
          (height . ,(frame-parameter nil 'height))
          (fullscreen))))

;;;###autoload
(defun suk/frame--fullscreen-p ()
  "Return Non-nil if the frame is fullscreen."
  (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth)))

;;;###autoload
(defun suk/frame-maximize ()
  "Maximize the frame."
  (interactive)
  (suk/frame--save-geometry)
  (unless (eq (frame-parameter nil 'fullscreen) 'maximized)
    (set-frame-parameter nil 'fullscreen 'maximized)))

;;;###autoload
(defun suk/frame-restore ()
  "Restore the frame's size and position."
  (interactive)
  (modify-frame-parameters nil suk/frame--geometry))

;;;###autoload
(defun suk/frame-left-half ()
  "Put the frame to the left-half."
  (interactive)
  (unless (suk/frame--fullscreen-p)
    (suk/frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

;;;###autoload
(defun suk/frame-right-half ()
  "Put the frame to the right-half."
  (interactive)
  (unless (suk/frame--fullscreen-p)
    (suk/frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (/ (nth 2 attr) 2) 20))
           (height (- (nth 3 attr) 30))
           (left (+ (nth 0 attr) width 20))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

;;;###autoload
(defun suk/frame-top-half ()
  "Put the frame to the top-half."
  (interactive)
  (unless (suk/frame--fullscreen-p)
    (suk/frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (nth 1 attr)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))

;;;###autoload
(defun suk/frame-bottom-half ()
  "Put the frame to the bottom-half."
  (interactive)
  (unless (suk/frame--fullscreen-p)
    (suk/frame--save-geometry)
    (let* ((attr (frame-monitor-workarea))
           (width (- (nth 2 attr) 20))
           (height (- (/ (nth 3 attr) 2) 30))
           (left (nth 0 attr))
           (top (+ (nth 1 attr) height 30)))
      (set-frame-parameter nil 'fullscreen nil)
      (set-frame-position nil left top)
      (set-frame-size nil width height t))))



(suk-set-key-bindings 'global-set-key
                      (list
                       (list (kbd "C-M-<return>")     #'suk/frame-maximize)
                       (list (kbd "C-M-<backspace>")  #'suk/frame-restore)
                       (list (kbd "C-M-<left>")       #'suk/frame-left-half)
                       (list (kbd "C-M-<right>")      #'suk/frame-right-half)
                       (list (kbd "C-M-<up>")         #'suk/frame-top-half)
                       (list (kbd "C-M-<down>")       #'suk/frame-bottom-half)
                       ))


;; 调节屏幕亮度
;;;###autoload
(defun suk/set-backlight (&optional light-value)
  (interactive "s请输入亮度(小数表示的百分比): ")
  (let ((max-backlight (string-to-number (string-trim-right
                                          (shell-command-to-string
                                           "cat /sys/class/backlight/intel_backlight/max_brightness")))))
    (when (and light-value (floatp (string-to-number light-value)))
      (shell-command
       (concat "echo "
               (format "%d" (* max-backlight (string-to-number light-value)))
               " > /sys/class/backlight/intel_backlight/brightness")))))

;; 增加10%屏幕亮度
;;;###autoload
(defun suk/plus-backlight ()
  (interactive)
  (let* (
         ;; 最大亮度
         (max-backlight (string-to-number (string-trim-right
                                           (shell-command-to-string "cat /sys/class/backlight/intel_backlight/max_brightness"))))
         ;; 当前亮度
         (current-backlight (string-to-number (string-trim-right
                                               (shell-command-to-string "cat /sys/class/backlight/intel_backlight/brightness"))))
         ;; 增加后的亮度
         (add-backlight (+ current-backlight (* max-backlight 0.1))))
    (if (< add-backlight max-backlight)
        (progn (shell-command
                (concat "echo "
                        (format "%d" add-backlight)
                        " > /sys/class/backlight/intel_backlight/brightness"))
               (message "亮度+10%"))
      (message "亮度MAX!!"))))

;; 减少屏幕亮度
;;;###autoload
(defun suk/less-backlight ()
  (interactive)
  (let* (
         ;; 最大亮度
         (max-backlight (string-to-number (string-trim-right
                                           (shell-command-to-string "cat /sys/class/backlight/intel_backlight/max_brightness"))))
         ;; 当前亮度
         (current-backlight (string-to-number (string-trim-right
                                               (shell-command-to-string "cat /sys/class/backlight/intel_backlight/brightness"))))
         ;; 减少后的亮度
         (less-backlight (- current-backlight (* max-backlight 0.1))))
    (if (> less-backlight (* max-backlight 0.1) )
        (progn (shell-command
                (concat "echo "
                        (format "%d" less-backlight)
                        " > /sys/class/backlight/intel_backlight/brightness"))
               (message "亮度-10%"))
      (message "亮度Min!!"))))



(require 'load-set-font)

(unless sys/win32p
  ;; 切换buffer焦点时高亮动画
  (use-package beacon
    :ensure t
    :hook (after-init . beacon-mode))
  )


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
