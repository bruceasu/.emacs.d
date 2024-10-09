;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

(provide 'init-package)

(eval-when-compile
  (require '+const)
  (require '+fn)
  (require '+custom))
;; 指定ELPA目录
(setq package-user-dir suk-emacs-elpa-dir)
(add-subdirs-to-load-path package-user-dir t)

;; HACK: DO NOT copy package-selected-packages to init/custom file forcibly.
;; https://github.com/jwiegley/use-package/issues/383#issuecomment-247801751
(defun my-save-selected-packages (&optional value)
  "Set `package-selected-packages' to VALUE but don't save to `custom-file'."
  (when value
    (setq package-selected-packages value)))

(advice-add 'package--save-selected-packages :override #'my-save-selected-packages)

(require 'package)
;; gnu：
;; http://elpa.gnu.org/packages/
;; https://elpa.emacs-china.org/gnu/ http://1.15.88.122/gnu/
;; https://mirrors.163.com/elpa/gnu/
;; https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/
;; melpa:
;; http://melpa.org/packages/
;; https://www.mirrorservice.org/sites/melpa.org/packages/
;; https://elpa.emacs-china.org/melpa/ http://1.15.88.122/melpa/
;; https://mirrors.163.com/elpa/melpa/
;; https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/

;;(setq package-archives '(("melpa" . "http://melpa.org/packages/")
;;                         ("gnu" . "http://elpa.gnu.org/packages/")
;;                         ("nongnu" . "https://elpa.nongnu.org/nongnu/"))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))

;; Un-comment below line if you follow "Install stable version in easiest way"
;; (setq package-archives '(("myelpa" . "~/myelpa/")))

;; my local repository is always needed.
;; (push (cons "localelpa" (expand-file-name  "localelpa/" suk-emacs-root-dir)) package-archives)

(setq package-check-signature nil) ; 个别时候会出现签名校验失败

;; Initialize packages
;; (unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
;;   (setq package-enable-at-startup nil)          ; To prevent initializing twice
;;   (package-initialize))

(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
;; make use-package default behavior better
;; with `use-package-always-ensure' you won't need ":ensure t" all the time
;; with `use-package-always-defer' you won't need ":defer t" all the time
(setq use-package-always-ensure t
      use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)

(require 'use-package)

;; Persistent the scratch buffer
;; (use-package persistent-scratch
;;  :diminish
;;  :bind (:map persistent-scratch-mode-map
;;			  ([remap kill-buffer] . (lambda (&rest _)
;;									   (interactive)
;;									   (user-error "Scratch buffer cannot be killed")))
;;			  ([remap revert-buffer] . persistent-scratch-restore)
;;			  ([remap revert-this-buffer] . persistent-scratch-restore))
;;  :hook ((after-init . persistent-scratch-autosave-mode)iu
;;         (lisp-interaction-mode . persistent-scratch-mode))
;;  :init
;;  ;; 创建 var 文件夹
;;  (make-directory (expand-file-name "var" user-emacs-directory) t)
;;
;; (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
;;        persistent-scratch-backup-directory (expand-file-name "var/persistent-scratch" user-emacs-directory)
;;        persistent-scratch-save-file (expand-file-name "var/.persistent-scratch" user-emacs-directory))
;;  (persistent-scratch-setup-default)
;;
;;  )

;; Misc

;;(unless sys/win32p
;;  (use-package daemons)                 ; system services/daemons
;;  )

;; On-demand installation of packages
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE with MIN-VERSION.
If NO-REFRESH is nil, `package-refresh-contents' is called."
  (my-ensure 'package)
  (unless (package-installed-p package min-version)
    (unless (or (assoc package package-archive-contents) no-refresh)
      (message "Missing package: %s" package)
      (package-refresh-contents))
    (package-install package)))

;;------------------------------------------------------------------------------
;; Fire up package.el and ensure the following packages are installed.
;;------------------------------------------------------------------------------

(when (display-graphic-p)
  ;; only graphic packages
  (require-package 'pretty-hydra)
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


  (require-package 'fringe-helper)
  ;; (require-package 'git-gutter) ; use my patched version, dependent to fringe-helper
  ;;(require-package 'git-modes)
  (require-package 'writeroom-mode)
  (require-package 'diredfl) ; font lock for `dired-mode'
  (when sys/linuxp
    (require-package 'vterm))
)
(unless (display-graphic-p)
  ;; only conole packages
)

;; completion
(require-package 'yasnippet)
(require-package 'yasnippet-snippets)
;;(require-package 'company)
;;(require-package 'native-complete)
;;(require-package 'company-native-complete)
;;(require-package 'company-c-headers)
;;(require-package 'company-statistics)


;; C-x r l to list bookmarks

;; UI
;;(require-package 'neotree)
(require-package 'hydra)
(require-package 'ivy-hydra) ; @see https://oremacs.com/2015/07/23/ivy-multiaction/
(require-package 'shackle)


;; windows
;;(require-package 'winum)
(require-package 'ace-window)

;; vi like key binds
;; (require-package 'evil)
;; (require-package 'evil-escape)
;; (require-package 'evil-exchange)
;; (require-package 'evil-find-char-pinyin)
;; (require-package 'evil-mark-replace)
;; (require-package 'evil-matchit)
;; (require-package 'evil-nerd-commenter)
;; (require-package 'evil-surround)
;; (require-package 'evil-visualstar)

;; viwer
(when sys/linuxp
	;; run "M-x pdf-tool-install" at debian and open pdf in GUI Emacs
	;;(require-package 'pdf-tools) ;; use the package in extension
	(require-package 'nov) ; read epub
)

;; kill buffer without my confirmation
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
