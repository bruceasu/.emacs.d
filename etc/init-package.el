;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

(provide 'init-package)

(eval-when-compile
  (require '+const)
  (require '+func)
  (require '+custom))

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

;; (setq package-check-signature nil) ; 个别时候会出现签名校验失败

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
(setq use-package-always-ensure nil
      use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)

(require 'use-package)

;; Persistent the scratch buffer
(use-package persistent-scratch
  :diminish
  :bind (:map persistent-scratch-mode-map
			  ([remap kill-buffer] . (lambda (&rest _)
									   (interactive)
									   (user-error "Scratch buffer cannot be killed")))
			  ([remap revert-buffer] . persistent-scratch-restore)
			  ([remap revert-this-buffer] . persistent-scratch-restore))
  :hook ((after-init . persistent-scratch-autosave-mode)
         (lisp-interaction-mode . persistent-scratch-mode))
  :init (setq persistent-scratch-backup-file-name-format "%Y-%m-%d"
			  persistent-scratch-backup-directory
			  (expand-file-name "var/persistent-scratch" user-emacs-directory)))

;; Misc

(unless sys/win32p
  (use-package daemons)                 ; system services/daemons
  )


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

(require-package 'async)
(require-package 'amx)
(require-package 'popup) ; some old package need it
(require-package 'auto-yasnippet)
(require-package 'csv-mode)
;;(require-package 'expand-region) ; I prefer stable version
;;(require-package 'fringe-helper)
(require-package 'wgrep)
(require-package 'request)
;;(require-package 'lua-mode)
(require-package 'yaml-mode)
;;(require-package 'paredit)
;;(require-package 'findr)
(require-package 'diredfl) ; font lock for `dired-mode'
(require-package 'pinyinlib)
(require-package 'find-by-pinyin-dired)
(require-package 'jump)
;;(require-package 'nvm)
;;(require-package 'writeroom-mode)
;;(require-package 'haml-mode)
(require-package 'markdown-mode)
(unless emacs/>=28p
  (require-package 'link)
  (require-package 'connection)
  ;; dictionary requires 'link and 'connection
  (require-package 'dictionary))
(require-package 'htmlize) ; prefer stable version
;;(require-package 'jade-mode)
;;(require-package 'diminish)
(require-package 'scratch)
(require-package 'rainbow-delimiters)
(require-package 'textile-mode)
(require-package 'git-timemachine)
(require-package 'exec-path-from-shell)
(require-package 'ivy)
(require-package 'swiper)
(require-package 'counsel) ; counsel => swiper => ivy
(require-package 'find-file-in-project)
(require-package 'counsel-bbdb)
(require-package 'command-log-mode)
(require-package 'regex-tool)
;;(require-package 'groovy-mode)
;;(require-package 'emmet-mode)
;;(require-package 'winum)
;;(require-package 'session)
;;(require-package 'unfill)
(require-package 'counsel-gtags)
;;(require-package 'eww-lnum)
(require-package 'buffer-move)
(require-package 'ace-window)
;;(require-package 'cmake-mode)
;;(require-package 'cpputils-cmake)
;;(require-package 'bbdb)
(require-package 'pomodoro)
;; rvm-open-gem to get gem's code
;;(require-package 'rvm)
;; C-x r l to list bookmarks
(require-package 'js-doc)
(require-package 'js2-mode)
(require-package 'rjsx-mode)
(require-package 'tagedit)
(require-package 'cliphist)
(require-package 'yasnippet)
;;(require-package 'yasnippet-snippets)
(require-package 'company)
(require-package 'native-complete)
(require-package 'company-native-complete)
(require-package 'company-c-headers)
(require-package 'company-statistics)
(require-package 'lsp-mode)
;;(require-package 'elpy)
;;(require-package 'legalese)
;;(require-package 'simple-httpd)
(require-package 'git-gutter) ; use my patched version
(require-package 'neotree)
(require-package 'hydra)
(require-package 'ivy-hydra) ; @see https://oremacs.com/2015/07/23/ivy-multiaction/
(require-package 'web-mode)
;;(require-package 'iedit)
;;(require-package 'websocket) ; for debug debugging of browsers
;; (require-package 'evil)
;; (require-package 'evil-escape)
;; (require-package 'evil-exchange)
;; (require-package 'evil-find-char-pinyin)
;; (require-package 'evil-mark-replace)
;; (require-package 'evil-matchit)
;; (require-package 'evil-nerd-commenter)
;; (require-package 'evil-surround)
;; (require-package 'evil-visualstar)
(require-package 'counsel-css)
(require-package 'auto-package-update)
;;(require-package 'keyfreq)
;;(require-package 'adoc-mode) ; asciidoc files
(require-package 'shackle)
(require-package 'toc-org)
;;(require-package 'elpa-mirror)
;; {{ @see https://pawelbx.github.io/emacs-theme-gallery/
;;(require-package 'color-theme)
;;(require-package 'visual-regexp) ;; Press "M-x vr-*"
;;(require-package 'vimrc-mode)
(require-package 'nov) ; read epub
;;(require-package 'rust-mode)
;; (require-package 'langtool) ; my own patched version is better
(require-package 'typescript-mode)
;; run "M-x pdf-tool-install" at debian and open pdf in GUI Emacs
(require-package 'pdf-tools)
;;(require-package 'pyim)
;;(require-package 'pyim-wbdict) ; someone may use wubi IME, not me
;;(require-package 'pyim-basedict)
(require-package 'hungry-delete)
(require-package 'esup)

;; {{ Fixed expiring GNU ELPA keys
;; GNU ELPA GPG key will expire on Sep-2019. So we need install this package to
;; update key or else users can't install packages from GNU ELPA.
;; @see https://www.reddit.com/r/emacs/comments/bn6k1y/updating_gnu_elpa_keys/
;; BTW, this setup uses MELPA only. So GNU ELPA GPG key is not used.
;; (require-package 'gnu-elpa-keyring-update)
;; }}

;; org => ppt
;;(require-package 'org-re-reveal)

(require-package 'git-modes)
(require-package 'ace-pinyin)
(require-package 'which-key)
(require-package 'highlight-symbol)
(require-package 'wc-mode)
(require-package 'helpful)
(require-package 'inf-ruby)
(require-package 'erlang)
;;(require-package 'qrencode)
;;(require-package 'ws-butler)
;;(require-package 'sage-shell-mode)
;;(require-package 'graphql-mode)
;;(require-package 'ob-sagemath)
;;(require-package 'pulseaudio-control)
(when sys/linuxp
  (require-package 'vterm))

;; magit sometime use packages which not released yet
;; so we place it at the end to make sure other packages are installed first
(require-package 'magit)

;; edit
(require-package 'olivetti)
(require-package 'goto-chg) ; Goto last change
(require-package 'subword) ;Handling capitalized subwords in a nomenclature
(require-package 'anzu)
(require-package 'avy)
(require-package 'avy-zap)
(require-package 'sudo-edit)
(require-package 'flyspell)
(require-package 'beginend)
(require-package 'autorevert)

(unless emacs/>=28p
  (require-package 'undo-tree))

;; {{ trivial packages which has extra dependency
;;(require-package 'emms)
;; }}

;; kill buffer without my confirmation
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
