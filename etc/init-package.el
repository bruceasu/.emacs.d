;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

(provide 'init-package)

(eval-when-compile
  (require '+const)
  (require '+custom)
  (require '+fn)
)
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
(unless (bound-and-true-p package--initialized)
  (package-initialize))

(unless package-archive-contents
  (package-refresh-contents))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  ;; Should set before loading `use-package'
  ;; make use-package default behavior better
  ;; with `use-package-always-ensure' you won't need ":ensure t" all the time
  ;; with `use-package-always-defer' you won't need ":defer t" all the time
  (setq use-package-always-ensure nil
      use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-expand-minimally t)

  (require 'use-package)
)

;; Misc
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
(require-package 'hydra)
(require-package 'pretty-hydra)
(require-package 'ivy-hydra) ; @see https://oremacs.com/2015/07/23/ivy-multiaction/
(require-package 'centaur-tabs)
(require-package 'doom-modeline)
(require-package 'hide-mode-line)
;;(require-package 'all-the-icons) ;; submodule
(require-package 'nerd-icons-completion)
(require-package 'nerd-icons-dired)

(require-package 'async)
(require-package 'request)
;;(require-package 'websocket) ; for debug debugging of browsers

;;Enhance M-x, use counsel-M-x
;;(require-package 'amx)
;;(require-package 'ivy)
;;(require-package 'swiper)
(require-package 'counsel) ; counsel => swiper => ivy
(require-package 'counsel-bbdb)
(require-package 'counsel-gtags)
(require-package 'counsel-css)

(require-package 'jump)

;; modes
;;(require-package 'lsp-mode)
(require-package 'graphql-mode)
(require-package 'markdown-mode)
(require-package 'yaml-mode)
(require-package 'command-log-mode)
(require-package 'groovy-mode)

(require-package 'js2-mode)
;; (require-package 'rjsx-mode) ; submodule
(require-package 'web-mode)
(require-package 'csv-mode)
(require-package 'typescript-mode)
(require-package 'wc-mode)

(require-package 'htmlize) ; prefer stable version


(require-package 'scratch)
(require-package 'persistent-scratch)

(require-package 'treemacs)
(require-package 'treemacs-projectile)
(require-package 'treemacs-nerd-icons)
(require-package 'treemacs-tab-bar)
(require-package 'treemacs-magit)
(require-package 'lsp-treemacs)





;;(require-package 'winum)
(require-package 'ace-window)

(require-package 'native-complete)

(require-package 'find-file-in-project)





;; (require-package 'evil)
;; (require-package 'evil-escape)
;; (require-package 'evil-exchange)
;; (require-package 'evil-find-char-pinyin)
;; (require-package 'evil-mark-replace)
;; (require-package 'evil-matchit)
;; (require-package 'evil-nerd-commenter)
;; (require-package 'evil-surround)
;; (require-package 'evil-visualstar)

;; run "M-x pdf-tool-install" at debian and open pdf in GUI Emacs
;;(require-package 'pdf-tools)

(require-package 'forge)
(require-package 'diminish)
(require-package 'popup) ; some old package need it
(require-package 'wgrep)
(require-package 'diredfl) ; font lock for `dired-mode'
(require-package 'ace-pinyin)
(require-package 'find-by-pinyin-dired)
(require-package 'pinyinlib)
;;(require-package 'pyim)
;;(require-package 'pyim-wbdict) ; someone may use wubi IME, not me
;;(require-package 'pyim-basedict)
;;(require-package 'esup)



(when sys/linuxp
  (require-package 'vterm))

;; magit sometime use packages which not released yet
;; so we place it at the end to make sure other packages are installed first
;; (require-package 'magit) ; submodule
(require-package 'git-modes)
(require-package 'git-timemachine)
(require-package 'git-gutter) ; use my patched version

;; edit
(require-package 'olivetti)
(require-package 'goto-chg) ; Goto last change
(require-package 'subword) ;Handling capitalized subwords in a nomenclature
(require-package 'avy)
(require-package 'avy-zap)
(require-package 'sudo-edit)
(require-package 'flyspell)
(require-package 'beginend)

;;(require-package 'move-text)
;;(require-package 'anzu)

;;(require-package 'expand-region) ; I prefer stable version

(use-package gnu-elpa-keyring-update)
(use-package all-the-icons
  :when (display-graphic-p))

;; Auto update packages
;; (use-package auto-package-update
;;   :init (setq auto-package-update-delete-old-versions t
;; 	      auto-package-update-hide-results t))




;; ctrlf, good isearch alternative
 (require-package 'ctrlf)
 (use-package ctrlf
   :hook (after-init . ctrlf-mode))

;; Settings for exec-path-from-shell
(require-package 'exec-path-from-shell)
(use-package exec-path-from-shell
  :defer nil
  :if (memq window-system '(mac ns x))
  :init (exec-path-from-shell-initialize))




;; Display available keybindings in popup
(require-package 'which-key)
(use-package which-key
  :diminish
  :bind (("C-h M-m" . which-key-show-major-mode)
         (:map help-map ("C-h" . which-key-C-h-dispatch)))

  :hook (after-init . which-key-mode)
  :custom
  (which-key-popup-type 'side-window)
  :init (setq which-key-max-description-length 30
              which-key-lighter nil
              which-key-show-remaining-keys t)
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements "C-c !" "flycheck")
  (which-key-add-key-based-replacements "C-c &" "yasnippet")
  (which-key-add-key-based-replacements "C-c @" "hideshow")
  (which-key-add-key-based-replacements "C-c d" "dict")
  (which-key-add-key-based-replacements "C-c l" "link-hint")
  (which-key-add-key-based-replacements "C-c n" "org-roam")
  (which-key-add-key-based-replacements "C-c t" "hl-todo")
  (which-key-add-key-based-replacements "C-c C-z" "browse")

  (which-key-add-key-based-replacements "C-x 8" "unicode")
  (which-key-add-key-based-replacements "C-x 8 e" "emoji")
  (which-key-add-key-based-replacements "C-x @" "modifior")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-x c" "citre")
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-x r" "rect & bookmark")
  (which-key-add-key-based-replacements "C-x t" "tab & treemacs")
  (which-key-add-key-based-replacements "C-x x" "buffer")
  (which-key-add-key-based-replacements "C-x C-a" "edebug")
  (which-key-add-key-based-replacements "C-x RET" "coding-system")
  (which-key-add-key-based-replacements "C-x X" "edebug")

  (which-key-add-major-mode-key-based-replacements 'org-mode
                                                   "C-c \"" "org-plot")
  (which-key-add-major-mode-key-based-replacements 'org-mode
                                                   "C-c C-v" "org-babel")
  (which-key-add-major-mode-key-based-replacements 'org-mode
                                                   "C-c C-x" "org-misc")

  (which-key-add-major-mode-key-based-replacements 'python-mode
                                                   "C-c C-t" "python-skeleton")

  (which-key-add-major-mode-key-based-replacements 'markdown-mode
                                                   "C-c C-a" "markdown-link")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
                                                   "C-c C-c" "markdown-command")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
                                                   "C-c C-s" "markdown-style")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
                                                   "C-c C-t" "markdown-header")
  (which-key-add-major-mode-key-based-replacements 'markdown-mode
                                                   "C-c C-x" "markdown-toggle")

  (which-key-add-major-mode-key-based-replacements 'gfm-mode
                                                   "C-c C-a" "markdown-link")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
                                                   "C-c C-c" "markdown-command")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
                                                   "C-c C-s" "markdown-style")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
                                                   "C-c C-t" "markdown-header")
  (which-key-add-major-mode-key-based-replacements 'gfm-mode
                                                   "C-c C-x" "markdown-toggle")

  (when (childframe-completion-workable-p)
    (use-package which-key-posframe
      :diminish
      :functions posframe-poshandler-frame-center-near-bottom
      :custom-face
      (which-key-posframe ((t (:inherit tooltip))))
      (which-key-posframe-border ((t (:inherit posframe-border :background unspecified))))
      :init
      (setq which-key-posframe-border-width posframe-border-width
            which-key-posframe-poshandler #'posframe-poshandler-frame-center-near-bottom
            which-key-posframe-parameters '((left-fringe . 8)
                                            (right-fringe . 8)))
      (which-key-posframe-mode 1))))





(my-run-with-idle-timer 2
	#'(lambda()
		(make-directory (expand-file-name "var/persistent-scratch" user-emacs-directory) t)
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
					  persistent-scratch-backup-directory (expand-file-name "var/persistent-scratch" user-emacs-directory)
		              persistent-scratch-save-file (expand-file-name "var/persistent-scratch/.persistent-scratch" user-emacs-directory))
			:config
				(persistent-scratch-setup-default)
				;;;###autoload
				(defun list-scratch-backups ()
				"List all available scratch buffer backups."
				(interactive)
				(let ((backup-files (directory-files persistent-scratch-backup-directory nil "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$")))
				  (if backup-files
				      (message "Available backups:\n%s" (string-join backup-files "\n"))
				    (message "No backups available."))))
				;;;###autoload
				(defun load-scratch-backup (date)
				"Load a scratch buffer backup from a specific DATE."
				(interactive
				 (list
				  (completing-read "Enter the date (YYYY-MM-DD): "
				                   (directory-files persistent-scratch-backup-directory nil "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$") nil t)))
				(let ((backup-file (expand-file-name date persistent-scratch-backup-directory)))
					(if (file-exists-p backup-file)
						(with-temp-buffer
						(insert-file-contents backup-file)
						(let ((content (buffer-string)))
						(with-current-buffer "*scratch*"
							(erase-buffer)
							(insert content)
							(message "Loaded backup from %s" date))))
					(message "Backup file for date %s does not exist" date))))

				;; 将列出备份和加载备份函数绑定到 M-x 命令
				;;(global-set-key (kbd "C-c l") 'list-scratch-backups)
				;;(global-set-key (kbd "C-c r") 'load-scratch-backup)
)))

(my-run-with-idle-timer 2
	#'(lambda()
	   ;; Jump to Chinese characters
	    (use-package ace-pinyin
	      :diminish
	      :hook (after-init . ace-pinyin-global-mode))
))


(require-package 'simple-httpd)
;; Do NOT start web-server by default, prefer manual way
;; per project setup may be better
(setq httpd-root "/var/www")
(setq httpd-port 4444)
;;;###autoload
(defun httpd-restart-now ()
  (interactive)
  (httpd-stop)
  (httpd-start)
  (message "http://localhost:%d/ at %s restarted"
           httpd-port httpd-root))


(unless sys/win32p
	;; Open files as another user
	(my-run-with-idle-timer 2 #'(use-package sudo-edit))

	;; On-the-fly spell checker
	(use-package flyspell
		:ensure t
		:defer 2
		:diminish flyspell-mode
		:if (executable-find "aspell")
		:hook (((text-mode outline-mode) . flyspell-mode)
		       (prog-mode . flyspell-prog-mode)
		       (flyspell-mode . (lambda ()
		                          (unbind-key "C-;" flyspell-mode-map)
		                          (unbind-key "C-," flyspell-mode-map)
		                          (unbind-key "C-." flyspell-mode-map))))
		:init
		(setq flyspell-issue-message-flag nil)
		(setq ispell-program-name "aspell")
		(setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))))



;; Automatically reload files was modified by external program
;; (require-package 'autorevert)  ; builtin
(use-package autorevert
  :ensure nil
  :diminish
  :defer 2
  :hook (after-init . global-auto-revert-mode))


;; Delete Behavior
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(add-hook 'after-init-hook 'delete-selection-mode)

;; Electric-Pair
(add-hook 'after-init-hook 'electric-indent-mode)
(add-hook 'prog-mode-hook 'electric-pair-mode)
(add-hook 'prog-mode-hook 'electric-layout-mode)


;; ibuffer
(use-package ibuffer
  :init (defalias 'list-buffers 'ibuffer))


;; ;; Ido ( instead of ivy & counsel & swiper)
;; (setq-default ido-auto-merge-work-directories-length -1
;; 	      ido-enable-flex-matching t
;; 	      isearch-lazy-count t
;; 	      lazy-count-prefix-format "%s/%s: ")
;; (setq completion-ignored-extensions '(".o" ".elc" "~" ".bin" ".bak" ".obj" ".map" ".a" ".ln" ".class"))
;; (fido-mode t)
