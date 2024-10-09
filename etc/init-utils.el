;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

(provide 'init-utils)
(eval-when-compile
  (require '+const)
  (require 'init-package)
  )
;; efficiency
;;(require-package 'esup)
(require-package 'buffer-move)
(require-package 'helpful)
(require-package 'wc-mode)
(require-package 'ws-butler)
(require-package 'async)
;;(require-package 'amx)
(require-package 'popup) ; some old package need it
(require-package 'htmlize) ; prefer stable version
(require-package 'diminish)
;;(require-package 'scratch)
(require-package 'session)
(require-package 'unfill)
(require-package 'eww-lnum) ;; pluin for eww, a built-in web browser
(require-package 'rainbow-delimiters)

(require 'init-treemacs)


;; C-x r l to list bookmarks

;; Tools
(require-package 'pomodoro) ;; notify you to take a break for a long time work. pomodoro-start, pomodoro-pause, pomodoro-stop
;;(require-package 'request) ;; a http client
;;(require-package 'websocket) ; for debug debugging of browsers
;;(require-package 'simple-httpd)
;;(require-package 'highlight-symbol)
;;(require-package 'cpputils-cmake)
;;(require-package 'rust-mode)
;;(require-package 'auto-package-update)
;;(require-package 'keyfreq)

;; Test tools
;;(require-package 'command-log-mode) ;; show the command you press the shortcuts. M-x command-log-mode, M-x clm/open-command-log-buffer
(require-package 'regex-tool)

;; Music
;;(require-package 'emms)
;;(require-package 'pulseaudio-control)

;; Misc

;;(unless sys/win32p
;;  (use-package daemons)                 ; system services/daemons
;;  )

(unless emacs/>=28p
  (require-package 'undo-tree))

;;(use-package bind-key)
;;Enhance M-x, use counsel-M-x
;;(use-package amx)
;; Display available keybindings in popup
(require-package 'which-key)
(use-package which-key
  :diminish
  :bind (("C-h M-m" . which-key-show-major-mode)
         (:map help-map ("C-h" . which-key-C-h-dispatch)))

  :hook (after-init . which-key-mode)
  :custom
  ;; 弹出方式，底部弹出
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

;;;; Run commands in a popup frame
(defun prot-window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `prot-window-popup-frame'.
Use this function via a hook."
  (when (frame-parameter nil 'prot-window-popup-frame)
    (delete-frame)))
(defmacro prot-window-define-with-popup-frame (command)
  "Define interactive function which calls COMMAND in a new frame.
Make the new frame have the `prot-window-popup-frame' parameter."
  `(defun ,(intern (format "prot-window-popup-%s" command)) ()
     ,(format "Run `%s' in a popup frame with `prot-window-popup-frame' parameter.
Also see `prot-window-delete-popup-frame'." command)
     (interactive)
     (let ((frame (make-frame '((prot-window-popup-frame . t)))))
       (select-frame frame)
       (switch-to-buffer " prot-window-hidden-buffer-for-popup-frame")
       (condition-case nil
           (call-interactively ',command)
         ((quit error user-error)
          (delete-frame frame))))))
(declare-function org-capture "org-capture" (&optional goto keys))
(defvar org-capture-after-finalize-hook)
;;;###autoload (autoload 'prot-window-popup-org-capture "prot-window")
(prot-window-define-with-popup-frame org-capture)
(add-hook 'org-capture-after-finalize-hook #'prot-window-delete-popup-frame)
(require-package 'tmr)
(declare-function tmr "tmr" (time &optional description acknowledgep))
(defvar tmr-timer-created-functions)
;;;###autoload (autoload 'prot-window-popup-tmr "prot-window")
(prot-window-define-with-popup-frame tmr)
(add-hook 'tmr-timer-created-functions #'prot-window-delete-popup-frame)
;;;; The emacsclient call depends on the daemon or `server-mode' (I use the latter)
(use-package server
  :ensure nil
  :defer 1
  :config
  (unless (server-running-p)
    (server-start)))
;;;; The emacsclient calls that need ot be bound to system-wide keys
;; emacsclient -e '(prot-window-popup-org-capture)'
;; emacsclient -e '(prot-window-popup-tmr)'


;; Do NOT start elnode-web-server by default, prefer manual way

;; per project setup may be better
;; (setq httpd-root "/var/www")
(setq httpd-port 4444)
(defun httpd-restart-now ()
  (interactive)
  (httpd-stop)
  (httpd-start)
  (message "http://localhost:%d/ at %s restarted"
           httpd-port httpd-root))

(defun httpd-restart-at-default-directory ()
  (interactive)
  (setq httpd-root default-directory)
  (httpd-restart-now))
