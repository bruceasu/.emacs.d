(provide 'init-idle)

(eval-when-compile
  (require '+const)
  (require 'init-package)
  (require 'init-key))

;;;###autoload
(defun loadUndo()
  ;; Treat undo history as a tree, ^x u
  (if emacs/>=28p
	  (progn
		;; vundo :load-path "~/.emacs.d/extensions/vundo"
		(lazy-load-global-keys
		 '(("C-x u" . vundo)
		   ("C-/" . vundo)
		   )
		 "vundo")
		(with-eval-after-load 'vundo
		  (setq vundo-glyph-alist vundo-unicode-symbols)))
	(progn
	  ;; use undo-tree
	  (setq undo-tree-visualizer-timestamps t
		    undo-tree-visualizer-diff t
		    undo-tree-enable-undo-in-region nil
		    undo-tree-auto-save-history nil)
	  ;; HACK: keep the diff window
	  (with-no-warnings
		(make-variable-buffer-local 'undo-tree-visualizer-diff)
		(setq-default undo-tree-visualizer-diff t))

	  (lazy-load-global-keys
	   '(("C-x u" . undo-trees)
		 ("C-/" . undo-tree-undo)
		 ("C-?" . undo-tree-redo)
		 )
	   "undo-tree")

	  (with-eval-after-load 'undo-tree
		(add-hook 'after-init-hook #'global-undo-tree-mode))
	  )
    )
  )

(run-with-idle-timer
 1
 nil
 #'(lambda()
     (loadUndo)
     (require 'init-auto-save)
	 (require 'init-highlight)
	 (require 'init-window)
	 (require 'load-abbrev)

	 ;; chmod +x
	 ;; ref. http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#chmod
	 (add-hook 'after-save-hook'executable-make-buffer-file-executable-if-script-p)
	 (autoload 'calendar "init-calendar" "Config Chinese calendar " t)
	 ;; Hanlde minified code
	 (if emacs/>=27p (add-hook 'after-init-hook #'global-so-long-mode))
     ;;saveplace
	 (setq save-place-file (expand-file-name "saveplace" suk-emacs-var-dir)) ; "~/.emacs.d/var/saveplace"
	 (save-place-mode 1)
	 ;;If emacs is slow to exit after enabling saveplace, you may be
	 ;;running afoul of save-place-forget-unreadable-files. On exit,
	 ;;it checks that every loaded file is readable before saving its
	 ;;buffer position - potentially very slow if you use NFS.
	 (setq save-place-forget-unreadable-files nil)
     (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
           history-length 1000
           savehist-additional-variables '(mark-ring
                                           global-mark-ring
                                           search-ring
                                           regexp-search-ring
                                           extended-command-history)
           savehist-autosave-interval 300
           savehist-file (expand-file-name "history" suk-emacs-var-dir) ; "~/.emacs.d/var/history"
           )
     (savehist-mode 1)
     ;; Programming
     (require 'init-ide)
     ;; Restore session at last.
     (require 'init-session)
     (emacs-session-restore)
     (unless (server-running-p)
       (server-start))
     ;; Make gc pauses faster by decreasing the threshold.
	 (setq gc-cons-threshold (* 16 1000 1000))))

(run-with-idle-timer
 2
 nil
 #'(lambda()
	 (when sys/linuxp
	   (require 'init-im)   ;; windows 下表现不好
	   (require 'init-sudo))

	 (when sys/win32p
	   ;; Key Modifiers
	   ;; make PC keyboard's Win key or other to type Super or Hyper
	   ;; (setq w32-pass-lwindow-to-system nil)
	   (setq w32-lwindow-modifier 'super)    ; Left Windows key
	   (setq w32-apps-modifier 'hyper)       ; Menu/App key
	   ;; w32-register-hot-key 在 Emacs 中是用来在Windows系统上注册全局热键的函数，
	   ;; 但它并不直接关联到执行 Emacs Lisp 函数。
	   ;; 这个函数更多的是告诉Windows操作系统，
	   ;; “当这个按键组合被按下时，应该通知Emacs”。
	   ;; 要使Emacs在按下这个热键时执行特定的Elisp函数，还需要在Emacs内部设置相应的
	   ;; 响应机制。这通常涉及到编写一些额外的Elisp代码来监听这个热键，
	   ;; 并在它被按下时触发相应的操作。
	   ;; 实际上，w32-register-hot-key 更多地用于在操作系统级别处理特定的按键组合，
	   ;; 而不是在Emacs的编辑环境内。如果您想在Emacs内部绑定热键并执行函数，
	   ;; 通常会使用像 global-set-key 或 define-key 这样的函数。
	   (w32-register-hot-key [s-t])

	   ;; Optimization
	   (setq w32-get-true-file-attributes nil   ; decrease file IO workload
	         w32-use-native-image-API t         ; use native w32 API
	         w32-pipe-read-delay 0              ; faster IPC
	         w32-pipe-buffer-size 65536)        ; read more at a time (64K, was 4K)
	   )
	 ))

(with-eval-after-load 'hydra
  (defhydra my-hydra-describe (:color blue :hint nil)
    "
Describe Something: (q to quit)
_a_ all help for everything screen
_b_ bindings
_c_ char
_C_ coding system
_f_ function
_i_ input method
_k_ key briefly
_K_ key
_l_ language environment
_m_ major mode
_M_ minor mode
_n_ current coding system briefly
_N_ current coding system full
_o_ lighter indicator
_O_ lighter symbol
_p_ package
_P_ text properties
_s_ symbol
_t_ theme
_v_ variable
_w_ where is something defined
"
	("b" describe-bindings)
	("C" describe-categories)
	("c" describe-char)
	("C" describe-coding-system)
	("f" describe-function)
	("i" describe-input-method)
	("K" describe-key)
	("k" describe-key-briefly)
	("l" describe-language-environment)
	("M" describe-minor-mode)
	("m" describe-mode)
	("N" describe-current-coding-system)
	("n" describe-current-coding-system-briefly)
	("o" describe-minor-mode-from-indicator)
	("O" describe-minor-mode-from-symbol)
	("p" describe-package)
	("P" describe-text-properties)
	("q" nil)
	("a" help)
	("s" describe-symbol)
	("t" describe-theme)
	("v" describe-variable)
	("w" where-is))
  (global-set-key (kbd "C-c C-q") 'my-hydra-describe/body))


;; (run-with-idle-timer 5 nil
;; 	#'(lambda()
;; 		(defvar my-remotes
;; 			'((remote1 "username1" "remote1.example.com" "/path/to/private/key1.ppk")
;; 				(remote2 "username2" "remote2.example.com" "/path/to/private/key2.ppk")))

;; 		(defun connect-to-remote (remote)
;; 			"Connect to the specified REMOTE."
;; 			(interactive
;; 				(list (completing-read "Choose remote: " (mapcar 'car my-remotes))))
;; 			(let* ((remote-info (assoc remote my-remotes))
;; 				(username (cadr remote-info))
;; 				(remote-host (caddr remote-info))
;; 				(ppk-file (cadddr remote-info)))
;; 				(find-file (format "/ssh:%s@%s:/path/to/remote/file" username remote-host))
;; 				(setq tramp-default-method "plink")
;; 				(setq tramp-terminal-type "dumb")
;; 				(setq tramp-ssh-args (concat "-i " ppk-file))))

;; 		(defhydra hydra-connect-to-remote (:color blue)
;; 		  "Connect to Remote"
;; 		  ("1" (connect-to-remote "remote1") "Remote 1")
;; 		  ("2" (connect-to-remote "remote2") "Remote 2")
;; 		  ("q" nil "cancel" :color blue))

;; 		;; (global-set-key (kbd "C-c r") 'connect-to-remote)
;; 		(global-set-key (kbd "C-c r") 'hydra-connect-to-remote/body)


;; 	))
