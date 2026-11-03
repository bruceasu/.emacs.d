;; Your Linux specific settings

;; (setenv "JAVA_HOME" "/usr/lib/jvm/java-14-openjdk-amd64")


;; Open files as another user
(run-with-idle-timer
 2
 nil
 #'(lambda()
     (require 'my-sudo)
     ;; (use-package sudo-edit)
     (lazy-load-global-keys
      '(("a s" . suk/sudo/body))
      "my-sudo"
      "M-m"
      )
     ))

;; On-the-fly spell checker
(run-with-idle-timer
 2
 nil
 #'(lambda()
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
       (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))))

;; Pyim is vendored locally and declares these ELPA dependencies.
(use-package async)
(use-package xr)

;; 加载输入法
(run-with-idle-timer
 1
 nil
 #'(lambda()
     (let ((rime-ready
	    (and suk-rime
		 (condition-case nil
		     (progn
		       (require 'init-rime)
		       (and (boundp 'rime--module-path)
			    (stringp rime--module-path)
			    (file-exists-p rime--module-path)
			    (boundp 'rime-user-data-dir)
			    (file-directory-p
			     (expand-file-name rime-user-data-dir))))
		   (error nil)))))
       (unless rime-ready
	 (when suk-rime
	   (display-warning 'suk "Rime is unavailable; using Pyim instead."))
	 (require 'pyim-hyly)
	 (pyim-hyly-enable)))))

;; 其他
(run-with-idle-timer
 1
 nil
 #'(lambda()
     (when (display-graphic-p)
       ;; only graphic packages
       )
     (unless (display-graphic-p)
       ;; only conole packages
       )
     ))

(when (display-graphic-p)
  ;; only graphic packages
  (lazy-load-global-keys
   '(
     ("C-x C-t" . toggle-vterm))
   "init-vterm"
   )

  )
