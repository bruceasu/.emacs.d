;; Your Linux specific settings

;; (setenv "JAVA_HOME" "/usr/lib/jvm/java-14-openjdk-amd64")

;; 开发工具
(run-with-idle-timer
 1 nil
 #'(lambda()
     (require 'init-devtools)
     ))

;; Open files as another user
(run-with-idle-timer
 2
 nil
 #'(lambda()
     (require 'my-sudo)
     ;; (use-package sudo-edit)
     (lazy-load-global-keys
      '(("C-z C-s" . suk/sudo/body))
      "my-sudo"
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

;; 特有工具
(run-with-idle-timer
 2
 nil
 #'(lambda()
     ;; run "M-x pdf-tools-install" at debian and open pdf in GUI Emacs
     (use-package pdf-tools
       :ensure t
       :config (pdf-tools-install))
     ))

;; 加载输入法
(run-with-idle-timer
 1
 nil
 #'(lambda()
     (if suk-rime
         (require 'init-rime)
       ;;(require 'rain)
       (progn
         (require 'pyim-hyly)
         (pyim-hyly-enable))
       )
     ))

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
