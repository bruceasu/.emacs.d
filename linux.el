;; Your Linux specific settings

; (setenv "JAVA_HOME" "/usr/lib/jvm/java-14-openjdk-amd64")

;; Open files as another user
(run-with-idle-timer
 2
 nil
 #'(lambda()
   (require 'my-sudo)
    (require-package 'sudo-edit)
    ;;(use-package sudo-edit)
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
    
(run-with-idle-timer
  2
  nil
  #'(lambda()
  	;; run "M-x pdf-tool-install" at debian and open pdf in GUI Emacs
	;;(require-package 'pdf-tools) ;; use the package in extension
	(require-package 'nov) ; read epub
    (require 'init-im)   ;; windows 下表现不好
  
    (when (display-graphic-p)
      ;; only graphic packages
      (require-package 'vterm))
    (unless (display-graphic-p)
      ;; only conole packages
    )  
  ))


