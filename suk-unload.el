;;; ### Insert translated name ###
;; youdao / google
(setq insert-translated-name-translate-engine "google")
(lazy-load-global-keys
 '(
   ("," . insert-translated-name-insert-with-underline)
   ("." . insert-translated-name-insert-with-camel)
   ("/" . insert-translated-name-insert)
   )
 "insert-translated-name"
 "C-z"
 )

(when (display-graphic-p)
  (use-package centaur-tabs
    :demand
    :init
    ;; Set the style to rounded with icons
    (setq centaur-tabs-style "bar")
    (setq centaur-tabs-set-icons t)
    :config
    (centaur-tabs-mode t)
    :bind
    ("C-<prior>" . centaur-tabs-backward)  ;; Ctrl PgUp
    ("C-<next>"  . centaur-tabs-forward))  ;; Ctrl PgDn
)

;;; ### Sdcv ###
;;; --- 星际译王命令行
(when  (eq system-type 'gnu/linux)
    (lazy-load-global-keys
     '(("p" . sdcv-search-pointer)           ;光标处的单词, buffer显示
       ("P" . sdcv-search-pointer+)          ;光标处的单词, tooltip显示
       ("i" . sdcv-search-input)             ;输入的单词, buffer显示
       (";" . sdcv-search-input+)
       ("y" . my-youdao-dictionary-search-at-point)
       ("Y" . youdao-dictionary-search-at-point)
       ("g" . google-translate-at-point)
       ("G" . google-translate-query-translate)
       ("s" . google-translate-smooth-translate)
       ("f" . fanyi-dwim)
       ("d" . fanyi-dwim2)
       ("h" . fanyi-from-history)
       )
     "init-translate"
     "C-z"))

(require-package 'command-log-mode) ;; show the command you press the shortcuts. M-x command-log-mode, M-x clm/open-command-log-buffer

(require 'eglot)
;;(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-10"))
;;(add-hook 'c-mode-hook 'eglot-ensure)
;;(add-hook 'c++-mode-hook 'eglot-ensure)

;; Java 需要JDK17+
(setenv "JAVA_HOME" "~/apps/jdk-21.0.5")
(setenv "PATH" (concat "$PATH:~/apps/jdk-21.0.5/bin"))
(defconst my-eclipse-jdt-home "/home/suk/.local/share/eclipse.jdt.ls/plugins/org.eclipse.equinox.launcher_1.6.700.v20231214-2017.jar")

(defun my-eglot-eclipse-jdt-contact (interactive)
  "Contact with the jdt server input INTERACTIVE."
  (let ((cp (getenv "CLASSPATH")))
    (setenv "CLASSPATH" (concat cp ":" my-eclipse-jdt-home))
    (unwind-protect (eglot--eclipse-jdt-contact nil)
      (setenv "CLASSPATH" cp))))
(setcdr (assq 'java-mode eglot-server-programs) #'my-eglot-eclipse-jdt-contact)
(add-hook 'java-mode-hook 'eglot-java-mode)

;;Show function arglist or variable docstring
(run-with-idle-timer
 1
 nil
 #'(lambda()
     (use-package eldoc
       :ensure nil
       :diminish
       :config
       (use-package eldoc-box
           :diminish (eldoc-box-hover-mode eldoc-box-hover-at-point-mode)
           :custom
           (eldoc-box-lighter nil)
           (eldoc-box-only-multi-line t)
           (eldoc-box-clear-with-C-g t)
           :custom-face
           (eldoc-box-border ((t (:inherit posframe-border :background unspecified))))
           (eldoc-box-body ((t (:inherit tooltip))))
           :hook ((eglot-managed-mode . eldoc-box-hover-at-point-mode))
           :config
           ;; Prettify `eldoc-box' frame
           (setf (alist-get 'left-fringe eldoc-box-frame-parameters) 8
                 (alist-get 'right-fringe eldoc-box-frame-parameters) 8)))
     ))
