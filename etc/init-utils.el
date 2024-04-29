;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-


(provide 'init-utils)

(eval-when-compile
  (require '+const)
  (require '+custom)
  (require 'init-package)
  )

;;(use-package bind-key)
;;Enhance M-x, use counsel-M-x
;;(use-package amx)


;; Display available keybindings in popup
(use-package which-key
  :diminish
  :bind (("C-h M-m" . which-key-show-major-mode)
         (:map help-map ("C-h" . which-key-C-h-dispatch)))

  :hook (after-init . which-key-mode)
  :custom
  ;; µ¯³ö·½Ê½£¬µ×²¿µ¯³ö
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
