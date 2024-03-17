(eval-when-compile
  (require '+const)
  (require '+custom)
  (require 'init-package)
  )

(use-package expand-region
  :load-path "~/.emacs.d/extensions/expand-region"
  :bind ("C-+" . er/expand-region)
  :config
  (when (suk-treesit-available-p)
	(defun treesit-mark-bigger-node ()
	  "Use tree-sitter to mark regions."
	  (let* ((root (treesit-buffer-root-node))
			 (node (treesit-node-descendant-for-range root (region-beginning) (region-end)))
			 (node-start (treesit-node-start node))
			 (node-end (treesit-node-end node)))
		;; Node fits the region exactly. Try its parent node instead.
		(when (and (= (region-beginning) node-start) (= (region-end) node-end))
		  (when-let ((node (treesit-node-parent node)))
			(setq node-start (treesit-node-start node)
				  node-end (treesit-node-end node))))
		(set-mark node-end)
		(goto-char node-start)))
	))

;; ;; Jump to Chinese characters
;; (use-package ace-pinyin
;;   :diminish
;;   :hook (after-init . ace-pinyin-global-mode))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :hook ((after-init . global-aggressive-indent-mode)
         ;; NOTE: Disable in large files due to the performance issues
         ;; https://github.com/Malabarba/aggressive-indent-mode/issues/73
         (find-file . (lambda ()
                        (when (too-long-file-p)
                          (aggressive-indent-mode -1)))))
  :config
  ;; Disable in some modes
  (dolist (mode '(gitconfig-mode
                  asm-mode web-mode html-mode css-mode
                  go-mode scala-mode
                  shell-mode term-mode vterm-mode
                  prolog-inferior-mode))
    (add-to-list 'aggressive-indent-excluded-modes mode))

  ;; Disable in some commands
  (add-to-list 'aggressive-indent-protected-commands #'delete-trailing-whitespace t)

  ;; Be slightly less aggressive in C/C++/C#/Java/Go/Swift
  (add-to-list 'aggressive-indent-dont-indent-if
               '(and (derived-mode-p 'c-mode 'c++-mode 'csharp-mode
                                     'java-mode 'go-mode 'swift-mode)
                     (null (string-match "\\([;{}]\\|\\b\\(if\\|for\\|while\\)\\b\\)"
                                         )))))

;; Rectangle
(use-package rect
  :ensure nil
  :bind (:map text-mode-map
              ("<C-return>" . rect-hydra/body)
              :map prog-mode-map
              ("<C-return>" . rect-hydra/body))
  :init
  (with-eval-after-load 'org
    (bind-key "<s-return>" #'rect-hydra/body org-mode-map))
  (with-eval-after-load 'wgrep
    (bind-key "<C-return>" #'rect-hydra/body wgrep-mode-map))
  (with-eval-after-load 'wdired
    (bind-key "<C-return>" #'rect-hydra/body wdired-mode-map))
  :pretty-hydra
  ((:title (pretty-hydra-title "Rectangle" 'mdicon "nf-md-border_all")
           :color amaranth :body-pre (rectangle-mark-mode) :post (deactivate-mark) :quit-key ("q" "C-g"))
   ("Move"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→"))
    "Action"
    (("w" copy-rectangle-as-kill "copy") ; C-x r M-w
     ("y" yank-rectangle "yank")         ; C-x r y
     ("t" string-rectangle "string")     ; C-x r t
     ("d" kill-rectangle "kill")         ; C-x r d
     ("c" clear-rectangle "clear")       ; C-x r c
     ("o" open-rectangle "open"))        ; C-x r o
    "Misc"
    (("N" rectangle-number-lines "number lines")        ; C-x r N
     ("e" rectangle-exchange-point-and-mark "exchange") ; C-x C-x
     ("u" undo "undo")
     ("r" (if (region-active-p)
              (deactivate-mark)
            (rectangle-mark-mode 1))
      "reset")))))

;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Pass a URL to a WWW browser
(use-package browse-url
  :ensure nil
  :defines dired-mode-map
  :bind (("C-c C-z ." . browse-url-at-point)
         ("C-c C-z b" . browse-url-of-buffer)
         ("C-c C-z r" . browse-url-of-region)
         ("C-c C-z u" . browse-url)
         ("C-c C-z e" . browse-url-emacs)
         ("C-c C-z v" . browse-url-of-file))
  :init
  (with-eval-after-load 'dired
    (bind-key "C-c C-z f" #'browse-url-of-file dired-mode-map))

  (let ((cmd-exe "c:/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic)
      (when (daemonp)
        (advice-add #'browse-url :override #'browse-url-generic)))))

;; Jump to things in Emacs tree-style
(use-package avy
  :bind (("C-:"   . avy-goto-char)
         ("C-'"   . avy-goto-char-2)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :hook (after-init . avy-setup-default)
  :config (setq avy-all-windows nil
                avy-all-windows-alt t
                avy-background t
                avy-style 'pre))

;; Kill text between the point and the character CHAR
(use-package avy-zap
  :bind (("M-z" . avy-zap-to-char-dwim)
         ("M-Z" . avy-zap-up-to-char-dwim)))

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

;; Redefine M-< and M-> for some modes
(use-package beginend
  :diminish beginend-global-mode
  :hook (after-init . beginend-global-mode)
  :config (mapc (lambda (pair)
                  (diminish (cdr pair)))
                beginend-modes))

;;使用自己的函数
;; Drag stuff (lines, words, region, etc...) around
;; (use-package drag-stuff
;;   :diminish
;;   :autoload drag-stuff-define-keys
;;   :hook (after-init . drag-stuff-global-mode)
;;   :config
;;   (add-to-list 'drag-stuff-except-modes 'org-mode)
;;   (drag-stuff-define-keys))

;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode)))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (([remap move-beginning-of-line] . mwim-beginning)
		 ([remap move-end-of-line] . mwim-end)))

;; Treat undo history as a tree, ^x u
(if emacs/>=28p
    (use-package vundo
      :ensure nil
      :load-path "~/.emacs.d/extensions/vundo"
      :bind (("C-x u" . vundo)
             ("C-/" . vundo))
      :config (setq vundo-glyph-alist vundo-unicode-symbols))
  (use-package undo-tree
    :diminish undo-tree-mode
    :bind (("C-x u" . undo-trees)
           ("C-/" . undo-tree-undo)
           ("C-?" . undo-tree-redo)
           )
    :hook (after-init . global-undo-tree-mode)
    :init (setq undo-tree-visualizer-timestamps t
                undo-tree-visualizer-diff t
                undo-tree-enable-undo-in-region nil
                undo-tree-auto-save-history nil)
    ;; HACK: keep the diff window
    (with-no-warnings
      (make-variable-buffer-local 'undo-tree-visualizer-diff)
      (setq-default undo-tree-visualizer-diff t))))
;; Goto last change
(use-package goto-chg)

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; Flexible text folding
(use-package hideshow
  :ensure nil
  :diminish hs-minor-mode
  :pretty-hydra
  ((:title (pretty-hydra-title "HideShow" 'octicon "nf-oct-fold")
    :color amaranth :quit-key ("q" "C-g"))
   ("Fold"
    (("t" hs-toggle-all "toggle all")
     ("a" hs-show-all "show all")
     ("i" hs-hide-all "hide all")
     ("g" hs-toggle-hiding "toggle hiding")
     ("c" hs-cycle "cycle block")
     ("s" hs-show-block "show block")
     ("h" hs-hide-block "hide block")
     ("l" hs-hide-level "hide level"))
    "Move"
    (("C-a" mwim-beginning-of-code-or-line "⭰")
     ("C-e" mwim-end-of-code-or-line "⭲")
     ("C-b" backward-char "←")
     ("C-n" next-line "↓")
     ("C-p" previous-line "↑")
     ("C-f" forward-char "→")
     ("C-v" pager-page-down "↘")
     ("M-v" pager-page-up "↖")
     ("M-<" beginning-of-buffer "⭶")
     ("M->" end-of-buffer "⭸"))))
  :bind (:map hs-minor-mode-map
         ("C-~" . hideshow-hydra/body)
         ("C-S-<escape>" . hideshow-hydra/body))
  :hook (prog-mode . hs-minor-mode)
  :config
  ;; More functions
  ;; @see https://karthinks.com/software/simple-folding-with-hideshow/
  (defun hs-cycle (&optional level)
    (interactive "p")
    (let (message-log-max
          (inhibit-message t))
      (if (= level 1)
          (pcase last-command
            ('hs-cycle
             (hs-hide-level 1)
             (setq this-command 'hs-cycle-children))
            ('hs-cycle-children
             (save-excursion (hs-show-block))
             (setq this-command 'hs-cycle-subtree))
            ('hs-cycle-subtree
             (hs-hide-block))
            (_
             (if (not (hs-already-hidden-p))
                 (hs-hide-block)
               (hs-hide-level 1)
               (setq this-command 'hs-cycle-children))))
        (hs-hide-level level)
        (setq this-command 'hs-hide-level))))

  (defun hs-toggle-all ()
    "Toggle hide/show all."
    (interactive)
    (pcase last-command
      ('hs-toggle-all
       (save-excursion (hs-show-all))
       (setq this-command 'hs-global-show))
      (_ (hs-hide-all))))

  ;; Display line counts
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
                   (concat
                    " "
                    (propertize
                     (if (char-displayable-p ?⏷) "⏷" "...")
                     'face 'shadow)
                    (propertize
                     (format " (%d lines)"
                             (count-lines (overlay-start ov)
                                          (overlay-end ov)))
                     'face '(:inherit shadow :height 0.8))
                    " "))))
  (setq hs-set-up-overlay #'hs-display-code-line-counts))

;; Narrow/Widen
(use-package fancy-narrow
  :diminish
  :hook (after-init . fancy-narrow-mode))

;; Hanlde minified code
(use-package so-long
  :hook (after-init . global-so-long-mode))


;; Nice writing
(use-package olivetti
  :diminish
  :bind ("<f7>" . olivetti-mode)
  :init (setq olivetti-body-width 0.62))

;; Edit text for browsers with GhostText or AtomicChrome extension
(use-package atomic-chrome
  :hook ((emacs-startup . atomic-chrome-start-server)
         (atomic-chrome-edit-mode . delete-other-windows))
  :init (setq atomic-chrome-buffer-frame-width 100
			  atomic-chrome-buffer-frame-height 30
			  atomic-chrome-buffer-open-style 'frame)
  :config
  (when (fboundp 'gfm-mode)
    (setq atomic-chrome-url-major-mode-alist
		  '(("github\\.com" . gfm-mode)
            ("gitlab\\.com" . gfm-mode)))))

(unless sys/win32p
	;; Open files as another user
	(use-package sudo-edit)
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
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

  )
(provide 'init-edit)
