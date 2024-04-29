(eval-when-compile
  (require '+const)
  (require '+custom)
  (require '+func)
  (require 'init-package)
  )


;; @see http://endlessparentheses.com/super-smart-capitalization.html
;;;###autoload
(defun endless/convert-punctuation (rg rp)
  "Look for regexp RG around point, and replace with RP.
Only applies to text-mode."
  (let* ((f "\\(%s\\)\\(%s\\)")
         (space "?:[[:blank:]\n\r]*"))
    ;; We obviously don't want to do this in prog-mode.
    (if (and (derived-mode-p 'text-mode)
             (or (looking-at (format f space rg))
                 (looking-back (format f rg space) (point-min))))
        (replace-match rp nil nil nil 1))))
;;;###autoload
(defun endless/call-subword-cmd (fn)
  (my-ensure 'subword)
  (call-interactively fn))
;;;###autoload
(defun endless/capitalize ()
  "Capitalize region or word.
Also converts commas to full stops, and kills
extraneous space at beginning of line."
  (interactive)
  (endless/convert-punctuation "," ".")
  (if (use-region-p)
      (call-interactively 'capitalize-region)
    ;; A single space at the start of a line:
    (when (looking-at "^\\s-\\b")
      ;; get rid of it!
      (delete-char 1))
    (endless/call-subword-cmd 'subword-capitalize)))
;;;###autoload
(defun endless/downcase ()
  "Downcase region or word.
Also converts full stops to commas."
  (interactive)
  (endless/convert-punctuation "\\." ",")
  (if (use-region-p)
      (call-interactively 'downcase-region)
    (endless/call-subword-cmd 'subword-downcase)))
;;;###autoload
(defun endless/upcase ()
  "Upcase region or word."
  (interactive)
  (if (use-region-p)
      (call-interactively 'upcase-region)
    (endless/call-subword-cmd 'subword-upcase)))

;; these bindings are fine
(global-set-key (kbd "M-c") 'endless/capitalize)
(global-set-key (kbd "M-l") 'endless/downcase)
(global-set-key (kbd "M-u") 'endless/upcase)

(my-run-with-idle-timer
 2
 #'(lambda()
     ;; expand-region :load-path "~/.emacs.d/extensions/expand-region"
     (lazy-load-global-keys
      '(("C-+" . er/expand-region))
      "expand-region")
     (with-eval-after-load 'expand-region
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
         ))))

;; ;; Jump to Chinese characters
;; (use-package ace-pinyin
;;   :diminish
;;   :hook (after-init . ace-pinyin-global-mode))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish
  :defer 2
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
  :defer 2
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
  :defer 2
  :hook (after-init . global-auto-revert-mode))

;; Pass a URL to a WWW browser
(use-package browse-url
  :ensure nil
  :defer 2
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
(my-run-with-idle-timer
 2
 #'(lambda()
     (with-eval-after-load 'avy
       (setq avy-all-windows nil
             avy-all-windows-alt t
             avy-background t
             avy-style 'pre)
       (add-hook 'after-init-hook #'avy-setup-default)
       )

     (lazy-load-global-keys
      ' (("C-:"   . avy-goto-char)
         ("C-'"   . avy-goto-char-2)
         ("M-g l" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
      "avy")
     ;; Kill text between the point and the character CHAR
     (lazy-load-global-keys
      '(("M-z" . avy-zap-to-char-dwim)
        ("M-Z" . avy-zap-up-to-char-dwim))
      "avy-zap")

     ))

;; Show number of matches in mode-line while searching
(my-run-with-idle-timer
 2
 #'(lambda()
     (use-package anzu
       :diminish
       :bind (([remap query-replace] . anzu-query-replace)
              ([remap query-replace-regexp] . anzu-query-replace-regexp)
              :map isearch-mode-map
              ([remap isearch-query-replace] . anzu-isearch-query-replace)
              ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
       :hook (after-init . global-anzu-mode))))

;; Redefine M-< and M-> for some modes
(my-run-with-idle-timer
 2
 #'(lambda()
     (require 'beginend)
     (add-hook 'after-init-hook #'(beginend-global-mode))
     (mapc (lambda (pair)
             (diminish (cdr pair)))
           beginend-modes)))

(my-run-with-idle-timer 2 #'(lambda()(require 'hungry-delete)))

(with-eval-after-load 'hungry-delete
  (setq hungry-delete-chars-to-skip " \t\f\v"
        hungry-delete-except-modes
        '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode))
  )


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
    ))

;; Goto last change
(my-run-with-idle-timer 2 #'(lambda()(require 'goto-chg)))

;; Handling capitalized subwords in a nomenclature
(my-run-with-idle-timer
 2
 #'(lambda()
     (require 'subword)
     (add-hook 'prog-mode-hook #'subword-mode)
     (add-hook 'mimibuffer-setup #'subword-mode)
     ))
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
  )

;; Hanlde minified code
(if emacs/>=27p
    (add-hook 'after-init-hook #'global-so-long-mode))


(lazy-load-global-keys
 '(
   ("<f7>" . olivetti-mode))
 "olivetti")

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
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

  )
(provide 'init-edit)
