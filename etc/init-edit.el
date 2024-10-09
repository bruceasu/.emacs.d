;; -*- coding: utf-8; lexical-binding: t; -*-
;;================================================
;; edit
;;================================================
(eval-when-compile
  (require '+const)
  (require '+custom)
  (require '+fn)
  (require 'init-key)
  (require 'init-package)
  )


    
(require-package 'paredit) ;; useful for lisp
(require-package 'tagedit) ;; useful for html
(require-package 'cliphist)
(require-package 'iedit)
(require-package 'wgrep) ;; eidt the grep / rg result then apply to the origin buffer. Cancel is supportted.
(require-package 'olivetti)
(require-package 'subword) ;Handling capitalized subwords in a nomenclature
(require-package 'flyspell)
(require-package 'markdown-mode)
;;(require-package 'textile-mode)
(require-package 'langtool) ; my own patched version is better an open-source grammar, spelling, and style checker, directly into Emacs. LanguageTool supports multiple languages, including English, Spanish, French, German, and many others, making it a versatile tool for checking the quality of your writing. 
;; org-mode
(require-package 'toc-org)
;; org => ppt
;;(require-package 'org-re-reveal)
;;(require-package 'sage-shell-mode)
;;(require-package 'ob-sagemath)
;;(require-package 'vimrc-mode)
;;(require-package 'qrencode)
;;(require-package 'cmake-mode)

;;;###autoload
(defun too-long-file-p ()
  "Check whether the file is too long."
  (or (> (buffer-size) 100000)
      (and (fboundp 'buffer-line-statistics)
           (> (car (buffer-line-statistics)) 10000))))

;; expand-region
(my-run-with-idle-timer 2
                        #'(lambda()
                            ;;(require-package 'expand-region) ; I prefer stable version
                            ;; expand-region :load-path "~/.emacs.d/extensions/expand-region"
                            (lazy-load-global-keys
                             '(("M-=" . er/expand-region)
                               ("M--" . er/contract-region)
                               )
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
(my-run-with-idle-timer 1
  #'(lambda()
  	(require-package 'autorevert)
	(use-package autorevert
	  :ensure nil
	  :diminish
	  :defer 2
	  :hook (after-init . global-auto-revert-mode))))


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


;; hungry-delete
(my-run-with-idle-timer 2
 #'(lambda()
    (require-package 'hungry-delete)
    (require 'hungry-delete)          
    (with-eval-after-load 'hungry-delete
      (setq hungry-delete-chars-to-skip " \t\f\v"
            hungry-delete-except-modes
            '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode))
      )))


;; Treat undo history as a tree, ^x u
(my-run-with-idle-timer 2 #'(lambda()
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
))
;; Handling capitalized subwords in a nomenclature
(my-run-with-idle-timer 2
    #'(lambda()
          (require 'subword)
          (add-hook 'prog-mode-hook #'subword-mode)
          (add-hook 'mimibuffer-setup #'subword-mode)))
;; Hanlde minified code
(if emacs/>=27p
    (add-hook 'after-init-hook #'global-so-long-mode))


(unless sys/win32p
  ;; Open files as another user
  (my-run-with-idle-timer 2
  	#'(lambda()
  		(require-package 'sudo-edit)
  		(use-package sudo-edit)))
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


;;; ### basic-toolkit ###
(lazy-load-global-keys
 '(
   ("M-s-n" . comment-part-move-down)   ;向下移动注释
   ("M-s-p" . comment-part-move-up)     ;向上移动注释
   ("C-s-n" . comment-dwim-next-line)   ;移动到上一行并注释
   ("C-s-p" . comment-dwim-prev-line)   ;移动到下一行并注释
   ("M-2" . indent-buffer)              ;自动格式化当前Buffer
   ("M-z" . upcase-char)                ;Upcase char handly with capitalize-word
   ;;("C-x u" . mark-line)              ;选中整行
   ("s-k" . kill-and-join-forward)      ;在缩进的行之间删除
   ("M-G" . goto-column)                ;到指定列
   ("C->" . remember-init)              ;记忆初始函数
   ("C-<" . remember-jump)              ;记忆跳转函数
   ("M-s-," . point-stack-pop)          ;buffer索引跳转
   ("M-s-." . point-stack-push)         ;buffer索引标记
   ("s-g" . goto-percent) ;跳转到当前Buffer的文本百分比, 单位为字符
   ("M-I" . backward-indent)            ;向后移动4个字符
                                        ;   ("s-J" . scroll-up-one-line)         ;向上滚动一行
                                        ;   ("s-K" . scroll-down-one-line)       ;向下滚动一行
   ("<f2>" . refresh-file)              ;自动刷新文件
   ("s-f" . find-file-root)             ;用root打开文件
   ("s-r" . find-file-smb)              ;访问sambao
   ("C-J" . join-lines)                ;连接行
   ("M-c" . endless/capitalize)
   ("M-l" . endless/downcase)
   ("M-u" . endless/upcase)
)
 "basic-toolkit")



;; ### Rect ###
;; --- 矩形操作
(lazy-load-global-keys
 '(
   ("s-M" . rm-set-mark)                ;矩形标记
   ("s-X" . rm-exchange-point-and-mark) ;矩形对角交换
   ("s-D" . rm-kill-region)             ;矩形删除
   ("s-S" . rm-kill-ring-save)          ;矩形保存
   ("s-Y" . yank-rectangle)             ;粘帖矩形
   ("s-O" . open-rectangle)       ;用空白填充矩形, 并向右移动文本
   ("s-C" . clear-rectangle)      ;清空矩形
   ("s-T" . string-rectangle)     ;用字符串替代矩形的每一行
   ("s-I" . string-insert-rectangle)    ;插入字符串在矩形的每一行
   ("s-F" . delete-whitespace-rectangle) ;删除矩形中空格
   ("s-\"" . copy-rectangle-to-register) ;拷贝矩形到寄存器
   ("s-:" . mark-rectangle-to-end)       ;标记矩形到行末
   )
 "rect-extension")


;;; ### open new line ###
(lazy-load-global-keys
  '(
    ("C-o" . open-newline-above)         ;在上面一行新建一行
    ("C-l" . open-newline-below)         ;在下面一行新建一行
    )
  "open-newline")


;; ### duplicate-line ###
(lazy-load-global-keys
 '(
   ("C-S-o" . duplicate-line-or-region-above) ;向上复制当前行或区域
   ("C-S-l" . duplicate-line-or-region-below) ;向下复制当前行或区域
   ("C-S-s-o" . duplicate-line-above-comment) ;复制当前行到上一行, 并注释当前行
   ("C-S-s-l" . duplicate-line-below-comment) ;复制当前行到下一行, 并注释当前行
   ("C-:" . comment-or-uncomment-region+)     ;注释当前行

   )
 "duplicate-line")


;; ### move text ###
(lazy-load-global-keys
 '(
   ("M-N" . move-text-down) ;把光标所在的整行文字(或标记)下移一行
   ("M-P" . move-text-up)   ;把光标所在的整行文字(或标记)上移一行
   ("M-<DOWN>" . move-text-down)    ;把光标所在的整行文字(或标记)下移一行
   ("M-<UP>"   . move-text-up)    ;把光标所在的整行文字(或标记)上移一行
   )
 "move-text")
 
;;; ### Delete block ###
;;; --- 快速删除光标左右的内容
(lazy-load-global-keys
 '(
   ("M-," . delete-block-backward)
   ("M-." . delete-block-forward))
 "delete-block")
 
;;; ### Thingh-edit ###
;;; --- 增强式编辑当前光标的对象
(lazy-load-global-keys
 '(
   ("C-c w" . thing-copy-word)
   ("C-c s" . thing-copy-symbol)
   ("C-c m" . thing-copy-email)
   ("C-c f" . thing-copy-filename)
   ("C-c u" . thing-copy-url)
   ("C-c x" . thing-copy-sexp)
   ("C-c g" . thing-copy-page)
   ("C-c t" . thing-copy-sentence)
   ("C-c o" . thing-copy-witespace)
   ("C-c i" . thing-copy-list)
   ("C-c c" . thing-copy-comment)
   ("C-c h" . thing-copy-defun)
   ("C-c p" . thing-copy-parentheses)
   ("C-c l" . thing-copy-line)
   ("C-c a" . thing-copy-to-line-begining)
   ("C-c e" . thing-copy-to-line-end)
   ("C-c W" . thing-cut-word)
   ("C-c S" . thing-cut-symbol)
   ("C-c M" . thing-cut-email)
   ("C-c F" . thing-cut-filename)
   ("C-c G" . thing-cut-page)
   ("C-c T" . thing-cut-sentence)
   ("C-c O" . thing-cut-whitespace)
   ("C-c I" . thing-cut-list)
   ("C-c C" . thing-cut-comment)
   ("C-c H" . thing-cut-defun)
   ("C-c P" . thing-cut-parentheses)
   ("C-c L" . thing-cut-line)
   ("C-c A" . thing-cut-to-line-beginning)
   ("C-c E" . thing-cut-to-line-end)
   )
 "thing-edit"
 "C-z"
 )
(provide 'init-edit)
