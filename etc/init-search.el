(provide 'init-search)

;;; ### Isearch ###
;;; ---
(lazy-load-set-keys
 '(
   ("TAB" . isearch-complete)               ;isearch补全
   ("C-s" . isearch-repeat-forward)         ;重复向前搜索, 第一次可以用来搜索上一次的历史哟
   ("C-r" . isearch-repeat-backward)        ;重复向后搜索
   ("C-g" . isearch-abort)                  ;中止搜索
   ("C-w" . isearch-yank-word-or-char)      ;粘帖光标后的词或字符作为搜索对象
   ("C-y" . isearch-yank-line)              ;粘帖光标后的行作为搜索对象
   ("M-o" . isearch-delete-char)            ;删除
   ("M-p" . isearch-ring-retreat)           ;搜索历史向后
   ("M-n" . isearch-ring-adjust)            ;搜索历史向前
   ("M-y" . isearch-yank-kill)              ;从 kill ring 中粘帖最后一项到搜索对象后
   ("M-h" . isearch-yank-char)              ;粘帖光标后的字符到搜索对象
   ("M-e" . isearch-edit-string)            ;编辑搜索对象
   ("M-c" . isearch-toggle-case-fold)       ;切换大小写
   ("M-r" . isearch-toggle-regexp)          ;切换正则表达式
   ("M-w" . isearch-toggle-word)            ;切换词
   ("M->" . isearch-beginning-of-buffer)    ;跳转到buffer开头并重新搜索, 搜索最前面一个
   ("M-<" . isearch-end-of-buffer)          ;跳转到buffer末尾并重新搜索, 搜索最后面一个
   ("M-%" . isearch-query-replace)          ;替换
   ("M-d" . isearch-find-duplicate-word)    ;查找重复的单词
   ("M-z" . isearch-find-duplicate-line)    ;查找重复的行
   ("C-M-%" . isearch-query-replace-regexp) ;正则表达式替换
   )
 isearch-mode-map
 )

(use-package ivy
 :ensure t
 :diminish (ivy-mode)
 :config
 (ivy-mode 1)
 (setq ivy-use-virtual-buffers t)
 (setq enable-recursive-minibuffers t)
 (setq ivy-count-format "%d/%d ")
 (setq ivy-display-style 'fancy)

 (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
 (setq ivy-re-builders-alist
       '((counsel-rg . ivy--regex-plus)
         (swiper . ivy--regex-plus)
         (swiper-isearch . ivy--regex-plus)
         (t . ivy--regex-ignore-order)))

 (when (display-graphic-p)    
   (use-package ivy-posframe))
 )

(use-package counsel
  :after ivy
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   ("M-x"     . counsel-M-x)
   ;;("C-x C-f" . counsel-find-file)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package swiper
  :bind
  (
   ("C-x M-s" . swiper)
   ("C-s"     . swiper-isearch)
   ("C-r"     . swiper-isearch)
   ("C-c C-r" . ivy-resume)
   )
  :config
  (progn
    ;;(ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    ;;(setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))
  )

(lazy-load-global-keys
 ' (("C-:"   . avy-goto-char)
    ("C-M-;" . avy-goto-char-2)
    ("M-g l" . avy-goto-line)
    ("M-g w" . avy-goto-word-1)
    ("M-g W" . avy-goto-word-0))
 "avy")
(with-eval-after-load 'avy
  (setq avy-all-windows nil
        avy-all-windows-alt t
        avy-background t
        avy-style 'pre)
  (add-hook 'after-init-hook #'avy-setup-default)


  )

(with-eval-after-load 'avy-zap
  ;; Kill text between the point and the character CHAR
  (lazy-load-global-keys
   '(("M-z" . avy-zap-to-char-dwim)
     ("M-Z" . avy-zap-up-to-char-dwim))
   "avy-zap"
   "C-z")
  )
