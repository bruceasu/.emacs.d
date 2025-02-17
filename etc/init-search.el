(provide 'init-search)

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

 (when (display-graphic-p)    
   (use-package ivy-posframe))
 )

(use-package counsel
  :after ivy
  :ensure t
  :bind
  (
   ;; ("M-y" . counsel-yank-pop)
   ;; ("C-x C-f" . counsel-find-file)
   ("M-x" . counsel-M-x)
  )
)

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
 '(("C-:"   . avy-goto-char)
    ("C-M-;" . avy-goto-char-2)
    ("M-g l" . avy-goto-line)
    ("M-g w" . avy-goto-word-1)
    ("M-g W" . avy-goto-word-0))
 "avy"
 )

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
