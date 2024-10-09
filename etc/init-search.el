;; init-search.el --- Initialize searchser configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Basic configuration.
;;

;;; Code:
;; Search tools

(eval-when-compile
  (require 'init-package)
  )


;; search tools

(require-package 'git-timemachine)
(require-package 'exec-path-from-shell)
(require-package 'findr) ;; a light file search tools.
(require-package 'find-by-pinyin-dired)
(require-package 'jump)


(require-package 'ivy)


(with-eval-after-load 'ivy
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
  (setq ivy-re-builders-alist
	    '((counsel-rg . ivy--regex-plus)
	      (swiper . ivy--regex-plus)
	      (swiper-isearch . ivy--regex-plus)
	      (t . ivy--regex-ignore-order)))
  
  (when (display-graphic-p)
    (require-package 'ivy-posframe)
    (use-package ivy-posframe))
  )
(require-package 'find-file-in-project)
(require-package 'swiper)
(use-package swiper
  :bind
  (
   ("C-x M-s" . swiper)
   ;;("C-x C-f"  . counsel-find-file)
   ;;("M-x" . counsel-M-x)
   )
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    ;;(define-key read-expression-map (kbd "C-r") 'counsel-expression-history))
    ))

;;(require-package 'counsel) ; counsel => swiper => ivy
;;(require-package 'counsel-bbdb)
;;(require-package 'bbdb)
;;(require-package 'counsel-gtags)
;;(require-package 'counsel-css)

;; Jump to Chinese characters
(my-run-with-idle-timer
 1 
 #'(lambda()
	 (require-package 'pinyinlib)
	 (require-package 'ace-pinyin)
	 (use-package ace-pinyin
	   :diminish
	   :hook (after-init . ace-pinyin-global-mode))
     (require-package 'goto-chg)
     (require 'goto-chg)
     (require-package 'avy)
     (with-eval-after-load 'avy
       (setq avy-all-windows nil
             avy-all-windows-alt t
             avy-background t
             avy-style 'pre)
       (add-hook 'after-init-hook #'avy-setup-default)
       (lazy-load-global-keys
        ' (("C-:"   . avy-goto-char)
           ("C-'"   . avy-goto-char-2)
           ("M-g l" . avy-goto-line)
           ("M-g w" . avy-goto-word-1)
           ("M-g e" . avy-goto-word-0))
        "avy")
       (require-package 'avy-zap)
       )

     (with-eval-after-load 'avy-zap
       ;; Kill text between the point and the character CHAR
       (lazy-load-global-keys
        '(("M-z" . avy-zap-to-char-dwim)
          ("M-Z" . avy-zap-up-to-char-dwim))
        "avy-zap")
       )
     (require-package 'anzu)
     (use-package anzu
       :diminish
       :bind (([remap query-replace] . anzu-query-replace)
              ([remap query-replace-regexp] . anzu-query-replace-regexp)
              :map isearch-mode-map
              ([remap isearch-query-replace] . anzu-isearch-query-replace)
              ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
       :hook (after-init . global-anzu-mode))
     
     ;; Redefine M-< and M-> for some modes
     (require-package 'beginend)
     (require 'beginend)
     (add-hook 'after-init-hook #'(beginend-global-mode))
     (mapc (lambda (pair)
             (diminish (cdr pair)))
           beginend-modes)
     ))


;;;###autoload
(defun github-code-search ()
  "Search code on github for a given language."
  (interactive)
  (let ((language (completing-read
                   "Language: "
                   '("Java" "C/C++" "Emacs Javascript" "Lisp"  "Python" "Rust")))
        (code (read-string "Code: ")))
    (browse-url
     (concat "https://github.com/search?l=" language
             "&type=code&q=" code))))

;;;###autoload
(defun google-search-str (str)
  (browse-url
   (concat "https://www.google.com/search?q=" str)))

;;;###autoload
(defun google-search ()
  "Google search region, if active, or ask for search string."
  (interactive)
  (if (region-active-p)
      (google-search-str
       (buffer-substring-no-properties (region-beginning)
                                       (region-end)))
    (google-search-str (read-from-minibuffer "Search: "))))


;; Writable `grep' buffer
(use-package wgrep
  :init
  (setq wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

;; Search tool
(use-package grep
  :ensure nil
  :autoload grep-apply-setting
  :init
  (when (executable-find "rg")
    (grep-apply-setting
     'grep-command "rg --color=auto --null -nH --no-heading -e ")
    (grep-apply-setting
     'grep-template "rg --color=auto --null --no-heading -g '!*/' -e <R> <D>")
    (grep-apply-setting
     'grep-find-command '("rg --color=auto --null -nH --no-heading -e ''" . 38))
    (grep-apply-setting
     'grep-find-template "rg --color=auto --null -nH --no-heading -e <R> <D>")))


;; Fast search tool `ripgrep'
(use-package rg
  :hook (after-init . rg-enable-default-bindings)
  :bind (:map rg-global-map
              ("c" . rg-dwim-current-dir)
              ("f" . rg-dwim-current-file)
              ("m" . rg-menu))
  :init (setq rg-group-result t
              rg-show-columns t)
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases))



(provide 'init-search)
