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
