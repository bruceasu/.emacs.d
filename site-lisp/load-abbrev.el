;;; Commentary:
;;; my abbrev table

;;; code:

;; sample use of emacs abbreviation feature

(define-abbrev-table 'global-abbrev-table '(

    ;; math/unicode symbols
    ("8in" "∈")
    ("8nin" "∉")
    ("8inf" "∞")
    ("8love" "♥")
    ("8smily" "☺")

    ;; email
    ("8me" "sukhonzeon@gmail.com")

    ;; computing tech
    ("8ms" "Microsoft")
    ("8g" "Google")
    ("8ie" "Internet Explorer")


    ;; normal english words
    ("8alt" "alternative")
    ("8char" "character")
    ("8def" "definition")
    ("8bg" "background")
    ("8kb" "keyboard")
    ("8ex" "example")
    ("8kbd" "keybinding")
    ("8env" "environment")
    ("8var" "variable")
    ("8ev" "environment variable")
    ("8cp" "computer")

    ;; signature
    ("8suk" "Suk Honzeon")


    ;; emacs regex
    ("8d" "\\([0-9]+?\\)")
    ("8str" "\\([^\"]+?\\)\"")

    ;; shell commands
    ("8ff" "firefox ")

    ("8f0" "find . -type f -size 0 -exec rm {} ';'")
    
    ))

;; stop asking whether to save newly added abbrev when quitting emacs
;; (setq save-abbrevs nil)

;; turn on abbrev mode globally
;; 简写模式
(setq-default abbrev-mode t)
(setq save-abbrevs nil)
(setq abbrev-file-name "~/.emacs.d/share/abbrev_definitions")

(provide 'load-abbrev)
;; load-abbrev.el ends here

