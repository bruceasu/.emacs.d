;;; init-string-inflection.el --- Config for string-inflection.el
;;; Commentary:
;;
;; Config for string-inflection.el
;;

;;; Installation:
;;
;; Put init-string-inflection.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-string-inflection)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-string-inflection RET
;;

;;; Change log:
;;
;; 2018/06/12
;;      * First released.
;;

;;; Acknowledgements:
;;
;;
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'string-inflection)

;; Handling capitalized subwords in a nomenclature
(run-with-idle-timer
    2 nil
    #'(lambda()
       (require-package 'subword) ;Handling capitalized subwords in a nomenclature
       (require 'subword)
       (add-hook 'prog-mode-hook #'subword-mode)
       (add-hook 'mimibuffer-setup #'subword-mode)))

(with-eval-after-load 'hydra
  (defhydra string-inflection-hydra (:color blue :hint nil)
    "
Describe Something: (q to quit)
_c_ camelcase FooBar
_l_ lower-camelcase fooBar
___ underscore foo_bar
_u_ upcase FOO_BAR
_k_ kebab-case foo-bar
_t_ toggle foo_bar <=> FooBar
"
    ("c" string-inflection-camelcase)
    ("l" string-inflection-lower-camelcase)
    ("_" string-inflection-underscore)
    ("u" string-inflection-upcase)
    ("k" string-inflection-kebab-case)
    ("t" string-inflection-toggle))
  ;; set key in init-keys.el
  ;; (global-set-key (kbd "C-c C-u") 'string-inflection-hydra/body)
  )
  
(provide 'init-string-inflection)