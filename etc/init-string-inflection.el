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

;;; Code:

(defvar one-key-string-inflection-alist nil
  "The `one-key' menu alist for DIRECTORY.")

(setq one-key-string-inflection-alist
      '(
        (("c" . "FooBar") . (lambda () (interactive) (string-inflection-camelcase)))
        (("l" . "fooBar") . (lambda () (interactive) (string-inflection-lower-camelcase)))
        (("_" . "foo_bar") . (lambda () (interactive) (string-inflection-underscore)))
        (("u" . "FOO_BAR") . (lambda () (interactive) (string-inflection-upcase)))
        (("k" . "foo-bar") . (lambda () (interactive) (string-inflection-kebab-case)))
        (("t" . "foo_bar <=> FooBar") . (lambda () (interactive) (string-inflection-toggle)))
        ))

(defun one-key-string-inflection ()
  "The `one-key' menu for DIRECTORY."
  (interactive)
  (one-key-menu "STRING INFLECTION" one-key-string-inflection-alist t))

(provide 'init-string-inflection)

;;; init-string-inflection.el ends here
