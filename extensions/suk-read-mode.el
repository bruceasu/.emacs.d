;;; suk-read-mode.el --- Some enhanced functions for buffer manipulate.
;;; Commentary:
;;
;; Some enhanced functions for buffer manipulate.
;;

;;; Code:
;; --------------------------------------------------------------
;; olivetti
;; --------------------------------------------------------------
(use-package olivetti)
(require 'olivetti)
(define-minor-mode suk-read-mode
  "Minor Mode for better reading experience."
  :init-value nil
  :group suk
  (if suk-read-mode
      (progn
        (and (fboundp 'olivetti-mode) (olivetti-mode 1))
        (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode 1))
        (text-scale-set +1))
    (progn
      (and (fboundp 'olivetti-mode) (olivetti-mode -1))
      (and (fboundp 'mixed-pitch-mode) (mixed-pitch-mode -1))
      (text-scale-set 0))))
(provide 'suk-read-mode)
;;; suk-read-mode.el ends here