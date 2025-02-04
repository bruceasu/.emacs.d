

(defun upcase-char (arg)
  "Uppercase for character."
  (interactive "P")
  (upcase-region (point) (+ (point) (or arg 1)))
  (forward-char (or arg 1)))

(defun downcase-char (arg)
  "Downcase for character."
  (interactive "P")
  (downcase-region (point) (+ (point) (or arg 1)))
  (forward-char (or arg 1)))

;; these bindings are fine
(global-set-key (kbd "M-l") 'downcase-char)
(global-set-key (kbd "M-u") 'upcase-char)
(provide 'cases)
