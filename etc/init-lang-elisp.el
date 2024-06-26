;; init-elisp.el --- Initialize Emacs Lisp configurations.	-*- lexical-binding: t -*-

(defun my-show-scratch-buffer-message ()
  "Show something in scratch buffer."
  (let* ((fortune-prog (or (executable-find "fortune-zh")
                           (executable-find "fortune"))))
    (cond
     (fortune-prog
      (format
       ";; %s\n\n"
       (replace-regexp-in-string
        "\n" "\n;; " ; comment each line
        (replace-regexp-in-string
         "\\(\n$\\|\\|\\[m *\\|\\[[0-9][0-9];?[0-9]?m *\\|\\[;m\\)" ""    ; remove trailing line break
         (shell-command-to-string fortune-prog)))))
     (t
      (concat ";; Happy hacking "
              (or user-login-name "")
              " - Emacs loves you!\n\n")))))

(setq-default initial-scratch-message (my-show-scratch-buffer-message))
;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)

(with-eval-after-load 'paredit
  (diminish 'paredit-mode " Par"))
(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(defun my-lisp-setup ()
  "Enable features useful in any Lisp mode."
  (enable-paredit-mode)
  (rainbow-delimiters-mode t)
  (turn-on-eldoc-mode))

(let* ((hooks '(lisp-mode-hook
                inferior-lisp-mode-hook)))
  (dolist (hook hooks)
    (add-hook hook 'my-lisp-setup)))

(defun elisp-mode-hook-setup ()
  (unless (my-buffer-file-temp-p)
    (my-ensure 'eldoc)
    (turn-on-eldoc-mode)
    (enable-paredit-mode)
    (rainbow-delimiters-mode t)

    ;; Locally set `hippie-expand' completion functions for use with Emacs Lisp.
    (make-local-variable 'hippie-expand-try-functions-list)
    (push 'try-complete-lisp-symbol hippie-expand-try-functions-list)
    (push 'try-complete-lisp-symbol-partially hippie-expand-try-functions-list)

    (checkdoc-minor-mode 1)))

(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook-setup)

;; Emacs lisp mode
(use-package elisp-mode
  :ensure nil
  :bind (:map emacs-lisp-mode-map
         ("C-c C-x" . ielm)
         ("C-c C-c" . eval-defun)
         ("C-c C-b" . eval-buffer))
  :config
  (with-no-warnings
    ;; Align indent keywords
    ;; @see https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
    (defun my-lisp-indent-function (indent-point state)
      "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
      (let ((normal-indent (current-column))
            (orig-point (point)))
        (goto-char (1+ (elt state 1)))
        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
        (cond
         ;; car of form doesn't seem to be a symbol, or is a keyword
         ((and (elt state 2)
               (or (not (looking-at "\\sw\\|\\s_"))
                   (looking-at ":")))
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
         ((and (save-excursion
                 (goto-char indent-point)
                 (skip-syntax-forward " ")
                 (not (looking-at ":")))
               (save-excursion
                 (goto-char orig-point)
                 (looking-at ":")))
          (save-excursion
            (goto-char (+ 2 (elt state 1)))
            (current-column)))
         (t
          (let ((function (buffer-substring (point)
                                            (progn (forward-sexp 1) (point))))
                method)
            (setq method (or (function-get (intern-soft function)
                                           'lisp-indent-function)
                             (get (intern-soft function) 'lisp-indent-hook)))
            (cond ((or (eq method 'defun)
                       (and (null method)
                            (length> function 3)
                            (string-match "\\`def" function)))
                   (lisp-indent-defform state indent-point))
                  ((integerp method)
                   (lisp-indent-specform method state
                                         indent-point normal-indent))
                  (method
                   (funcall method indent-point state))))))))
    (add-hook 'emacs-lisp-mode-hook
              (lambda () (setq-local lisp-indent-function #'my-lisp-indent-function)))

    ;; Add remove buttons for advices
    (add-hook 'help-mode-hook 'cursor-sensor-mode)

    (defun function-advices (function)
      "Return FUNCTION's advices."
      (let ((flist (indirect-function function)) advices)
        (while (advice--p flist)
          (setq advices `(,@advices ,(advice--car flist)))
          (setq flist (advice--cdr flist)))
        advices))

    (defun add-remove-advice-button (advice function)
      (when (and (functionp advice) (functionp function))
        (let ((inhibit-read-only t)
              (msg (format "Remove advice `%s'" advice)))
          (insert "\t")
          (insert-button
           "Remove"
           'face 'custom-button
           'cursor-sensor-functions `((lambda (&rest _) ,msg))
           'help-echo msg
           'action (lambda (_)
                     (when (yes-or-no-p msg)
                       (message "%s from function `%s'" msg function)
                       (advice-remove function advice)
                       (if (eq major-mode 'helpful-mode)
                           (helpful-update)
                         (revert-buffer nil t))))
           'follow-link t))))

    (defun add-button-to-remove-advice (buffer-or-name function)
      "Add a button to remove advice."
      (with-current-buffer buffer-or-name
        (save-excursion
          (goto-char (point-min))
          (let ((ad-list (function-advices function)))
            (while (re-search-forward "^\\(?:This function has \\)?:[-a-z]+ advice: \\(.+\\)$" nil t)
              (let ((advice (car ad-list)))
                (add-remove-advice-button advice function)
                (setq ad-list (delq advice ad-list))))))))

    (define-advice describe-function-1 (:after (function) advice-remove-button)
      (add-button-to-remove-advice (help-buffer) function))
    (with-eval-after-load 'helpful
      (define-advice helpful-update (:after () advice-remove-button)
        (when helpful--callable-p
          (add-button-to-remove-advice (current-buffer) helpful--sym))))

    ;; Remove hooks
    (defun remove-hook-at-point ()
      "Remove the hook at the point in the *Help* buffer."
      (interactive)
      (unless (memq major-mode '(help-mode helpful-mode))
        (error "Only for help-mode or helpful-mode"))

      (let ((orig-point (point)))
        (save-excursion
          (when-let
              ((hook (progn (goto-char (point-min)) (symbol-at-point)))
               (func (when (and
                            (or (re-search-forward (format "^Value:?[\s|\n]") nil t)
                                (goto-char orig-point))
                            (sexp-at-point))
                       (end-of-sexp)
                       (backward-char 1)
                       (catch 'break
                         (while t
                           (condition-case _err
                               (backward-sexp)
                             (scan-error (throw 'break nil)))
                           (let ((bounds (bounds-of-thing-at-point 'sexp)))
                             (when (<= (car bounds) orig-point (cdr bounds))
                               (throw 'break (sexp-at-point)))))))))
            (when (yes-or-no-p (format "Remove %s from %s? " func hook))
              (remove-hook hook func)
              (if (eq major-mode 'helpful-mode)
                  (helpful-update)
                (revert-buffer nil t)))))))
    (bind-key "r" #'remove-hook-at-point help-mode-map)))

;; Interactive macro expander
(use-package macrostep
  :bind (:map emacs-lisp-mode-map
         ("C-c e" . macrostep-expand)
         :map lisp-interaction-mode-map
         ("C-c e" . macrostep-expand)))


(provide 'init-lang-elisp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-elisp.el ends here
