;;; load-set-font.el --- Configure monospaced fonts -*- lexical-binding: t; -*-

;; This file is directly maintained.  README.org loads it but does not tangle it.

;;; Code:

(require 'cl-lib)

(defvar efl
  '("asu-cjk-code"
    "Cascadia Code"
    "JetBrains Mono"
    "Source Code Pro"
    "Consolas"
    "DejaVu Sans Mono"
    "Migu 1M Less"
    "Ubuntu Mono"
    "Victor Mono"
    "Monaco"
    "Courier New")
  "Preferred monospaced fonts for Latin text, in priority order.")

(defvar cfl
  '("asu-cjk-code"
    "Sarasa Mono SC"
    "Sarasa Mono HK"
    "Sarasa Mono TC"
    "Sarasa Mono JP"
    "LXGW WenKai Mono"
    "Migu 1M Less"
    "Noto Sans Mono CJK SC"
    "Noto Sans Mono CJK TC"
    "Noto Sans Mono CJK JP"
    "WenQuanYi Zen Hei Mono")
  "Preferred monospaced fonts for CJK text, in priority order.")

(defvar emacs-font-size-pair-list
  '((12 . 12) (14 . 14) (16 . 16) (18 . 18) (20 . 20)
    (22 . 22) (24 . 24) (26 . 26) (28 . 28) (30 . 30)
    (32 . 32) (36 . 36))
  "Supported font sizes as (LATIN-PIXELS . CJK-PIXELS) pairs.")

(defvar emacs-font-size-pair '(24 . 24)
  "Current font sizes as (LATIN-PIXELS . CJK-PIXELS).")

(defvar en-font nil "Selected monospaced Latin font family.")
(defvar cn-font nil "Selected monospaced CJK font family.")
(defvar emacs-english-font nil "Current monospaced Latin font family.")
(defvar emacs-cjk-font nil "Current monospaced CJK font family.")

(defun suk-font-family-list (&optional frame)
  "Return cached font families available on FRAME."
  (let* ((frame (or frame (selected-frame)))
         (cached (frame-parameter frame 'suk-font-family-cache)))
    (or cached
        (let ((families
               (mapcar #'downcase (font-family-list frame))))
          (set-frame-parameter frame 'suk-font-family-cache families)
          families))))

(defun available-font (font-list &optional frame)
  "Return the first available family in FONT-LIST on FRAME."
  (let ((families (suk-font-family-list frame)))
    (cl-find-if (lambda (font) (member (downcase font) families)) font-list)))

(defun font-exist-p (fontname &optional frame)
  "Return non-nil if FONTNAME is available on FRAME."
  (and (stringp fontname)
       (member (downcase fontname) (suk-font-family-list frame))))

(defun suk-install-fonts ()
  "Install Nerd Icons fonts and refresh cached font-family lists."
  (interactive)
  (unless (require 'nerd-icons nil t)
    (user-error "The nerd-icons package is not available"))
  (nerd-icons-install-fonts)
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'suk-font-family-cache nil)))

(defun suk-font-for-frame (preferred candidates frame)
  "Choose PREFERRED on FRAME, or the first available family in CANDIDATES."
  (or (and (font-exist-p preferred frame) preferred)
      (available-font candidates frame)))

(defun set-font (latin cjk size-pair &optional frame)
  "Set LATIN and CJK fonts on FRAME using pixel SIZE-PAIR."
  (let ((frame (or frame (selected-frame))))
    (when (and (frame-live-p frame) (display-graphic-p frame))
      (when (font-exist-p latin frame)
        ;; Integer `font-spec :size' and CJK `font-spec :size' use pixels.
        (set-frame-font (font-spec :family latin :size (car size-pair))
                        t (list frame))
        ;; Keep the frame font as the source of the family.  Setting a family
        ;; such as "asu-cjk-code" on a face makes Emacs parse its hyphens as
        ;; foundry/family fields and can replace the frame font with a fallback.
        (dolist (face '(fixed-pitch fixed-pitch-serif variable-pitch))
          (set-face-attribute face frame
                              :family 'unspecified
                              :inherit 'default))
        (when (font-exist-p cjk frame)
          (dolist (charset '(han hangul kana cjk-misc bopomofo
                             japanese-jisx0208 japanese-jisx0212
                             katakana-jisx0201))
            (set-fontset-font
             nil charset
             (font-spec :family cjk :size (cdr size-pair))
             frame)))
        (setq emacs-english-font latin
              emacs-cjk-font (and (font-exist-p cjk frame) cjk)
              en-font (or en-font latin)
              cn-font (or cn-font (and (font-exist-p cjk frame) cjk)))
        (set-frame-parameter frame 'suk-font-state
                             (list latin emacs-cjk-font size-pair))
        t))))

(defun suk-apply-frame-font (frame)
  "Apply the configured monospaced fonts to FRAME if needed."
  (when (and (frame-live-p frame) (display-graphic-p frame))
    (let* ((latin (suk-font-for-frame emacs-english-font efl frame))
           (cjk (suk-font-for-frame emacs-cjk-font cfl frame))
           (state (list latin cjk emacs-font-size-pair)))
      (unless (equal state (frame-parameter frame 'suk-font-state))
        (when latin
          (set-font latin cjk emacs-font-size-pair frame)))
      (suk-set-table-face-font frame)))
  frame)

(defun load-default-font ()
  "Apply the configured monospaced fonts to the selected frame."
  (interactive)
  (suk-apply-frame-font (selected-frame)))

(defun load-program-font ()
  "Apply the same monospaced font profile used by other buffers."
  (interactive)
  (load-default-font))

(defface suk-fixed-width-table-face
  '((t (:inherit fixed-pitch)))
  "Face for tables that need fixed Latin-to-CJK cell widths.")

(defun suk-set-table-face-font (&optional frame)
  "Make Org and Markdown table faces inherit the fixed-pitch font on FRAME.

Keep the family unspecified so table text uses FRAME's configured Latin font
and CJK fontset.  Assigning a fallback family directly can be parsed
incorrectly for hyphenated family names and bypass the frame's CJK fallback."
  (let ((frame (or frame (selected-frame))))
    (set-face-attribute 'suk-fixed-width-table-face frame
                        :family 'unspecified
                        :inherit 'fixed-pitch)
    (when (facep 'org-table)
      ;; Org's built-in face specifies colors but does not inherit a fixed
      ;; pitch face.  Keep its colors while inheriting the configured mono font.
      (set-face-attribute 'org-table frame
                          :family 'unspecified
                          :inherit 'fixed-pitch))))

(defun load-org-font ()
  "Keep Org text monospaced and give Org tables their fixed-width font."
  (when (display-graphic-p)
    (let ((frame (selected-frame)))
      (dolist (face '(org-block org-code
                      org-document-title org-document-info
                      org-level-1 org-level-2 org-level-3 org-level-4
                      org-level-5 org-level-6 org-level-7 org-level-8))
        (when (facep face)
          (set-face-attribute face frame :family 'unspecified))))
    (when (fboundp 'variable-pitch-mode)
      (variable-pitch-mode -1))
    (when (fboundp 'mixed-pitch-mode)
      (mixed-pitch-mode -1)))
  (suk-set-table-face-font (selected-frame)))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'load-org-font))

(defun suk-markdown-table-line-matcher (limit)
  "Match Markdown pipe-table rows up to LIMIT."
  (let (matched)
    (while (and (not matched)
                (re-search-forward "^[ \t]*|.*|[ \t]*$" limit t))
      (when (or (not (fboundp 'markdown-table-at-point-p))
                (save-excursion
                  (goto-char (match-beginning 0))
                  (markdown-table-at-point-p)))
        (setq matched t)))
    matched))

(defvar-local suk-markdown-table-font-enabled nil
  "Non-nil when the table font-lock rule is installed in this buffer.")

(defun suk-markdown-setup-table-font ()
  "Use the fixed-width table face for Markdown pipe-table rows."
  (unless suk-markdown-table-font-enabled
    (font-lock-add-keywords
     nil '((suk-markdown-table-line-matcher
            (0 'suk-fixed-width-table-face prepend)))
     'append)
    (setq suk-markdown-table-font-enabled t))
  (suk-set-table-face-font (selected-frame))
  (when (and font-lock-mode (fboundp 'font-lock-flush))
    (font-lock-flush)))

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook #'suk-markdown-setup-table-font))

(defun emacs-step-font-size (step)
  "Move the font size by STEP entries in `emacs-font-size-pair-list'."
  (let* ((sizes (copy-sequence emacs-font-size-pair-list))
         (current (or (cl-position emacs-font-size-pair sizes :test #'equal)
                      0))
         (target (max 0 (min (1- (length sizes)) (+ current step)))))
    (when sizes
      (setq emacs-font-size-pair (nth target sizes))
      (dolist (frame (frame-list))
        (set-frame-parameter frame 'suk-font-state nil)
        (suk-apply-frame-font frame))
      (message "Font size set to %d/%d pixels"
               (car emacs-font-size-pair)
               (cdr emacs-font-size-pair)))))

(defun increase-emacs-font-size ()
  "Increase the font size by one configured step."
  (interactive)
  (emacs-step-font-size 1))

(defun decrease-emacs-font-size ()
  "Decrease the font size by one configured step."
  (interactive)
  (emacs-step-font-size -1))

(when (eq system-type 'windows-nt)
  (global-set-key (kbd "C-M-=") #'increase-emacs-font-size)
  (global-set-key (kbd "C-M--") #'decrease-emacs-font-size))

(add-hook 'prog-mode-hook
          (lambda ()
            (when (fboundp 'variable-pitch-mode)
              (variable-pitch-mode -1))
            (when (fboundp 'mixed-pitch-mode)
              (mixed-pitch-mode -1))))

(add-hook 'after-make-frame-functions #'suk-apply-frame-font)
(if after-init-time
    (load-default-font)
  (add-hook 'after-init-hook #'load-default-font))

(provide 'load-set-font)
;;; load-set-font.el ends here
