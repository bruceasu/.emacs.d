;;; init-buffers.el --- Initialize buffers configurations. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Suk

;; Author: Suk

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; buffers configuration.
;;
;;; Code:

(eval-when-compile
  (require '+const)
  (require '+custom))

;; ==============================================================
;; buffers
;; --------------------------------------------------------------
(defvar suk/recently-closed-buffers nil
  "A list of recently closed buffers. Each element is (buffer name, file path). The max number to track is controlled by the variable `suk/recently-closed-buffers-max'.")

(defvar suk/recently-closed-buffers-max 40 "The maximum length for `suk/recently-closed-buffers'.")

;; ---------------------------------------------------------------
(defun suk/close-current-buffer ()
  "Close the current buffer.

Similar to `kill-buffer', with the following addition:

• Prompt user to save if the buffer has been modified even if the
buffer is not associated with a file.

• If the buffer is editing a source file in an 'org-mode' file,
prompt the user to save before closing.

• If the buffer is a file, add the path to the list
`suk/recently-closed-buffers'.

• If it is the minibuffer, exit the minibuffer"
  (interactive)
  (let ($emacs-buff-p
        ($org-p (string-match "^*Org Src" (buffer-name))))

    (setq $emacs-buff-p (if (string-match "^*" (buffer-name)) t nil))

    (if (string= major-mode "minibuffer-inactive-mode")
        (minibuffer-keyboard-quit) ; if the buffer is minibuffer
      (progn
        ;; offer to save buffers that are non-empty and modified, even for
        ;; non-file visiting buffer. (because kill-buffer does not offer to save
        ;; buffers that are not associated with files)
        (when (and (buffer-modified-p)
                   (not $emacs-buff-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        (when (and (buffer-modified-p)
                   $org-p)
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (org-edit-src-save)
            (set-buffer-modified-p nil)))

        ;; save to a list of closed buffer
        (when (buffer-file-name)
          (setq suk/recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name)) suk/recently-closed-buffers))
          (when (> (length suk/recently-closed-buffers) suk/recently-closed-buffers-max)
            (setq suk/recently-closed-buffers (butlast suk/recently-closed-buffers 1))))

        ;; close
        (kill-buffer (current-buffer))))))

;; ---------------------------------------------------------------
(defun suk/open-last-closed ()
  "Open the last closed file."
  (interactive)
  (if (> (length suk/recently-closed-buffers) 0)
      (find-file (cdr (pop suk/recently-closed-buffers)))
    (progn (message "No recently close buffer in this session."))))

;; ---------------------------------------------------------------
(defun suk/open-recently-closed ()
  "Open recently closed file.Prompt for a choice."
  (interactive)
  (find-file (ido-completing-read "open:" (mapcar (lambda (f) (cdr f)) suk/recently-closed-buffers))))

;; ---------------------------------------------------------------
;; List the recently closed files.
;; ---------------------------------------------------------------
(defun suk/list-recently-closed ()
  "List recently closed file."
  (interactive)
  (let (($buf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer $buf)
    (mapc (lambda ($f) (insert (cdr $f) "\n"))
          suk/recently-closed-buffers)))


;; --------------------------------------------------------------
;; new empty buffer
;; --------------------------------------------------------------
(defun suk/new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.
It returns the buffer."
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
;;    (funcall initial-major-mode)
    (text-mode)
    (setq buffer-offer-save t)
    $buf
    ))

;; --------------------------------------------------------------
;; Reload
;; --------------------------------------------------------------
(defun suk/reload-emacs-configuration ()
  "Reload emacs initial configured file init.el."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; --------------------------------------------------------------
;; Open custom file
;; --------------------------------------------------------------
(defun open-custom-file()
  "Open custom.el if exists, otherwise create it."
  (interactive)
  (let ((custom-example
         (expand-file-name "custom-example.el" user-emacs-directory)))
    (unless (file-exists-p custom-file)
      (if (file-exists-p custom-example)
          (copy-file custom-file)
        (error "Unable to find \"%s\"" custom-example)))
    (find-file custom-file)))

;; --------------------------------------------------------------
;; Create a new scratch buffer
;; --------------------------------------------------------------
(defun suk/create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))
(global-set-key "\C-cs" 'suk/create-scratch-buffer) ;; Bind to `C-c s'

(defun suk/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))
(global-set-key "\C-co" 'suk/switch-to-minibuffer) ;; Bind to `C-c o'

;; 显示当前buffer或region或函数的行数和字符数
;; --------------------------------------------------------------
(defun suk/count-brf-lines (&optional is-fun)
  "显示当前buffer或region或函数的行数和字符数."
  (interactive "P")
  (let (min max)
    (if is-fun
        (save-excursion
          (beginning-of-defun) (setq min (point))
          (end-of-defun) (setq max (point))
          (message "当前函数%s内共有%d行, %d个字符" (which-function) (count-lines min max) (- max min)))
      (if mark-active
          (progn
            (setq min (min (point) (mark)))
            (setq max (max (point) (mark))))
        (setq min (point-min))
        (setq max (point-max)))
      (if (or (= 1 (point-min)) mark-active)
          (if mark-active
              (message "当前region内共有%d行, %d个字符" (count-lines min max) (- max min))
            (message "当前buffer内共有%d行, %d个字符" (count-lines min max) (- max min)))
        (let ((nmin min) (nmax max))
          (save-excursion
            (save-restriction
              (widen)
              (setq min (point-min))
              (setq max (point-max))))
          (message "narrow下buffer内共有%d行, %d个字符, 非narrow下buffer内共有%d行, %d个字符"
                   (count-lines nmin nmax) (- nmax nmin) (count-lines min max) (- max min)))))))

;; =========================================================
;; Browse the homepage
(defun suk/browse-homepage ()
  "Browse the Github page of Suk Emacs."
  (interactive)
  (browse-url suk-homepage))


(global-set-key (kbd "C-x k") 'suk/close-current-buffer)
(global-set-key (kbd "C-S-t") 'suk/open-last-closed) ; control+shift+t
(global-set-key [S-f2]  'suk/new-empty-buffer)

(provide 'init-buffers)
;;; init-buffers.el ends here
