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
  (require '+custom)
  (require 'buffer-extension))

;; =========================================================
;; 另外一种解决乱码的办法，就是用命令
;; C-x <RET> r or M-x revert-buffer-with-coding-system or C-x C-m r
;; 来用指定的编码重新读入这个文件。
;; 以DOS格式重读文件（UNIX格式类似）
;; C-x C-m r dos
;;
;; 1. 查看当前 buffer 的编码：M-x describe-coding-system
;; 2. 列出所有编码：C-x <RET> r <TAB>
;; 3. 以指定编码重读当前buffer：C-x <RET> r utf-8，（revert-buffer-with-coding-system）
;; 4. 改变当前buffer的编码：C-x <RET> f utf-8，（set-buffer-file-coding-system）
;; 5. 设定下一步操作的编码格式：C-x <RET> c，（universal-coding-system-argument）
;; =========================================================

;; =========================================================
;; 换行符设置(只是设定保存文件的换行符，并不是用这种换行符重新读取文件)
;; Dos/Unix
;; ---------------------------------------------------------
;;;###autoload
(defun set2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

;;;###autoload
(defun set2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; ---------------------------------------------------------------

;; If you have a mix of lines that end in ^M and lines that don’t,
;; you can use ‘M-x replace-string RET C-q C-m RET’ to remove the
;; ^M’s. ‘C-q’ quotes the key you press right after it, letting you
;; insert a ^M character. Typing ‘C-m’ won’t work – you have to
;; hold down ‘Control’ while hitting ‘q’ followed by ‘m’.
;; ---------------------------------------------------------------
;;;###autoload
(defun dos2unix ()
  "Delete `' characters in current region or buffer.
Same as '`replace-string' `C-q' `C-m' `RET' `RET''."
  (interactive)
  (save-excursion
    (when (region-active-p)
      (narrow-to-region (region-beginning) (region-end)))
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward "\r" nil t)
        (replace-match "" nil t)
        (setq count (1+ count)))
      (message "Removed %d " count))
    (widen))
  ;; 设置当前缓冲区的保存文件格式为 UNIX
  (set-buffer-file-coding-system 'unix 't)
  )

;;;###autoload
(defun unix2dos ()
  "Convert the current buffer from Unix line endings to DOS line
endings."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\r\n" nil t)))
  ;; 设置当前缓冲区的保存文件格式为 DOS
  (set-buffer-file-coding-system 'dos 't))


;; ===============================================================
;; 重新读取文件
;; Revert buffer
;; ---------------------------------------------------------------
;;;###autoload
(defun suk/revert-current-buffer ()
  "Revert the current buffer. key \\[suk/revert-current-buffer]."
  (interactive)
  (unless (minibuffer-window-active-p (selected-window))
    (revert-buffer t t)
    (message "Reverted this buffer")))

;;; =========================================================
;;; 用新编码重新读取文件
;;; ---------------------------------------------------------
;;;###autoload
(defun suk/revert-buffer-no-confirm ()
  "执行`revert-buffer'时不需要确认. key \\[suk/revert-buffer-no-confirm]."
  (interactive)
  (when (buffer-file-name)
    (revert-buffer buffer-file-name t)
    )
  )

;;; ---------------------------------------------------------
;;;###autoload
(defun suk/revert-buffer-with-coding-system-no-confirm (coding-system)
  "Call `revert-buffer-with-coding-system' with CODING-SYSTEM, but when `revert-buffer' do not need confirm."
  (interactive "Coding system for visited file (default nil): ")
  (let ((coding-system-for-read coding-system))
    (suk/revert-buffer-no-confirm)
    (set-buffer-file-coding-system coding-system)
    ))

;;; ---------------------------------------------------------
;;;###autoload
(defun suk/revert-buffer-with-gbk ()
  "Call `revert-buffer-with-coding-system-no-confirm' with gbk.
It is bound to \\[suk/revert-buffer-with-gbk]."
  (interactive)
  (suk/revert-buffer-with-coding-system-no-confirm 'gb18030))

;;; ---------------------------------------------------------
;;;###autoload
(defun suk/revert-buffer-with-utf8 ()
  "Call `revert-buffer-with-coding-system-no-confirm' with utf-8.
It is bound to \\[suk/revert-buffer-with-utf8]."
  (interactive)
  (suk/revert-buffer-with-coding-system-no-confirm 'utf-8))

;;; =========================================================
;;; set the eol to unix format
;;; ---------------------------------------------------------
;;;###autoload
(defun suk/no-junk-please-were-unixish ()
  "只用unix类换行格式.It is bound to \\[suk/no-junk-please-were-unixish]."
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))

(add-hook 'find-file-hook 'suk/no-junk-please-were-unixish)

;; ==============================================================
;; buffers
;; --------------------------------------------------------------
(defvar suk/recently-closed-buffers nil
  "A list of recently closed buffers. Each element is (buffer name, file path). The max number to track is controlled by the variable `suk/recently-closed-buffers-max'.")

(defvar suk/recently-closed-buffers-max 40 "The maximum length for `suk/recently-closed-buffers'.")

;; ---------------------------------------------------------------
;;;###autoload
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
        (minibuffer-keyboard-quit)  ; if the buffer is minibuffer
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
;;;###autoload
(defun suk/open-last-closed ()
  "Open the last closed file."
  (interactive)
  (if (> (length suk/recently-closed-buffers) 0)
      (find-file (cdr (pop suk/recently-closed-buffers)))
    (progn (message "No recently close buffer in this session."))))

;; ---------------------------------------------------------------
;;;###autoload
(defun suk/open-recently-closed ()
  "Open recently closed file.Prompt for a choice."
  (interactive)
  (find-file (ido-completing-read "open:" (mapcar (lambda (f) (cdr f)) suk/recently-closed-buffers))))

;; ---------------------------------------------------------------
;; List the recently closed files.
;; ---------------------------------------------------------------
;;;###autoload
(defun suk/list-recently-closed ()
  "List recently closed file."
  (interactive)
  (let (($buf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer $buf)
    (mapc (lambda ($f) (insert (cdr $f) "\n"))
          suk/recently-closed-buffers)))

;;;###autoload
(defun suk/kill-current-mode-buffers ()
  "Kill all buffers that major mode same with current mode."
  (interactive)
  (suk/kill-special-mode-buffers-internal major-mode))

;;;###autoload
(defun suk/kill-current-mode-buffers-except-current ()
  "Kill all buffers that major mode same with current mode.
And don't kill current buffer."
  (interactive)
  (kill-special-mode-buffers-internal major-mode t))

;;;###autoload
(defun suk/kill-special-mode-buffers ()
  "Kill all buffers that major mode that user given."
  (interactive)
  (let (mode-list)
    (dolist (element (buffer-list))
      (set-buffer element)
      (unless (member (symbol-name major-mode) mode-list)
        (add-to-ordered-list 'mode-list (symbol-name major-mode))))
    (kill-special-mode-buffers-internal (intern-soft (completing-read "Mode: " mode-list)))))

;;;###autoload
(defun suk/kill-special-mode-buffers-internal (mode &optional except-current-buffer)
  "Kill all buffers that major MODE same with special.
If option EXCEPT-CURRENT-BUFFER is `non-nil',
kill all buffers with MODE except current buffer."
  (interactive)
  (let ((current-buf (current-buffer))
        (count 0))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (and (equal major-mode mode)
                 (or (not except-current-buffer)
                     (not (eq current-buf buffer))))
        (cl-incf count)
        (kill-buffer buffer)))
    (message "Killed %s buffer%s" count (if (> count 1) "s" ""))))

;;;###autoload
(defun suk/kill-all-buffers-except-current ()
  "Kill all buffers except current buffer.  key \\[suk/kill-all-buffers-except-current]."
  (interactive)
  (let ((current-buf (current-buffer)))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (unless (eq current-buf buffer)
        (kill-buffer buffer)))))

;;;###autoload
(defun suk/kill-other-window-buffer ()
  "Kill the buffer in other window.  key \\[suk/kill-other-window-buffer]."
  (interactive)
  (other-window +1)
  (kill-this-buffer)
  (other-window -1))

;;;###autoload
(defun suk/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; --------------------------------------------------------------
;; new empty buffer
;; --------------------------------------------------------------
;;;###autoload
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
;; Rename
;; --------------------------------------------------------------
;;;###autoload
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;; --------------------------------------------------------------
;; move
;; --------------------------------------------------------------
;;;###autoload
(defun suk/move-buffer-file (dir)
  "Move both current buffer and file it's visiting to DIR."
  (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (copy-file filename newname 1)
      (delete-file filename)
      (set-visited-file-name newname)
      (set-buffer-modified-p nil)
      t)))

;;;###autoload
(defun suk/browse-this-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))


;;;###autoload
(defun copy-file-name ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (if filename
        (progn
          (kill-new filename)
          (message "Copied '%s'" filename))
      (warn "Current buffer is not attached to a file!"))))

;; --------------------------------------------------------------
;; Reload
;; --------------------------------------------------------------
;;;###autoload
(defun suk/reload-emacs-configuration ()
  "Reload emacs initial configured file init.el."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; --------------------------------------------------------------
;; Open custom file
;; --------------------------------------------------------------
;;;###autoload
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
;;;###autoload
(defun suk/create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))


;;;###autoload
(defun suk/create-scratch-org ()
  "Create a scratch buffer with org-mode"
  (interactive)
  (find-file "/tmp/scratch.org")
  (gnus-make-directory "/tmp"))
(global-set-key "\C-cs" 'suk/create-scratch-org) ;; Bind to `C-c s'

;;;###autoload
(defun suk/switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (if (active-minibuffer-window)
      (select-window (active-minibuffer-window))
    (error "Minibuffer is not active")))
(global-set-key "\C-co" 'suk/switch-to-minibuffer) ;; Bind to `C-c o'


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

;; 显示当前buffer或region或函数的行数和字符数
;; --------------------------------------------------------------
;;;###autoload
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
		  mark-active
        (message "当前region内共有%d行, %d个字符" (count-lines min max) (- max min))
        (message "当前buffer内共有%d行, %d个字符" (count-lines min max) (- max min)))
      (let ((nmin min) (nmax max))
        (save-excursion
          (save-restriction
            (widen)
            (setq min (point-min))
            (setq max (point-max))))
        (message "narrow下buffer内共有%d行, %d个字符, 非narrow下buffer内共有%d行, %d个字符"
                 (count-lines nmin nmax) (- nmax nmin) (count-lines min max) (- max min))))))


;; =========================================================
;;;###autoload
(defun suk/indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;;(global-set-key [S-f7] 'indent-buffer)

;;;###autoload
(defun suk/xah-narrow-to-region ()
  "Same as `narrow-to-region', but if no selection, narrow to the current block.
Version 2022-01-22"
  (interactive)
  (if (region-active-p)
      (progn
        (narrow-to-region (region-beginning) (region-end)))
    (progn
      (let ($p1 $p2)
        (save-excursion
          (if (re-search-backward "\n[ \t]*\n" nil "move")
              (progn (goto-char (match-end 0))
                     (setq $p1 (point)))
            (setq $p1 (point)))
          (if (re-search-forward "\n[ \t]*\n" nil "move")
              (progn (goto-char (match-beginning 0))
                     (setq $p2 (point)))
            (setq $p2 (point))))
        (narrow-to-region $p1 $p2)))))

;; =========================================================
;; 段落格式化
;; ---------------------------------------------------------
;;;###autoload
(defun suk/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph (or REGION) and make it into a single line of text."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list t)))
  (let ((fill-column (point-max)))
    (fill-paragraph nil region)))

;;(bind-key "M-Q" 'suk/unfill-paragraph)

;; M-q will fill the paragraph normally, and C-u M-q will unfill it.
;; --------------------------------------------------------------
;;;###autoload
(defun suk/fill-or-unfill-paragraph (&optional unfill region)
  "Fill paragraph (or REGION).
With the prefix argument UNFILL, unfill it instead."
  (interactive (progn
                 (barf-if-buffer-read-only)
                 (list (if current-prefix-arg 'unfill) t)))
  (let ((fill-column (if unfill (point-max) fill-column)))
    (fill-paragraph nil region)))
;;(bind-key "M-q" 'suk/fill-or-unfill-paragraph)

;; =========================================================
;; 方便的切换major mode
;; ---------------------------------------------------------
(defvar suk/switch-major-mode-last-mode nil)
;; ---------------------------------------------------------
;;;###autoload
(defun suk/major-mode-heuristic (symbol)
  (and (fboundp symbol)
       (string-match ".*-mode$" (symbol-name symbol))))
;; ---------------------------------------------------------
;;;###autoload
(defun suk/switch-major-mode (mode)
  "Change major mode to MODE."
  (interactive
   (let ((fn suk/switch-major-mode-last-mode) val)
     (setq val
           (completing-read
            (if fn (format "Change major-mode(default:%s): " fn) "Change major mode: ")
            obarray 'suk/major-mode-heuristic t nil nil (symbol-name fn)))
     (list (intern val))))
  (let ((last-mode major-mode))
    (funcall mode)
    (setq suk/switch-major-mode-last-mode last-mode)
    (message "Change to %s." major-mode))
  )
;; ---------------------------------------------------------
;; show major mode
;;;###autoload
(defun suk/get-mode-name ()
  "Show `major-mode' and `mode-name'."
  (interactive)
  (message "major-mode:%s, mode-name:%s" major-mode mode-name))

;;;###autoload
(defun suk/toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0)))

(suk-set-key-bindings 'global-set-key
                      (list
                       ;;(list (kbd "C-x b l") #'suk/count-brf-lines)
                       (list (kbd "C-x n x") #'suk/xah-narrow-to-region) ; C-x n w ,  C-x n d, C-x n n, C-x n p 系列
                       (list (kbd "C-x x x") #'suk/switch-major-mode)
                       (list (kbd "C-x x X") #'suk/get-mode-name)
                       (list (kbd "C-x x n") #'suk/new-empty-buffer)
                       (list (kbd "C-x x s") #'suk/create-scratch-buffer)
                       (list (kbd "C-x x o") #'suk/create-scratch-org)
                       (list (kbd "C-x x m") #'suk/switch-to-minibuffer)
                       (list (kbd "C-x x c") #'copy-buffer-file-name-as-kill)
                       (list (kbd "C-x x t") #'suk/toggle-margin-right)
                       (list (kbd "C-x x q") #'suk/fill-or-unfill-paragraph)
                       (list (kbd "C-x x Q") #'suk/unfill-paragraph)
                       (list (kbd "C-x k")   #'suk/close-current-buffer)
                       (list (kbd "C-x C-k")   #'suk/kill-all-buffers-except-current)
					   (list (kbd "C-x K")  #'suk/kill-other-window-buffer) ;关闭其他窗口的
                       (list (kbd "C-x x u") #'suk/revert-buffer-with-utf8)
                       (list (kbd "C-x x g") #'suk/revert-buffer-with-gbk)
                       ;;'([C-t]               transpose-chars)
                       ;;'([S-f6]              hs-minor-mode)
                       ;;'([S-f5]              toggle-truncate-lines)
                       (list (kbd "C-S-t") #'suk/open-last-closed)
                       (list (kbd "C-x R") #'recentf-open)
                       (list (kbd "C-S-<f6>") #'suk/move-buffer-file)
                       (list (kbd "C-F")  #'suk/indent-buffer)
                       (list (kbd "C-S-<f2>")  #'suk/rename-file-and-buffer)
                       ))



(provide 'init-buffers)
;;; init-buffers.el ends here
