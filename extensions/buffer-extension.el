;;; buffer-extension.el --- Some enhanced functions for buffer manipulate.

;;; Commentary:
;;
;; Some enhanced functions for buffer manipulate.
;;

;;; Installation:
;;
;; Put buffer-extension.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'buffer-extension)
;;
;; No need more.

;;; Change log:
;;
;; 2009/02/07
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

(require 'basic-toolkit)
(require 'leader-key)

;;; Code:

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
(defun set2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

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
;;; --- EOL conversion helpers (content + saving format) ---

(defun suk/dos2unix (&optional beg end)
  "将当前区域或缓冲区内的 CR 去掉（把 CRLF 变成 LF），并把保存格式设为 UNIX。
如果有选区，只处理选区；否则处理整个缓冲区。"
  (interactive)
  (save-excursion
    (save-restriction
      (when (and (region-active-p) (not (use-region-p))) ; 兼容旧配置
        (activate-mark))
      (when (use-region-p)
        (narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (let ((count 0))
        (while (search-forward "\r" nil t)
          (replace-match "" nil t)
          (setq count (1+ count)))
        (message "Removed %d CR (^M) chars" count))
      (widen)))
  ;; 设置保存为 UNIX（LF）
  (set-buffer-file-coding-system 'unix t))

(defun suk/unix2dos ()
  "将当前缓冲区的 **保存格式** 设为 DOS（CRLF）。不要手工插入 CR。"
  (interactive)
  ;; 若缓冲区中残留了文本形式的 CR（很少见），先清理一下更稳妥：
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match "" nil t)))
  (set-buffer-file-coding-system 'dos t)
  (message "Buffer will be saved with DOS (CRLF) line endings"))

(defun suk/toggle-eol ()
  "在 DOS (CRLF) 与 UNIX (LF) 行尾之间切换，并同步转换内容。"
  (interactive)
  (if (string-match-p "dos\\'" (symbol-name buffer-file-coding-system))
      ;; 当前是 DOS → 转成 UNIX
      (progn
        (suk/dos2unix)
        (message "Converted to UNIX (LF)"))
    ;; 当前是 UNIX → 转成 DOS
    (progn
      (suk/unix2dos)
      (message "Converted to DOS (CRLF)"))))

(global-set-key (kbd "C-c t e") #'suk/toggle-eol)


;; 直接用内置 M-x revert-this-buffer（Emacs 28+）即可“一键刷新当前 buffer”。
;; 需要换编码重读 → M-x revert-buffer-with-coding-system。
;;; =========================================================
;;; 快速以指定编码重新读取当前文件（无确认）
;;; - 带前缀参数 C-u 同时把“之后保存编码”设为同一编码
;;; ---------------------------------------------------------

(defun suk/revert-with-coding (coding &optional set-for-save)
  "用 CODING 重新读取当前文件（不询问）。若 SET-FOR-SAVE 非 nil，也把保存编码设为 CODING。"
  (interactive (list (read-coding-system "Revert using coding: ") current-prefix-arg))
  (unless (buffer-file-name)
    (user-error "当前 buffer 不是文件"))
  (let ((coding-system-for-read coding))
    (revert-buffer t t))                   ; 忽略autosave & 不确认
  (when set-for-save
    (set-buffer-file-coding-system coding t))
  (message "Reverted with %s%s" coding (if set-for-save ", and set as save coding" "")))

;; --- 直达 UTF-8 / GB18030 / MS932（cp932） ---
(defun suk/revert-utf8 (&optional set-for-save)
  "用 UTF-8 重读当前文件。C-u 同时设为保存编码。"
  (interactive "P")
  (suk/revert-with-coding 'utf-8 set-for-save))

(defun suk/revert-gb18030 (&optional set-for-save)
  "用 GB18030 重读当前文件。C-u 同时设为保存编码。"
  (interactive "P")
  (suk/revert-with-coding 'gb18030 set-for-save))

(defun suk/revert-ms932 (&optional set-for-save)
  "用 MS932(=cp932/windows-31j) 重读当前文件。C-u 同时设为保存编码。"
  (interactive "P")
  (suk/revert-with-coding 'cp932 set-for-save))   ; Emacs 的编码名是 cp932

;;; =========================================================
;;; 自动识别（优先尝试 Unicode，再尝试常见 CJK 编码）
;;; - 尝试顺序可按需调整
;;; - 成功即停，失败则报错
;;; ---------------------------------------------------------
(defun suk/revert-auto-unicode-cjk (&optional set-for-save)
  "按常见 Unicode/CJK 编码顺序重读：UTF-8/UTF-16(含BOM)/UTF-16LE/BE → CP932 → GB18030 → GBK → BIG5。
C-u 同时把保存编码设为成功使用的编码。"
  (interactive "P")
  (unless (buffer-file-name)
    (user-error "当前 buffer 不是文件"))
  (let* ((candidates '(utf-8
                       utf-16           ; 含 BOM 自动判别端序
                       utf-16le utf-16be
                       cp932            ; MS932 / Windows-31J
                       gb18030 gbk
                       big5))
         (done nil)
         (used nil))
    (dolist (c candidates)
      (unless done
        (condition-case _
            (progn
              (let ((coding-system-for-read c))
                (revert-buffer t t))
              (setq done t
                    used c))
          (error nil))))
    (unless done)
    (if used
        (progn
          (when set-for-save
            (set-buffer-file-coding-system used t))
          (message "Auto-revert OK with %s%s"
                   used (if set-for-save ", and set as save coding" "")))
      (user-error "给定候选编码都无法成功重读。可手动试试 M-x revert-buffer-with-coding-system"))))

;;; 可选：常用快捷键（按需改）
(global-set-key (kbd "C-c r a") #'suk/revert-auto-unicode-cjk)


;; ==============================================================
;; buffers
;; --------------------------------------------------------------
(defvar suk/recently-closed-buffers nil
  "A list of recently closed buffers. Each element is (buffer name, file path).
   The max number to track is controlled by the variable `suk/recently-closed-buffers-max'.")

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


;; ---------------------------------------------------------------
;; Kill all buffers except current buffer.
;; ---------------------------------------------------------------
(defun suk/kill-all-buffers-except-current ()
  "Kill all buffers except current buffer.  key \\[suk/kill-all-buffers-except-current]."
  (interactive)
  (let ((current-buf (current-buffer)))
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (unless (eq current-buf buffer)
        (kill-buffer buffer)))))


;; ---------------------------------------------------------------
;; Kill the buffer in other window. 
;; ---------------------------------------------------------------
(defun suk/kill-other-window-buffer ()
  "Kill the buffer in other window.  key \\[suk/kill-other-window-buffer]."
  (interactive)
  (other-window +1)
  (kill-this-buffer)
  (other-window -1))

;; ---------------------------------------------------------------
;; Delete the current file, and kill the buffer.
;; ---------------------------------------------------------------
(defun suk/delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))
;;; --------------------------------------------------------------
;;; Unified rename/move current buffer's file with fallback
;;; --------------------------------------------------------------

(defun suk--rename-or-move (new-path &optional ok-if-exists)
  "Rename/move current buffer's visiting file to NEW-PATH.
Prefer `rename-file'; if it fails (e.g. cross-device), fallback to copy+delete.
When OK-IF-EXISTS is non-nil, allow overwriting."
  (unless (buffer-file-name)
    (user-error "当前 buffer 没有关联文件"))
  (let* ((old (buffer-file-name))
         (dst (expand-file-name new-path))
         (dst-dir (file-name-directory dst)))
    ;; 确保目标目录存在
    (unless (file-directory-p dst-dir)
      (make-directory dst-dir t))
    (condition-case err
        (progn
          ;; 尝试直接重命名（更快）
          (rename-file old dst ok-if-exists)
          (message "Renamed: %s → %s" old dst))
      (file-error
       ;; rename 失败（多见于跨设备），fallback：copy + delete
       (progn
         (when (and (file-exists-p dst) (not ok-if-exists))
           (user-error "目标已存在：%s（使用前缀参数覆盖或改名）" dst))
         (copy-file old dst t)     ; t = ok-if-exists 覆盖
         (delete-file old)
         (message "Moved by copy+delete: %s → %s" old dst)))))
  ;; 同步 buffer → 新文件
  (set-visited-file-name (expand-file-name new-path) t t)
  (rename-buffer (file-name-nondirectory (buffer-file-name)) t)
  (set-buffer-modified-p nil)
  (message "Buffer now visiting: %s" (buffer-file-name))
  (buffer-file-name))

;;; 交互式：输入“新完整路径”（可改名，也可换目录）
(defun suk/rename-or-move-file-here (new-path &optional overwrite)
  "把当前文件重命名/移动到 NEW-PATH。
带前缀参数时允许覆盖已存在的同名文件。"
  (interactive
   (let* ((old (or (buffer-file-name) (user-error "当前 buffer 没有关联文件")))
          (prompt (format "新路径（默认目录：%s）: " (file-name-directory old))))
     (list (read-file-name prompt (file-name-directory old) old nil
                           (file-name-nondirectory old))
           current-prefix-arg)))
  (suk--rename-or-move new-path overwrite))

;;; 交互式：输入“新目录”，文件名保持不变
(defun suk/move-file-to-dir (dir &optional overwrite)
  "把当前文件移动到 DIR（文件名保持不变）。
带前缀参数时允许覆盖已存在的同名文件。"
  (interactive
   (list (read-directory-name "目标目录: " nil nil t) current-prefix-arg))
  (let* ((name (file-name-nondirectory
                (or (buffer-file-name) (user-error "当前 buffer 没有关联文件"))))
         (dst  (expand-file-name name dir)))
    (suk--rename-or-move dst overwrite)))

;; --------------------------------------------------------------
;; copy file name
;; --------------------------------------------------------------
(defun suk/copy-file-name ()
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
(global-set-key (kbd "C-c b b") #'suk/create-scratch-buffer)


;; --------------------------------------------------------------
;; Create a new scratch buffer
;; --------------------------------------------------------------
(defun suk/new-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

(global-set-key (kbd "C-c b s") #'suk/create-scratch-buffer)


;; ------------------------------------------------------------
;; Buffer 相关 (b)
;; ------------------------------------------------------------
;;(keymap-set leader-key-map "b b" #'switch-to-buffer)
;;(keymap-set leader-key-map "b d" #'kill-this-buffer)
(keymap-set leader-key-map "b n" #'next-buffer) ;; default keys: C-x LEFT/RIGHT
(keymap-set leader-key-map "b p" #'previous-buffer)
(keymap-set leader-key-map "b d" #'suk/close-current-buffer)
(keymap-set leader-key-map "b D" #'suk/delete-this-file)
(keymap-set leader-key-map "b k" #'suk/kill-all-buffers-except-current)
(keymap-set leader-key-map "b K" #'suk/kill-other-window-buffer)
(keymap-set leader-key-map "b c" #'suk/copy-file-name)
(keymap-set leader-key-map "b s" #'suk/new-scratch-buffer)
(keymap-set leader-key-map "b b" #'suk/new-empty-buffer)
(keymap-set leader-key-map "b r" #'suk/open-recently-closed)
(keymap-set leader-key-map "b R" #'suk/list-recently-closed)
(keymap-set leader-key-map "b l" #'suk/open-last-closed)
(keymap-set leader-key-map "b L" #'suk/toggle-eol)
(keymap-set leader-key-map "b e" #'suk/suk/revert-auto-unicode-cjk)
(keymap-set leader-key-map "b M" #'suk/rename-or-move-file-here)
(keymap-set leader-key-map "b m" #'suk/move-file-to-dir)


(provide 'buffer-extension)
;;; buffer-extension.el ends here
