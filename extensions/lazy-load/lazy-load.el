;;; lazy-load.el --- Lazy load keys for speed ​​up Emacs startup.

;; Filename: lazy-load.el
;; Description: Lazy set keystroke.
;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Created: 2009-02-07 22:32:50
;; Version: 0.1
;; Last-Updated: 2009-02-07 22:32:50
;;           By: Andy Stewart
;; URL: http://www.emacswiki.org/emacs/download/lazy-load.el
;; Keywords:
;; Compatibility: GNU Emacs 23.0.60.1
;;
;; Features that might be required by this library:
;;
;;
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; Lazy set keystroke.
;;

;;; Installation:
;;
;; Put lazy-load.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'lazy-load)
;;
;; No need more.

;;; Change log:
;;
;; 2019/05/05
;;      * Refactory code.
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


;;; Code:

(defun lazy-load-global-keys (key-alist filename &optional key-prefix)
  (lazy-load-set-keys key-alist nil key-prefix)
  (dolist (element key-alist)
    (setq fun (cdr element))
    (autoload fun filename nil t)))

(defun lazy-load-local-keys (key-alist keymap filename &optional key-prefix)
  (lazy-load-set-keys key-alist keymap key-prefix)
  (dolist (element key-alist)
    (setq fun (cdr element))
    (autoload fun filename nil t)))

(defun lazy-load-set-keys (key-alist &optional keymap key-prefix)
  "This function is to little type when define key binding.
`KEYMAP' is a add keymap for some binding, default is `current-global-map'.
`KEY-ALIST' is a alist contain main-key and command.
`KEY-PREFIX' is a add prefix for some binding, default is nil."
  (let (key def)
    (or keymap (setq keymap (current-global-map)))
    (if key-prefix
        (setq key-prefix (concat key-prefix " "))
      (setq key-prefix ""))
    (dolist (element key-alist)
      (setq key (car element))
      (setq def (cdr element))
      (cond ((stringp key) (setq key (read-kbd-macro (concat key-prefix key))))
            ((vectorp key) nil)
            (t (signal 'wrong-type-argument (list 'array key))))
      (define-key keymap key def))))

;; (defun lazy-load-unset-keys (key-list &optional keymap)
;;   "This function is to little type when unset key binding.
;; `KEYMAP' is add keymap for some binding, default is `current-global-map'
;; `KEY-LIST' is list contain key."
;;   (let (key)
;;     (or keymap (setq keymap (current-global-map)))
;;     (dolist (key key-list)
;;       (cond ((stringp key) (setq key (read-kbd-macro (concat key))))
;;             ((vectorp key) nil)
;;             (t (signal 'wrong-type-argument (list 'array key))))
;;       (define-key keymap key nil))))

(defvar lazy-load-dangerous-prefixes
  '("C-x" "C-c" "M-s" "M-g")
  "不要直接解绑的危险前缀键列表。
解绑这些等于移除整个大 keymap，会导致很多内置功能失效。")

(defun lazy-load-unset-keys (key-list &optional keymap)
  "批量解绑 KEY-LIST 里的快捷键。
- KEY-LIST 元素可以是字符串（如 \"C-x C-c\"）或向量（如 [?\C-x ?\C-c]）。
- 如果试图解绑危险前缀（见 `lazy-load-dangerous-prefixes`），会发出警告并跳过。
- 解绑时会提示原先绑定的命令。"
  (let ((map (or keymap (current-global-map))))
    (dolist (key key-list)
      (cond
       ;; 字符串形式
       ((stringp key)
        (if (member key lazy-load-dangerous-prefixes)
            (message "⚠️ 跳过解绑危险前缀: %s" key)
          (let* ((kbd (read-kbd-macro key))
                 (old (lookup-key map kbd)))
            (define-key map kbd nil)
            (message "🔑 Unset %s (was %s)" key (or old "unbound")))))
       ;; 向量形式
       ((vectorp key)
        (let ((old (lookup-key map key)))
          (define-key map key nil)
          (message "🔑 Unset %s (was %s)" key (or old "unbound"))))
       ;; 其他情况报错
       (t
        (signal 'wrong-type-argument (list 'string-or-vector key)))))))

(defun lazy-load-rebind-key (key def &optional keymap)
  "重新绑定 KEY 到 DEF。
- KEY 是字符串（如 \"C-x C-c\"）或向量（如 [?\C-x ?\C-c]）。
- DEF 是目标命令（函数符号）。
- KEYMAP 默认为全局 `current-global-map'`。
会在 minibuffer 提示原绑定命令 → 新命令。"
  (let* ((map (or keymap (current-global-map)))
         (kbd (if (stringp key)
                  (read-kbd-macro key)
                (if (vectorp key)
                    key
                  (signal 'wrong-type-argument (list 'string-or-vector key)))))
         (old (lookup-key map kbd)))
    ;; 先解绑
    (define-key map kbd nil)
    ;; 再绑定新命令
    (define-key map kbd def)
    ;; 提示消息
    (message "🔑 Rebind %s: %s → %s"
             (key-description kbd)
             (or old "unbound")
             def)))



(provide 'lazy-load)

;;; lazy-load.el ends here
