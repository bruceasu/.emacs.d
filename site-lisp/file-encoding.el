;; file-encoding.el --- about file encoding and end of line configurations.
;; -*- lexical-binding: t -*-

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
;; file encoding and end of line  configuration.
;;
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

(eval-when-compile
  (require '+const)
  (require '+custom))
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

;; ===============================================================
;; 重新读取文件
;; Revert buffer
;; ---------------------------------------------------------------
;;;###autoload
(defun suk/revert-current-buffer ()
  "Revert the current buffer. key \\[suk/revert-current-buffer]."
  (interactive)
  (message "Revert this buffer.")
  (revert-buffer t t))
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
    (suk/revert-buffer-no-confirm)))
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
(provide 'file-encoding)
