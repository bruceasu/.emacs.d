;; init-file-encoding.el --- Initialize file encoding and end of
;; line configurations. -*- lexical-binding: t -*-

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

(eval-when-compile
  (require '+const)
  (require '+custom))

;; 编码设置 begin
(setq default-buffer-file-coding-system 'utf-8-unix)            ;缓存文件编码
(setq default-file-name-coding-system 'utf-8-unix)              ;文件名编码
(setq default-keyboard-coding-system 'utf-8-unix)               ;键盘输入编码
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)) ;进程输出输入编码
(setq default-sendmail-coding-system 'utf-8-unix)               ;发送邮件编码
(setq default-terminal-coding-system 'utf-8-unix)               ;终端编码

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; 23.2 之后废弃，用buffer-file-coding-system
;(setq default-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;; for windows
;; (setq-default pathname-coding-system 'euc-cn)
;; (set-language-environment 'Chinese-GB)
;; 据说设置为UTF-8不会卡顿
(set-language-environment "UTF-8")
;;(setq file-name-coding-system 'euc-cn)
;;(setq file-name-coding-system 'gb18030)
;;(if is-os-windows
;;	(setq file-name-coding-system 'gb18030))
(if sys/win32p
	(setq file-name-coding-system 'gb18030)
	;;(setq file-name-coding-system 'big5-hkscs)
)
;; 重要提示:写在最后一行的，实际上最优先使用; 最前面一行，反而放到最后才识别。
;; utf-16le-with-signature 相当于 Windows 下的 Unicode 编码，这里也可写成
;; utf-16 (utf-16 实际上还细分为 utf-16le, utf-16be, utf-16le-with-signature等多种)
;; 繁体
(prefer-coding-system 'cp950)
(prefer-coding-system 'big5-hkscs)
;; 简体
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
;; Unicode
;(prefer-coding-system 'utf-16le-with-signature)
(prefer-coding-system 'utf-16)
;; 新建文件使用utf-8-unix方式
;; 如果不写下面两句，只写
(prefer-coding-system 'utf-8)
;; 这一句的话，新建文件以utf-8编码，行末结束符平台相关
(unless sys/win32p
  (prefer-coding-system 'utf-8-dos)
  (prefer-coding-system 'utf-8-unix)
  )

(setq session-save-file-coding-system 'utf-8)
;;(set-charset-priority 'unicode)


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

;; ===============================================================
;; 重新读取文件
;; Revert buffer
;; ---------------------------------------------------------------
(defun suk/revert-current-buffer ()
  "Revert the current buffer. key \\[suk/revert-current-buffer]."
  (interactive)
  (message "Revert this buffer.")
  (revert-buffer t t))

(defun suk/revert-buffer-no-confirm ()
  "执行`revert-buffer'时不需要确认. key \\[suk/revert-buffer-no-confirm]."
  (interactive)
  (when (buffer-file-name)
    (revert-buffer buffer-file-name t)
   )
  )



;;; =========================================================
;;; 用新编码重新读取文件
;;; ---------------------------------------------------------
(defun suk/revert-buffer-with-coding-system-no-confirm (coding-system)
  "Call `revert-buffer-with-coding-system' with CODING-SYSTEM, but when `revert-buffer' do not need confirm."
  (interactive "Coding system for visited file (default nil): ")
  (let ((coding-system-for-read coding-system))
    (suk/revert-buffer-no-confirm)))
;;; ---------------------------------------------------------
(defun suk/revert-buffer-with-gbk ()
  "Call `revert-buffer-with-coding-system-no-confirm' with gbk.
It is bound to \\[suk/revert-buffer-with-gbk]."
  (interactive)
  (suk/revert-buffer-with-coding-system-no-confirm 'gb18030))
;;; ---------------------------------------------------------
(defun suk/revert-buffer-with-utf8 ()
  "Call `revert-buffer-with-coding-system-no-confirm' with utf-8.
It is bound to \\[suk/revert-buffer-with-utf8]."
  (interactive)
  (suk/revert-buffer-with-coding-system-no-confirm 'utf-8))
;;; =========================================================

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


;; (bind-keys ("<f5>" . suk/revert-current-buffer)
;;            ("s-r" . suk/revert-current-buffer))

;; --------------------------------------------------------------

(defun no-junk-please-were-unixish ()
  "只用unix类换行格式."
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))

(add-hook 'find-file-hook 'no-junk-please-were-unixish)

(provide 'init-file-encoding)
;;; init-file-encoding.el ends here
