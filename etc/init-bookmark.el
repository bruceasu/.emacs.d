;; init-bookmark.el --- bookmark configurations.	-*- lexical-binding: t -*-

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
;; Bookmark configuration.
;;

;;; Code:


;; 同步更新书签文件
;; 或者退出时保存
(setq bookmark-save-flag 1)

;;C-x r m (name)  M-x bookmark-set  设置书签
;;C-x r b (name)  M-x bookmark-jump  跳转到书签
;;C-x r l         M-x bookmark-bmenu-list  书签列表
;;                M-x bookmark-delete  删除书签
;;                M-x bookmark-load  读取存储书签文件

;; =========================================================
;; 方便快速跳到bookmark
;; ---------------------------------------------------------
;;;###autoload
(defun suk/ska-point-to-register()
  "Store cursorposition _fast_ in a register. Use ska-jump-to-register to jump back to the stored position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))
;; ---------------------------------------------------------
;;;###autoload
(defun suk/ska-jump-to-register()
  "Switch between current cursorposition and position that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
        (jump-to-register 8)
        (set-register 8 tmp)))

;; use init-key.el to load and bind the functions.
;;(global-set-key  [C-f7] 'suk/ska-point-to-register)
;;(global-set-key  [f7] 'suk/ska-jump-to-register)

;; bookmark
;; C-x r m <RET>
;; Set the bookmark for the visited file, at point.
;; C-x r m bookmark <RET>
;; Set the bookmark named bookmark at point (bookmark-set).
;; C-x r M bookmark <RET>
;; Like C-x r m, but don't overwrite an existing bookmark.
;; C-x r b bookmark <RET>
;; Jump to the bookmark named bookmark (bookmark-jump).
;; C-x r l
;; List all bookmarks (list-bookmarks).
;; M-x bookmark-save
;; Save all the current bookmark values in the default bookmark file.
;; M-x bookmark-load <RET> filename <RET>
;; Load a file named filename that contains a list of bookmark values. You can use this command, as well as bookmark-write, to work with other files of bookmark values in addition to your default bookmark file.
;; M-x bookmark-write <RET> filename <RET>
;; Save all the current bookmark values in the file filename.
;; M-x bookmark-delete <RET> bookmark <RET>
;; Delete the bookmark named bookmark.
;; M-x bookmark-insert-location <RET> bookmark <RET>
;; Insert in the buffer the name of the file that bookmark bookmark points to.
;; M-x bookmark-insert <RET> bookmark <RET>
;; Insert in the buffer the contents of the file that bookmark bookmark points to.


(provide 'init-bookmark)
;;; init-bookmark.el ends here
