;;; init-binding.el --- Global key bindings.	-*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2018 Suk

;; Author: Suk
;; Version: 1.0.0
;; Keywords: .emacs.d

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
;; Global Key Bindings
;;

;;; Code:

(eval-when-compile
  (require 'init-custom))

;; M-x global-set-key RET 交互式的绑定你的键。
;; C-x Esc Esc 调出上一条“复杂命令”
;; 设置绑定
(defun suk-set-key-bindings (ACTION BINDINGLIST)
  "Map keys.
ACTION usually is 'global-set-key', and BINDINGLIST is key and command LIST."

     (mapcar (lambda(lst)
      ""
      (let ((x (car lst))
        (y (car (last lst))))
        (funcall ACTION x y))) BINDINGLIST ))

;; 使用方式
; (suk-set-key-bindings 'global-set-key
;   (list
     ; '([f2]                            calendar)
     ; '([(shift f2)]                    remember)
     ; '([f5]                            revert-buffer)
     ; (list (kbd "C-c l")               'copy-line)
;    )
; )



;;; 在其他地方有设置
;;; (require 'undo-tree)
;;;(define-key undo-tree-map (kbd "C-x u") #'(lambda ()
;;;   (interactive)
;;;   (undo-tree-visualize)
;;;   (undo-tree-visualize-undo)))
;;; c-/ c-_  undo | c-x u undo-tree | c-s-/ s-? M-_ redo

;; M-x align-regexp 可以方便的对齐一些文字
(suk-set-key-bindings 'global-set-key
  (list
      (list (kbd "C-x l")   'suk/count-brf-lines)
      (list (kbd "C-x M-a") 'align-regexp)
      (list (kbd "C-x x")   'suk/switch-major-mode)
      (list (kbd "C-x X")   'suk/get-mode-name)
      (list (kbd "C-x U")   'suk/revert-buffer-with-utf8)
      (list (kbd "C-x K")   'suk/revert-buffer-with-gbk)
      '([C-t]                transpose-chars)
      '([C-f7]               suk/ska-point-to-register)
      '([f7]                 suk/ska-jump-to-register)
      ;; '([f1]              suk/toggle-letter-case) M-u, M-l, M-c
      ;; f3 start macro(kmacro-start-macro-or-insert-counter), f4 done macro or run marcro (kmacro-end-or-call-macro).
      ;; C-x ( start macro (kmacro-start-macro), C-x ) end done marco, C-x e run marco(kmacro-end-macro)
      '([M-f6]               insert-kbd-macro)
      '([C-f6]               name-last-kbd-macro)
      '([S-f6]               hs-minor-mode)
      '([f5]                 toggle-truncate-lines)
      '([S-f11]              insert-translated-name-insert) ;; Chinese to English
      '([S-f12]              toggle-company-english-helper) ;; popup English tips
      '([M-f12]              aweshell-dedicated-toggle)
      ;;'([M-f11]            aweshell-sudo-toggle)
      '([M-f10]              aweshell-prev)
      '([M-f11]              aweshell-next)
      '([M-f9]               aweshell-new)
      '([C-f2]               xah-new-empty-buffer)
      ;; '([f2]             calendar)
      '([S-f2]            gnus)
      '([C-f1]          toggle-font)
 ))
(global-set-key  (kbd "S-SPC") 'set-mark-command)
(global-set-key "\C-c\C-i" 'indent-region) ; C-u C-c TAB => (un)indent-region
(global-set-key (kbd "C-x k") 'xah-close-current-buffer)
(global-set-key (kbd "C-S-t") 'xah-open-last-closed) ; control+shift+t

;; global-set-key examples:
;; (global-set-key (kbd "C-x C-\\") 'next-line)
;; (global-set-key [?\C-x ?\C-\\] 'next-line)
;; (global-set-key [(control ?x) (control ?\\)] 'next-line)


;; 下面这两个键模拟Vi的光标不动屏幕动效果, 我很喜欢, 几乎总在使用.
(global-set-key [(meta N)] 'window-move-up)        
(global-set-key [(meta P)] 'window-move-DOWN)
;; 同上, 但是是另一个buffer窗口上下移动. 常常查看帮助用这个.
(global-set-key [(control N)] 'other-window-move-up)
(global-set-key [(control P)] 'other-window-move-down)

;;;
;; 演示了如何定义一个新的按键前缀. 这里定义了M-c作为按键前缀.
;; (define-prefix-command 'comma-map)
;; (global-set-key (kbd ",") 'comma-map)
;; (global-set-key [(meta c)] 'meta-c-map)

;; 演示了如何在一个模式下(这里是isearch模式), 定义快捷键. 退出isearch-mode, 所有按键失效.
 (add-hook 'isearch-mode-hook
 '(lambda ()
;; 搜索下一个结果
 (define-key isearch-mode-map [(meta n)] 'isearch-repeat-forward)
;; 搜索前一个结果
(define-key isearch-mode-map [(meta p)] 'isearch-repeat-backward)
;; 替换
 (define-key isearch-mode-map [(control r)] 'isearch-query-replace)
;; 正则替换
 (define-key isearch-mode-map [(meta 5)] 'isearch-query-replace-regexp)
 (define-key isearch-mode-map [(meta f)] 'isearch-yank-word-or-char)
;; 剪切板作为搜索内容
 (define-key isearch-mode-map [(meta y)] 'isearch-yank-kill)
;; 将光标到行尾作为搜索内容
 (define-key isearch-mode-map [(meta k)] 'isearch-yank-line)
(define-key isearch-mode-map [(hyper l)] 'isearch-yank-char)
;; 向左或向右(选择/取消)单个字符作为搜索内容
(define-key isearch-mode-map [(hyper j)] 'isearch-delete-char)
;; 显示occur视图
(define-key isearch-mode-map [(meta o)] 'isearch-occur)
;; 单词搜索
 (define-key isearch-mode-map [(meta w)] 'isearch-forward-word)
 (define-key isearch-mode-map [(meta s)] 'isearch-repeat-forward)
))

;; ------------------------------下面是上面用到的函数定义------------------------------
(defun window-move-up (&optional arg)
	"Current window move-up 2 lines."
	(interactive "P")
	(if arg
		(scroll-up arg)
		(scroll-up 2)))

(defun window-move-down (&optional arg)
	"Current window move-down 3 lines."
	(interactive "P")
	(if arg
		(scroll-down arg)
		(scroll-down 3)))

		(defun other-window-move-up (&optional arg)
		"Other window move-up 1 lines."
		(interactive "p")
		(scroll-other-window arg))

(defun other-window-move-down (&optional arg)
	"Other window move-down 2 lines."
	(interactive "P")
	(if arg
		(scroll-other-window-down arg)
	(scroll-other-window-down 2)))

;;Emacs 自动排版
;;很简单：C-x h C-M-\
;;其中C-x h 是全选
;;C-M-\ 是排版

;; C-x C-q set/unset readonly

;;; rectangle
;; C-x r k
;; Kill the text of the region-rectangle, saving its contents as the last killed rectangle (kill-rectangle).
;; C-x r M-w
;; Save the text of the region-rectangle as the last killed rectangle (copy-rectangle-as-kill).
;; C-x r d
;; Delete the text of the region-rectangle (delete-rectangle).
;; C-x r y
;; Yank the last killed rectangle with its upper left corner at point (yank-rectangle).
;; C-x r o
;; Insert blank space to fill the space of the region-rectangle (open-rectangle). This pushes the previous contents of the region-rectangle to the right.
;; C-x r N
;; Insert line numbers along the left edge of the region-rectangle (rectangle-number-lines). This pushes the previous contents of the region-rectangle to the right.
;; C-x r c
;; Clear the region-rectangle by replacing all of its contents with spaces (clear-rectangle).
;; M-x delete-whitespace-rectangle
;; Delete whitespace in each of the lines on the specified rectangle, starting from the left edge column of the rectangle.
;; C-x r t string <RET>
;; Replace rectangle contents with string on each line (string-rectangle).
;; M-x string-insert-rectangle <RET> string <RET>
;; Insert string on each line of the rectangle.
;; C-x <SPC>
;; Toggle Rectangle Mark mode (rectangle-mark-mode). When this mode is active, the region-rectangle is highlighted and can be shrunk/grown, and the standard kill and yank commands operate on it.
;; The rectangle operations fall into two classes: commands to erase or insert rectangles, and comm

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

(provide 'init-bindings)
