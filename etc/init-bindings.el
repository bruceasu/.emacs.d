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
  (require '+custom))

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
;; (suk-set-key-bindings 'global-set-key
;;   (list
;;      '([f2]                            calendar)
;;      '([(shift f2)]                    remember)
;;      '([f5]                            revert-buffer)
;;      (list (kbd "C-c l")               'copy-line)
;;    )
;; )




(suk-set-key-bindings 'global-set-key
					  (list
					   (list (kbd "C-x l")   'suk/count-brf-lines)
					   (list (kbd "C-x M-a") 'align-regexp)
					   (list (kbd "C-x x")   'suk/switch-major-mode)
					   (list (kbd "C-x X")   'suk/get-mode-name)
					   (list (kbd "C-x U")   'suk/revert-buffer-with-utf8)
					   (list (kbd "C-x K")   'suk/revert-buffer-with-gbk)
					   '([C-t]                transpose-chars)
					   
					   '([S-f6]               hs-minor-mode)
					   '([f5]                 toggle-truncate-lines)
					   ;; '([S-f11]              insert-translated-name-insert) ;; Chinese to English
					   ;; '([S-f12]              toggle-company-english-helper) ;; popup English tips
					   ;; '([M-f12]              aweshell-dedicated-toggle)
					   ;; '([M-f11]            aweshell-sudo-toggle)
					   ;; '([M-f10]              aweshell-prev)
					   ;; '([M-f11]              aweshell-next)
					   ;; '([M-f9]               aweshell-new)
					   ;; '([S-f2]               suk/new-empty-buffer)
					   '([f2]             hs-toggle-hiding)
					   ;; '([C-f2]            gnus)
					   '([S-f1]          snails)
					   ))

(global-set-key  (kbd "S-SPC") 'set-mark-command)
(global-set-key "\C-c\C-i" 'indent-region) ; C-u C-c TAB => (un)indent-region

;; global-set-key examples:
;; (global-set-key (kbd "C-x C-\\") 'next-line)
;; (global-set-key [?\C-x ?\C-\\] 'next-line)
;; (global-set-key [(control ?x) (control ?\\)] 'next-line)

;; has set to f7, c-f7
;;(global-set-key (kbd "<C-f6>") '(lambda () (interactive) (bookmark-set "SAVED")))
;;(global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "SAVED")))

;; C-x LEFT/RIGHT
;;(global-set-key (kbd "C-<f9>") 'previous-buffer)
;;(global-set-key (kbd "C-<f10>") 'next-buffer)

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



;;Emacs 自动排版
;;很简单：C-x h C-M-\
;;其中C-x h 是全选
;;C-M-\ 是排版

;; C-x C-q set/unset readonly

;; (require 'undo-tree)
;;(define-key undo-tree-map (kbd "C-x u") #'(lambda ()
;;   (interactive)
;;   (undo-tree-visualize)
;;   (undo-tree-visualize-undo)))
;; c-/ c-_  undo | c-x u undo-tree | c-s-/ s-? M-_ redo

;; 大小写转换： M-u, M-l, M-c

;; M-x align-regexp 可以方便的对齐一些文字

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

;; f3 start macro(kmacro-start-macro-or-insert-counter),
;; f4 done macro or run marcro (kmacro-end-or-call-macro).
;; C-x ( start macro (kmacro-start-macro),
;; C-x ) end done marco,
;; C-x e run marco(kmacro-end-macro)
;; 先定义一个宏
;; 然后 name-last-kbd-macro
;; 然后 insert-kbd-macro
;; 等到如下类似的配置 
(fset 'delete-empty-lines (kbd "M-x flush-lines RET ^\s-*$ RET"))


(define-prefix-command 'leader-key)
(global-set-key (kbd "M-s-SPC") 'leader-key)

(global-set-key (kbd "C-(") 'backward-sexp) 
(global-set-key (kbd "C-)") 'forward-sexp)


;; 启动点er啥
(defhydra suk/hydra-open-menu ()
  "
							^启动点er啥^
-----------------------------------------------------------------
[_t_] ^vterm^
"
  ("t" vterm nil :color blue)
  ("q" nil "QUIT" :color blue))
;; 开关
(defhydra suk/hydra-toggle-menu ()
  "
							^开关^
-----------------------------------------------------------------
[_T_] ^透明^		[_p_] ^代理^		[_f_] ^FlyCheck^
"
  ("T" suk/toggle-transparency nil)
  ("p" suk/toggle-proxy nil)
  ("f" global-flycheck-mode nil)
  ("q" nil "QUIT" :color blue))
;; 窗格
(defhydra suk/hydra-window-menu ()
  "
							^窗口管理器^
-----------------------------------------------------------------
[_x_] ^关闭窗格^			[_F_] ^全屏模式^		[_K_] ^↑+^		[_k_] ^go ↑^    
[_;_] ^关闭其他窗格^		[_r_] ^旋转交换^		[_J_] ^↓+^		[_j_] ^go ↓^ 
[_:_] ^新建窗格(垂直)^		[_s_] ^选择交换^		[_H_] ^←+^		[_h_] ^go ←^
[_|_] ^新建窗格(水平)^		[_b_] ^平均铺开^		[_L_] ^→+^		[_l_] ^go →^
"
  ("x" delete-window nil)
  (";" delete-other-window nil :color blue)
  (":" split-window-vertically nil)
  ("|" split-window-horizontally nil)
  ("F" toggle-frame-fullscreen nil :color blue)
  ("r" rotate-window nil)
  ("s" ace-swap-window nil :color blue)
  ("b" balance-windows nil :color blue)
  ("H" shrink-window-horizontally nil)
  ("J" enlarge-window nil)
  ("K" shrink-window nil)
  ("L" enlarge-window-horizontally nil)
  ("h" windmove-left nil)
  ("j" windmove-down nil)
  ("k" windmove-up nil)
  ("l" windmove-right nil)
  ("q" nil "QUIT" :color blue))

;; 常用的命令
(defhydra suk/hydra-common-menu ()
  "
						^常用^
------------------------------------------------------
[_g_] ^counsel-rg^					[_y_] ^counsel-yank-pop^
[_f_] ^counsel-fzf^					[_d_] ^counsel-dired^	   
[_r_] ^counsel-recentf^				[_m_] ^counsel-bookmark^ 
[_b_] ^counsel-switch-buffer^		[_l_] ^counsel-linux-app^
[_w_] ^capture-word^
"
  ("g" counsel-rg nil :color blue)
  ("f" counsel-fzf nil :color blue)
  ("r" counsel-recentf nil :color blue)
  ("b" counsel-switch-buffer nil :color blue)
  ("y" counsel-yank-pop nil :color blue)
  ("d" counsel-dired nil :color blue)
  ("m" counsel-bookmark nil :color blue)
  ("l" counsel-linux-app nil :color blue)
  ("w" suk/capture-get-word-point nil :color blue)
  ("q" nil "QUIT" :color blue))
;; 主菜单
(defhydra suk/hydra-main-menu ()
  "
							^主菜单^
————————————————————————————————————————————————————————————
[_o_] ^开启^ 	[_t_] ^开关^ 	[_w_] ^窗格^ 	[_c_] ^常用^
"
  ("o" suk/hydra-open-menu/body nil :color blue)
  ("t" suk/hydra-toggle-menu/body nil :color blue)
  ("w" suk/hydra-window-menu/body nil :color blue)
  ("c" suk/hydra-common-menu/body nil :color blue)
  ("q" nil "QUIT" :color blue))

(global-set-key (kbd "C-M-,") #'suk/hydra-main-menu/body)



(provide 'init-bindings)
