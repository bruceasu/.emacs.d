;;; auto-save.el --- Auto save files when idle

;; Filename: auto-save.el
;; Description: Auto save files when idle
;; Author: Andy Stewart lazycat.manatee@gmail.com
;; Maintainer: Andy Stewart lazycat.manatee@gmail.com
;; Copyright (C) 2013 ~ 2014, Andy Stewart, all rights reserved.
;; Created: 2013-12-31 00:32:00
;; Version: 0.6
;; Last-Updated: 2018-12-20 12:10:44
;;           By: Andy Stewart
;; URL:
;; Keywords: autosave
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
;; Auto save file when emacs idle
;;

;;; Installation:
;;
;; Put auto-save.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-auto-save)
;; (auto-save-enable)
;;
;; Set `auto-save-silent' with non-nil if want emacs save files slient:
;; (setq auto-save-silent t)
;;
;; No need more.

;;; Change log:
;;
;; 2018/12/20
;;      * Don't save buffer when yassnippet or company is active.
;;
;; 2018/12/11
;;      * Do not flash minibuffer when saving automatically.
;;
;; 2018/10/05
;;      * Update font lock before save file.
;;
;; 2018/08/14
;;      *Fixed typo, change `auto-save-slient' to `auto-save-silent'.
;;
;; 2018/07/06
;;      * Add new option `auto-save-delete-trailing-whitespace'.
;;
;; 2014/01/04
;;      * Add new function `auto-save-enable' to enable auto-save in user config file.
;;      * Add options: `auto-save-idle' and `auto-save-silent'.
;;
;; 2008/10/20
;;      First released.
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

;; defgroup 关键字的意思是定义一个工作组，执行 Alt + x customize-group 命令的时候可以进行图形化的模块配置
;; 第一个参数是模块的名字， 比如 auto-save
;; 第二个参数是模块默认开启的状态， 在 elisp 中， t 表示 true, nil 表示 false
;; 第三个参数是对模块的文本解释
;; 第四个参数表示对外提供 auto-save 这个 group
(defgroup auto-save nil
  "Auto save file when emacs idle."
  :group 'auto-save)

;; defcustom 关键字的意思是定义一个可以被用户自定义的变量， 当用户执行 Alt + x customize-variable 的时候就可以补全 auto-save-idle 这个变量， defcustom 和 defvar 的区别主要是 defcustom 用于提供一些参数让用户可以在 Emacs 中图形化定制变量内容， defvar 这只有变量名和 List 内容， 一般用于函数内部变量值存储用， 不对外抛出给用户定制
;; 第一个参数是变量的名字 autos-ave-idle 
;; 第二个参数是变量的值， 这里我们定义为 1s, 表示自动保存的延迟秒数
;; 第三个参数是变量的解释， 一般在 Alt + x describe-variable 的时候就会显示具体变量的文档描述
;; 第四个参数用于定义变量的类型， 这里定义为整形， 这样在 customize-group 的时候只有输入整型才是正确保存
;; 第五个参数表示这个变量属于 auto-save 这个组， 主要作用就是 customize-group 的时候能够在一个界面中设置同一组的所有变量 
(defcustom auto-save-idle 1
  "The idle seconds to auto save file."
  :type 'integer
  :group 'auto-save)

;; autos-save-slient 的作用就是一个boolean值得变量， 设置为 nil 的时候， 表示每次自动保存都会在 minibuffer 提示， 设置成 t 的时候就会 shutup, 让我安安静静写会代码， 别闹...
(defcustom auto-save-silent nil
  "Nothing to dirty minibuffer if this option is non-nil."
  :type 'boolean
  :group 'auto-save)

(defcustom auto-save-delete-trailing-whitespace nil
  "Delete trailing whitespace when save if this option is non-nil.
Note, this option is non-nil, will delete all training whitespace execpet current line,
avoid delete current indent space when you programming."
  :type 'boolean
  :group 'auto-save)

;; 这段代码的作用就是避免 Emacs 在保存文件的时候生成一大堆垃圾的 #foo# 文件， 这种文件最讨厌了， 不但什么用都没有， 反而污染代码目录， 删除都删的我手酸
;; 想当年为了找到关闭这个脑残功能的变量， 我把 emacs 几百个带有 save 的变量全部打出来， 一个一个变量的试才找到你啊 （可惜当年我英文不好， 不知道怎么描述我想要的效果）
;; Emacs' default auto-save is stupid to generate #foo# files!
(setq auto-save-default nil)

;; 前方高能核心代码， 请集中注意力
(defun auto-save-buffers ()
  ;; 所有你在 Alt + x 以后可以调用的函数都要手动加上 (interactive) ， 否则这段代码只能在 Elisp 解释器中执行， 但是不能直接被用户从 Alt + x 调用， 就想 interactive 这个单词的意思一样
  (interactive)
  ;; 创建 autosave-buffer-list 这个变量， 用于保存所有需要遍历的 buffer 列表
  (let ((autosave-buffer-list))
    (ignore-errors
      ;; save-excursion 这个关键字的意思是， 所有在 save-excursion 里面的代码不管怎么折腾都不会对 save-excursion 之前的Emacs状态进行任何改变， 你可以理解为这个关键字的意思就是用于保护现场用的 ;)
      (save-excursion
        ;; dolist 的作用就和很多语言的 foreach 一个意思， 把 buffer-list 这个函数返回的所有 buffer 在循环内赋值给 buf 这个变量， 并在 dolist 的作用域中执行对 buf 影响的代码
        (dolist (buf (buffer-list))
          ;; 设置当前代码的 buffer 为 buf 变量值， 如果没有前面 save-excursion, 你会发现emacs会一直在快速的切换所有 buffer 的过程
          (set-buffer buf)
          ;; 如果当前 buffer 有一个相关联文件 (buffer-file-name), 同时当前 buffer 已经被用户修改了 (buffer-modified-p) 的情况下就执行自动保存
          (when (and
                 ;; Buffer associate with a filename?
                 (buffer-file-name)
                 ;; Buffer is modifiable?
                 (buffer-modified-p)
                 ;; Yassnippet is not active?
                 (or (not (boundp 'yas--active-snippets))
                     (not yas--active-snippets))
                 ;; Company is not active?
                 (or (not (boundp 'company-candidates))
                     (not company-candidates)))
            ;; 把当前 buffer 的名字压进 autosave-buffer-list 列表， 用于后面的保存提示
            (push (buffer-name) autosave-buffer-list)
            (if auto-save-silent
                ;; 如果 auto-save-slient 这个变量为 true, 就不显示任何保存信息， 因为 Emacs 的保存函数 (basic-save-buffer) 本身机会 blabla 的告诉你文件已经保存了， 所以我们用 with-temp-message 配合空字符串来禁止 with-temp-message 里面的代码在 minibuffer 显示任何内容
                ;; `inhibit-message' can shut up Emacs, but we want
                ;; it doesn't clean up echo area during saving
                (with-temp-message ""
                  (let ((inhibit-message t))
                    (basic-save-buffer)))
              (basic-save-buffer))
            ))
        ;; unless 的意思是除非 auto-save-slient 为 false 就执行    
        ;; Tell user when auto save files.
        (unless auto-save-silent
          ;; cond 就是 elisp 版的 switch， 用于条件语句对比执行
          (cond
           ;; 如果 autosave-buffer-list 列表里面没有任何一个文件需要保存， 我们就不要去烦用户了， 默默打酱油路过就好了
           ;; 如果有一个文件需要保存， 我们就说 Saved ... 
           ;; It's stupid tell user if nothing to save.
           ((= (length autosave-buffer-list) 1)
            (message "# Saved %s" (car autosave-buffer-list)))
           ;; 如果有多个文件需要保存， 就说 Saved ... files 
           ((> (length autosave-buffer-list) 1)
            (message "# Saved %d files: %s"
                     (length autosave-buffer-list)
                     (mapconcat 'identity autosave-buffer-list ", ")))))
        ))))

(defun auto-save-delete-trailing-whitespace-except-current-line ()
  (interactive)
  (when auto-save-delete-trailing-whitespace
    (let ((begin (line-beginning-position))
          (end (line-end-position)))
      (save-excursion
        (when (< (point-min) begin)
          (save-restriction
            (narrow-to-region (point-min) (1- begin))
            (delete-trailing-whitespace)))
        (when (> (point-max) end)
          (save-restriction
            (narrow-to-region (1+ end) (point-max))
            (delete-trailing-whitespace)))))))

(defun auto-save-enable ()
  (interactive)
  ;; run-with-idle-timer 函数的意思就是在 auto-save-idle 定义的描述以后自动执行 auto-save-buffers 函数
  ;; #' 的意思就是在 runtime 执行的时候再展开 auto-save-buffers 函数
  (run-with-idle-timer auto-save-idle t #'auto-save-buffers)
  (add-hook 'before-save-hook 'auto-save-delete-trailing-whitespace-except-current-line)
  (add-hook 'before-save-hook 'font-lock-flush)
  )

(provide 'auto-save)

;;; auto-save.el ends here
