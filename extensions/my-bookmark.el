;; init-bookmark.el --- bookmark configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Bookmark configuration.
;;

;;; Code:


;; 同步更新书签文件
;; 或者退出时保存
(setq bookmark-save-flag 1)

(with-eval-after-load 'bookmark
  ;; use my own bookmark if it exists
  (let ((file "~/var/.emacs.bmk"))
    (when (file-exists-p file)
      (setq bookmark-default-file file))))


(defun remember-init ()
  "Remember current position and setup."
  (interactive)
  (point-to-register 8)
  (message "Have remember one position"))

(defun remember-jump ()
  "Jump to latest position and setup."
  (interactive)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp))
  (message "Have back to remember position"))

(defvar point-stack nil
  "A stack to store (buffer . point) pairs.")

(defun point-stack-push ()
  "Push current point in stack."
  (interactive)
  (message "Location marked.")
  (setq point-stack (cons (list (current-buffer) (point)) point-stack)))

(defun point-stack-pop ()
  "Pop point from stack."
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (switch-to-buffer (caar point-stack))
    (goto-char (cadar point-stack))
    (setq point-stack (cdr point-stack))))

;; use init-key.el to load and bind the functions.
;;(global-set-key  [C-f7] 'suk/point-to-register)
;;(global-set-key  [f7] 'suk/jump-to-register)


(with-eval-after-load 'hydra
  (defhydra suk/bookmark-launcher (:color blue)
    "
^Bookmark^
-------------------------------------------------------------------
[_b_] New bookmark     [_s_] Store bookmark
[_m_] Goto bookmark    [_l_] Load bookmark
[_i_] Quick bookmark   [_r_] Register bookmark
[_j_] Goto bookmark    [_,_] Pop bookmark
[_q_] Quit             [_._] Push bookmark
"
    ("b" bookmark-set)
    ("m" bookmark-jump)
    ("l" consult-register-load)
    ("s" consult-register-store)      ;; orig. abbrev-prefix-mark (unrelated)
    ("r" consult-register)
    ("i" remember-init)      ;记忆初始函数
    ("j" remember-jump)      ;记忆跳转函数
    ("J" bookmark-jump)
    ("," point-stack-pop)    ;buffer索引跳转
    ("." point-stack-push)   ;buffer索引标记
    ("q" nil :color red))
  ;; (global-set-key (kbd "C-c C-y") 'suk/bookmark-launcher/body)
  )

;; bookmark

;;C-x r m (name)  M-x bookmark-set  设置书签
;;C-x r b (name)  M-x bookmark-jump  跳转到书签
;;C-x r l         M-x bookmark-bmenu-list  书签列表
;;                M-x bookmark-delete  删除书签
;;                M-x bookmark-load  读取存储书签文件


;; 这个是 Emacs 自带的功能，通过 C-x r m 调用 bookmark-set 函数，将文件夹设置为
;; bookmark。设置成功之后，就可以使用 C-x r j 跳转到文件夹当中。除书签功能之处，
;; 也可以使用 register 功能跳转到某个文件。

;; C-x r m <RET>          Set the bookmark for the visited file, at point.
;; C-x r m bookmark <RET> Set the bookmark named bookmark at point (bookmark-set).
;; C-x r M bookmark <RET> Like C-x r m, but don't overwrite an existing bookmark.
;; C-x r b bookmark <RET> Jump to the bookmark named bookmark (bookmark-jump).
;; C-x r l                List all bookmarks (list-bookmarks).
;; M-x bookmark-save      Save all the current bookmark values in the default bookmark file.
;; M-x bookmark-load <RET> filename <RET>            Load a file named filename that contains a list of bookmark values. You can use this command, as well as bookmark-write, to work with other files of bookmark values in addition to your default bookmark file.
;; M-x bookmark-write <RET> filename <RET>           Save all the current bookmark values in the file filename.
;; M-x bookmark-delete <RET> bookmark <RET>          Delete the bookmark named bookmark.
;; M-x bookmark-insert-location <RET> bookmark <RET> Insert in the buffer the name of the file that bookmark bookmark points to.
;; M-x bookmark-insert <RET> bookmark <RET>          Insert in the buffer the contents of the file that bookmark bookmark points to.


;; Bookmark
(provide 'my-bookmark)
;;; my-bookmark.el ends here
