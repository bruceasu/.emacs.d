;;; init-speedbar.el --- Init speedbar
;;; Commentary:
;;
;; Init speedbar
;;

;;; Installation:
;;
;; Put init-speedbar.el to your load-path.
;; The load-path is usually ~/elisp/.
;; It's set in your ~/.emacs like this:
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;;
;; And the following to your ~/.emacs startup file.
;;
;; (require 'init-speedbar)
;;
;; No need more.

;;; Customize:
;;
;;
;;
;; All of the above can customize by:
;;      M-x customize-group RET init-speedbar RET
;;

;;; Change log:
;;
;; 2013/12/30
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

(require 'speedbar)
(require 'sr-speedbar)

;;; Code:

(setq speedbar-show-unknown-files t)    ;显示文件
(setq sr-speedbar-skip-other-window-p t)
(setq sr-speedbar-right-side nil)
(setq speedbar-buffers-key-map nil)     ;卸载一些按键
(setq speedbar-file-key-map nil)
(lazy-load-set-keys
 '(
   ;; 导航操作
   ("f" . speedbar-edit-line)             ;进入当前条目
   ("C-m" . speedbar-edit-line)           ;进入当前条目
   ("j" . speedbar-next)                  ;下一行
   ("k" . speedbar-prev)                  ;上一行
   ("n" . speedbar-forward-list)          ;下一条目
   ("p" . speedbar-backward-list)         ;上一条目
   ("u" . speedbar-forced-contract)       ;跳到上一级
   ("F" . speedbar-files)                 ;切换文件视图
   ("B" . speedbar-buffers)               ;切换缓存视图
   ("q" . sr-speedbar-toggle)             ;退出
   ;; 树操作
   ("x" . speedbar-expand-line)           ;展开当前行
   ("z" . speedbar-contract-line)         ;收缩当前行
   ("v" . speedbar-toggle-line-expansion) ;切换当前行的状态
   ;; 文件操作
   ("g" . speedbar-refresh)             ;刷新
   ("'" . speedbar-up-directory)        ;上一级目录
   ("i" . speedbar-item-info)           ;显示信息
   ("b" . speedbar-item-byte-compile)   ;编译
   ("l" . speedbar-item-load)           ;加载
   ("c" . speedbar-item-copy)           ;拷贝
   ("d" . speedbar-item-delete)         ;删除
   ("o" . speedbar-item-object-delete)  ;删除对象
   ("r" . speedbar-item-rename)         ;重命令
   ("m" . speedbar-create-directory)    ;创建目录
   ("K" . speedbar-buffer-kill-buffer)  ;关闭当前buffer
   )
 speedbar-key-map
 )

(provide 'init-speedbar)

;;; init-speedbar.el ends here
