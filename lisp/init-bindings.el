(eval-when-compile
  (require 'init-custom))

;; M-x global-set-key RET 交互式的绑定你的键。
;; C-x Esc Esc 调出上一条“复杂命令”
;; 设置绑定
(defun suk-set-key-bindings (action bindingList)
  "map keys for a set of actions."
  (mapcar (lambda(lst)
      ""
      (let ((x (car lst))
        (y (car (last lst))))
        (funcall action x y))) bindingList ))
;; 使用方式
; (suk-set-key-bindings 'global-set-key
;   (list
     ; '([f2]                            calendar)
     ; '([(shift f2)]                    remember)
     ; '([f5]                            revert-buffer)
     ; (list (kbd "C-c l")               'copy-line)
;    )
; )

;;Emacs 自动排版
;;很简单：C-x h C-M-\
;;其中C-x h 是全选
;;C-M-\ 是排版

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
      (list (kbd "C-c u")   'outline-up-heading)
      (list (kbd "C-x a")   'align)
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
      ;; '([f6]              set-mark-command)  ; c-@ or m-@
      '([S-f6]               hs-minor-mode)
      '([f5]                 toggle-truncate-lines)
      ;; '([?\S- ]           set-mark-command) ; shift+space,
      '([S-f11]              insert-translated-name-insert) ;; Chinese to English
      '([S-f12]              toggle-company-english-helper) ;; popup English tips
      '([M-f12]              aweshell-dedicated-toggle)
      ;;'([M-f11]              aweshell-sudo-toggle)
      '([M-f10]              aweshell-prev)
      '([M-f11]              aweshell-next)
      '([M-f9]               aweshell-new)
      ;; Ctrl-z 在普通PC中是undo, 在终端是转到后台。经常按错。把它做称开始选区文本吧。
      '([C-z]               set-mark-command)
      ;; '([f2]             calendar)
      '([f2]            gnus)
      '([C-f1]          toggle-font)
      '([apps]          showOrHide)
 )
)

;; 下面这两个键模拟Vi的光标不动屏幕动效果, 我很喜欢, 几乎总在使用.
(global-set-key [(meta n)] 'window-move-up)        
(global-set-key [(meta p)] 'window-move-DOWN)
;; 同上, 但是是另一个buffer窗口上下移动. 常常查看帮助用这个.
(global-set-key [(control N)] 'other-window-move-down)
(global-set-key [(control P)] 'other-window-move-up)

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


;; C-x C-q set/unset readonly


(provide 'init-bindings)
