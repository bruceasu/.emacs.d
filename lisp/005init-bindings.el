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


;;;


;; C-x C-q set/unset readonly


(provide 'init-bindings)
