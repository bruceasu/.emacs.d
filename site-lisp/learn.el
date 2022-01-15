;; -*- lexical-binding: t; -*-
;; 学习如何编写 emacs 插件
;;
;;
;; 在底部 message 栏那里显示当前所在函数的帮助
;;  eldoc-mode ，emacs 自带的

;; 简易的配置文件目录

(defun crete-quck-config-link (label link)
  "create LINK with LABEL."
  (insert label ": ")
  (insert-button link
				 'action (lambda (_) (find-file link))
				 'follow-link t)
  (insert "\n)"))

(defun open-quick-config-links()
  "Open the configurations."
  (interactive)
  (let ((buf (get-buffer-create "*Config Links*")) ; create a buffer
		(configs '(("BASH" , "~/.bashrc")
				   ("Emacs" . "~/.emacs.d")))) ; set configs
	(with-current-buffer buf ; change to the buf
	  (erase-buffer) ; clear buf
	  (mapcar (lambda (item)
				(create-quick-config-link (car item) (cdr item)))
			  configs))
	(pop-to-buffer buf t) ; display the buf
	))

;; map key
;;(define-key global-map (kbd "<f9>") #'open-quick-config-links)


;; -------------------------------------------------------------
;; overlay
;; 用来对改变原始文本的显示，例如高亮。

(defvar ovs nil)
(progn
  (mapcar #'delete-overlay ovs) ; clean
  (setq ovs nil)
  (let (( ov (make-overlay 37 42))) ;; 37 - 42 is the postion of the text.
	(push ov ovs)
	(overlay-put ov 'face 'font-lock-keyword-face) ; font and color
	(overlay-put ov 'display '((height 1.5) ; scale font size.
							   (raise -0.5)))
;;	(overlay-put ov 'display "xxxx") ; replace the text.
;;	(overlay-put ov 'dsipaly (create-image "url")) ; replace with picture.
	))

;; -------------------------------------------------------------
;; 用自定义函数改良命令
;;

(require 'cl-lib)

(defface postion-hint-face
  '((t (:inherit region)))
  "Position hint")

(defvar position-hint-move-function nil)


(defun move-to-position-hint(n)
  (interactive)
  (dotimes (_i n )
	(call-interactively position-hint-move-function))
  (highlight-position-hint position-hint-move-function))

(defun move-to-position-hint-1()
  (interactive)
  (move-to-position-hint 1))


(defun move-to-position-hint-2()
  (interactive)
  (move-to-position-hint 2))


(defun highlight-position-hint (cmd)
  (let (ovs)
  (save-mark-and-excursion
	(cl-loop for i from 1 to 9 do
			 (call-interactively cmd)
			 (let ((ov (make-overlay (point) (1+(point)))))
			   (overlay-put ov 'display
							(cond ((looking-at-p "\n")
								   (format "%d\n" i))
								  (t (format "%d" i))))
			   (overlay-put ov 'face position-hint-face)
			   (push ov ovs)))))
  (sit-for 1)
  (mapcar #'delete-overlay ovs)
  (setq position-hint-move-function cmd)
  (set-transient-map position-hint-map t (lambda () (setq position-hint-move-function nil))  ))

(defun my-forward-word()
  (interactive)
  (call-interactively #'forward-word)
  (heighlight-position-hint #'forward-word))

(defun my-backward-word()
  (interactive)
  (call-interactively #'backward-word)
  (heighlight-position-hint #'backward-word))


;; use mode
(defvar position-hint-map
  (let ((keymap (make-sparse-keymap)))
	(define-key keymap (kbd "M-1") 'move-to-position-hint-1)
	(define-key keymap (kbd "M-2") 'move-to-position-hint-2)
	keymap))

 (define-key global-map [remap  forward-word] #'my-forward-word)
 (define-key global-map [remap backward-word] #'my-backward-word)


;; --------------------------------------------------
;; 变量
;; 全局的
;; efvar name "cat" "my awesome name")
;; etq name "dog")
;; 局部
;; (let ...)

;; ---------------------------------------------------
;; 交换选区和剪贴板 和简单的宏实现
(defun mlet* (steps &rest body)
  (let ((step (car steps))
		 (steps (cdr steps)))
	(if step
		`(when-let (, step)
		   (mlet ,steps, @body))
	  `(progn, @body))))

(defmacro mlet (steps &rest body)
  (declare (indent defun))
  (apply #'melt* steps body))

(defun exchange-region-kill-ring-car()
  (interactive)
  (mlet ((m (mark))
		 (_ (region-active-p))
		 (rbeg (region-beginning))
	 	 (rend (region-end))
		 (rep (pop kill-ring))
		 (txt (buffer-substring rbeg rend)))
		(delete-region rbeg rend)
		(push txt kill-ring)
		(let ((p (point)))
		  (insert rep)
		  (push-mark p t t)
		  ((save-excursion )tq deactivate-mark nil)))
  )

;; (global-set-key (kbd "<f9>") #'exchange-region-kill-ring-car)
