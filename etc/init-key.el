(require 'lazy-load)
(provide 'init-key)
;;; ### Unset key ###
;;; --- 卸载按键
(lazy-load-unset-keys                   ;全局按键的卸载
 ;; '("C-z"  "s-W" "s-z" "M-h" "C-\\" "s-c" "s-x" "s-v"))
 '("C-z" ))

(lazy-load-global-keys
 '(
   ("C-c b"  . my-hydra-buffers/body)
   ("M-<f7>" . suk-read-mode)
   ("<f7>"   . olivetti-mode)
   ("C-;"    . suk/close-current-buffer) ;关闭当前buffer
   )
 "buffer-extension")

;; (lazy-load-global-keys
;;  '(
;;     ("<f7>" . olivetti-mode)
;;   )
;;  "olivetti")

;; default keys: C-x LEFT/RIGHT C-, C-.

;; --- 缓存移动
(lazy-load-set-keys
 '(
   ;;("C-z i" . beginning-of-buffer)      ;缓存开始 M-<
   ;;("C-z k" . end-of-buffer)            ;缓存结尾 M->
   ("C-M-f" . forward-paragraph)        ;下一个段落
   ("C-M-b" . backward-paragraph)       ;上一个段落
   ("C-M-y" . backward-up-list)         ;向左跳出 LIST
   ("C-M-o" . up-list)                  ;向右跳出 LIST
   ("C-M-u" . backward-down-list)       ;向左跳进 LIST
   ("C-M-i" . down-list)                ;向右跳进 LIST
   ("C-M-a" . beginning-of-defun)       ;函数开头
   ("C-M-e" . end-of-defun)             ;函数末尾
   ))

(lazy-load-global-keys
 '(
   ("<C-S-up>" . buf-move-up)   
   ("<C-S-down>" . buf-move-down)
   ("<C-S-left>" . buf-move-left)  
   ("<C-S-right>" . buf-move-right)   
   )
 "buffer-move")

;; ### move text ###
(lazy-load-global-keys
 '(
   ("M-S-n" . move-text-down) ;把光标所在的整行文字(或标记)下移一行
   ("M-S-p" . move-text-up)   ;把光标所在的整行文字(或标记)上移一行
   ("M-<DOWN>" . move-text-down)    ;把光标所在的整行文字(或标记)下移一行
   ("M-<UP>"   . move-text-up)    ;把光标所在的整行文字(或标记)上移一行
   )
 "move-text")

;;; ### open new line ###
(lazy-load-global-keys
 '(
   ("C-o" . open-newline-above) ;在上面一行新建一行
   ("C-l" . open-newline-below) ;在下面一行新建一行
   )
 "open-newline")

;; ### duplicate-line ###
(lazy-load-global-keys
 '(
   ("C-S-o" . duplicate-line-or-region-above) ;向上复制当前行或区域
   ("C-S-l" . duplicate-line-or-region-below) ;向下复制当前行或区域
   ("C-S-s-o" . duplicate-line-above-comment) ;复制当前行到上一行, 并注释当前行
   ("C-S-s-l" . duplicate-line-below-comment) ;复制当前行到下一行, 并注释当前行
   ("C-:" . comment-or-uncomment-region+)     ;注释当前行

   )
 "duplicate-line")

;;; ### Delete block ###
;;; --- 快速删除光标左右的内容
(lazy-load-global-keys
 '(
   ("M-," . delete-block-backward)
   ("M-." . delete-block-forward))
 "delete-block")

;; ### String Inflection ###
;; --- 单词语法风格快速转换
(lazy-load-global-keys
 '(
   ("C-c C-u" . string-inflection-hydra/body)
   )
 "init-string-inflection")

;;; ### Thing-edit ###
;;; --- 增强式编辑当前光标的对象
(lazy-load-global-keys
 '(
   ("C-c w" . thing-copy-word)
   ("C-c s" . thing-copy-symbol)
   ("C-c m" . thing-copy-email)
   ("C-c f" . thing-copy-filename)
   ("C-c u" . thing-copy-url)
   ("C-c x" . thing-copy-sexp)
   ("C-c g" . thing-copy-page)
   ("C-c t" . thing-copy-sentence)
   ("C-c o" . thing-copy-witespace)
   ("C-c i" . thing-copy-list)
   ("C-c c" . thing-copy-comment)
   ("C-c h" . thing-copy-defun)
   ("C-c p" . thing-copy-parentheses)
   ("C-c l" . thing-copy-line)
   ("C-c a" . thing-copy-to-line-begining)
   ("C-c e" . thing-copy-to-line-end)
   ("C-c W" . thing-cut-word)
   ("C-c S" . thing-cut-symbol)
   ("C-c M" . thing-cut-email)
   ("C-c F" . thing-cut-filename)
   ("C-c G" . thing-cut-page)
   ("C-c T" . thing-cut-sentence)
   ("C-c O" . thing-cut-whitespace)
   ("C-c I" . thing-cut-list)
   ("C-c C" . thing-cut-comment)
   ("C-c H" . thing-cut-defun)
   ("C-c P" . thing-cut-parentheses)
   ("C-c L" . thing-cut-line)
   ("C-c A" . thing-cut-to-line-beginning)
   ("C-c E" . thing-cut-to-line-end)
   )
 "thing-edit"
 "C-z"
 )

;; ### Buffer Edit ###
;; --- 缓存编辑
  (lazy-load-set-keys
   '(
     ("C-x C-x" . exchange-point-and-mark)   ;交换当前点和标记点
     ("M-o" . backward-delete-char-untabify) ;向前删除字符
     ("C-M-S-h" . mark-paragraph)            ;选中段落
     ("M-SPC" . just-one-space)              ;只有一个空格在光标处
     ))

;;; ### basic-toolkit ###
(lazy-load-global-keys
 '(
   ("M-s-n" . comment-part-move-down)   ;向下移动注释
   ("M-s-p" . comment-part-move-up)     ;向上移动注释
   ("C-s-n" . comment-dwim-next-line)   ;移动到上一行并注释
   ("C-s-p" . comment-dwim-prev-line)   ;移动到下一行并注释
   ("M-2" . indent-buffer)              ;自动格式化当前Buffer
   ("M-z" . upcase-char)                ;Upcase char handly with capitalize-word
   ;;("C-x u" . mark-line)              ;选中整行
   ("s-k" . kill-and-join-forward)      ;在缩进的行之间删除
   ("M-G" . goto-column)                ;到指定列
   ("C->" . remember-init)              ;记忆初始函数
   ("C-<" . remember-jump)              ;记忆跳转函数
   ("M-s-," . point-stack-pop)          ;buffer索引跳转
   ("M-s-." . point-stack-push)         ;buffer索引标记
   ("s-g" . goto-percent) ;跳转到当前Buffer的文本百分比, 单位为字符
   ("M-I" . backward-indent)            ;向后移动4个字符
                                        ;   ("s-J" . scroll-up-one-line)         ;向上滚动一行
                                        ;   ("s-K" . scroll-down-one-line)       ;向下滚动一行
   ("<f2>" . refresh-file)              ;自动刷新文件
   ("s-f" . find-file-root)             ;用root打开文件
   ("s-r" . find-file-smb)              ;访问sambao
   ("C-S-j" . join-lines)               ;连接下行
   ("M-j"   . join-line)                ;连接上行
   ("M-c" . endless/capitalize)
   ("M-l" . endless/downcase)
   ("M-u" . endless/upcase)
   )
 "basic-toolkit")

(lazy-load-global-keys
       '(("M-=" . er/expand-region)
         ("M--" . er/contract-region)
         )
       "expand-region")

(if emacs/>=28p
    (lazy-load-global-keys
     '(("C-x u" . vundo)
       ("C-/" . vundo)
       )
     "vundo")
  (lazy-load-global-keys
   '(("C-x u" . undo-trees)
     ("C-/"   . undo-tree-undo)
     ("C-?  " . undo-tree-redo)
     )
   "undo-tree")
  )

;;; --- 笔记管理和组织
(define-prefix-command 'F9-map)
(global-set-key (kbd "<f9>") 'F9-map)
(lazy-load-global-keys
 '(("a" . org-agenda)
   ("A" . org-attach)
   ("s" . show-org-agenda)
   ("c" . org-capture)
   ("i" . org-toggle-inline-images)
   ("l" . org-toggle-link-display)
   ("d" . calendar)
   ("f" . suk/file-shortcuts/body)

   )
 "init-org"
 "<f9>")

  ;; I use C-c c to start capture mode
  (global-set-key (kbd "C-c c") #'org-capture)
  ;; ;; (global-set-key (kbd "C-c C") 'org-capture)
  (global-set-key "\C-cl" #'org-store-link)
  (global-set-key "\C-ca" #'org-agenda)
  ;;(global-set-key "\C-cb" #'org-iswitchb)

  ;; C-',  C-, is org-cycle-agenda-files keys
  ;; 新版的org-mode使用C-c C-, 替换了 <sTAB 提供的模板功能。

(lazy-load-set-keys
  '(
    ([S-f6] . hs-minor-mode)
    ("C-c ." . hs-toggle-hiding)
    ("C-c ," . hs-show-all)
    )
  )
