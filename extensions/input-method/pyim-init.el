;;; pyim-init.el
(require 'pyim)
(setq default-input-method "pyim")
(setq pyim-default-scheme 'hyly)
(setq pyim-assistant-scheme 'xiaohe-shuangpin)
;; 拼音小词库
;;(use-package pyim-basedict) ; 拼音词库设置，五笔用户 *不需要* 此行设置
;;(pyim-basedict-enable)   ; 拼音词库，五笔用户 *不需要* 此行设置

;; 我使用全拼
;; (setq pyim-default-scheme 'quanpin)
;; 开启拼音搜索功能
;; (setq pyim-isearch-enable-pinyin-search t)
;; 拼音大词库
;; (use-package pyim-greatdict)
;; (pyim-greatdict-enable)

;; 五笔用户使用 wbdict 词库
;; (use-package pyim-wbdict
;;   :ensure nil
;;   :config (pyim-wbdict-gbk-enable))
;; (setq pyim-default-scheme 'wubi')

;; pyim 支持双拼输入模式，用户可以通过变量 `pyim-default-scheme' 来设定：
;; (setq pyim-default-scheme 'pyim-shuangpin)
;; (setq pyim-default-scheme 'xiaohe-shuangpin)
;; 注意：
;; 
;; pyim 支持微软双拼（microsoft-shuangpin）和小鹤双拼（xiaohe-shuangpin）。
;; 用户可以使用变量 `pyim-schemes' 添加自定义双拼方案。
;; 用户可能需要重新设置 `pyim-translate-trigger-char'。

;; 不好, 傻X. 想输入中文时, 自动变英文。
;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;; 我自己使用的中英文动态切换规则是：
;; 1. 光标只有在注释里面时，才可以输入中文。
;; 2. 光标前是汉字字符时，才能输入中文。
;; 3. 使用 M-j 快捷键(容易冲突，改成 M-i)，强制将光标前的拼音字符串转换为中文。
(setq-default pyim-english-input-switch-functions
              '(
                pyim-probe-dynamic-english
                pyim-probe-isearch-mode
                pyim-probe-program-mode
                pyim-probe-org-structure-template))

(setq pyim-isearch-enable-pinyin-search t)

(setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation))

;; 让选词框跟随光标
;; 使用 popup 包来绘制选词框 （emacs overlay 机制）
;;(setq pyim-page-tooltip 'popup)

;; 使用 pupup-el 来绘制选词框, 如果用 emacs26, 建议设置
;; 为 'posframe, 速度很快并且菜单不会变形，不过需要用户
;; 手动安装 posframe 包。
(setq pyim-page-tooltip 'posframe)

;; 调整 tooltip 选词框的显示样式
;; pyim 的 tooltip 选词框默认使用 双行显示 的样式，
;; 在一些特 殊的情况下（比如：popup 显示的菜单错位），用户可以使用
;; 单行显示 的样式：
;; (setq pyim-page-style 'one-line)
;; 注：用户可以添加函数 pyim-page-style-STYLENAME-style 来定义自己的选词框格式。


;; 设置模糊音
;; 可以通过设置 `pyim-fuzzy-pinyin-alist' 变量来自定义模糊音。

;; 手动加词和删词

;; `pyim-create-Ncchar-word-at-point 这是一组命令，从光标前提取N个汉 字字符组成字符串，并将其加入个人词库。
;; `pyim-translate-trigger-char' 以默认设置为例：在“我爱吃红烧肉”后输 入“5v” 可以将“爱吃红烧肉”这个词条保存到用户个人词库。
;; `pyim-create-word-from-selection', 选择一个词条，运行这个命令后，就 可以将这个词条添加到个人词库。
;; `pyim-delete-word' 从个人词库中删除当前高亮选择的词条。

;; 选词框显示3个候选词
(setq pyim-page-length 3)

;; 让 Emacs 启动时自动加载 pyim 词库
;;(add-hook 'emacs-startup-hook
;;          #'(lambda () (pyim-restart-1 t)))



;; active
(global-set-key (kbd "C-\\") 'toggle-input-method)
;; 将光标处的拼音或者五笔字符串转换为中文 (与 vimim 的 “点石成金” 功能类似)
(global-set-key (kbd "M-i") 'pyim-convert-code-at-point)
(global-set-key (kbd "C-;") 'pyim-delete-word-from-personal-buffer)

;; 中文词语没有强制用空格分词，所以 Emacs 内置的命令 `forward-word' 和 `backward-word'
;; 在中文环境不能按用户预期的样子执行，而是 forward/backward “句子” ，pyim
;; 自带的两个命令可以在中文环境下正常工作：
(global-set-key (kbd "M-f") 'pyim-forward-word)
(global-set-key (kbd "M-b") 'pyim-backward-word)
;; ** Tips
;; *** 如何将个人词条导出到一个文件
;; 使用命令：pyim-personal-dcache-export
;; *** pyim 出现错误时，如何开启 debug 模式

;; #+BEGIN_EXAMPLE
;; (setq debug-on-error t)
;; #+END_EXAMPLE


;; 使用魔术转换器
;; 用户可以将待选词条作 “特殊处理” 后再 “上屏”，比如 “简体转繁体” 或者“输入中文，上屏英文” 之类的。

;; 用户需要设置 `pyim-magic-converter', 比如：下面这个例子实现，输入 “二呆”，“一个超级帅的小伙子” 上屏 :-)

;; (defun my-converter (string)
;;   (if (equal string "二呆")
;;       "“一个超级帅的小伙子”"
;;     string))
;; (setq pyim-magic-converter #'my-converter)

;; 切换全角标点与半角标点
;; 第一种方法：使用命令 `pyim-punctuation-toggle'，全局切换。这个命令主要用来设置变量： `pyim-punctuation-translate-p', 用户也可以手动设置这个变量， 比如：

;; (setq pyim-punctuation-translate-p '(yes no auto))   ;使用全角标点。
;; (setq pyim-punctuation-translate-p '(no yes auto))   ;使用半角标点。
;; (setq pyim-punctuation-translate-p '(auto yes no))   ;中文使用全角标点，英文使用半角标点。
;; 第二种方法：使用命令 `pyim-punctuation-translate-at-point' 只切换光标处标点的样式。

;; 第三种方法：设置变量 `pyim-translate-trigger-char' ，输入变量设定的字符会切换光标处标点的样式。
;; 使用 ; 做次选
(define-key pyim-mode-map ";"
  (lambda ()
    (interactive)
    (pyim-page-select-word-by-number 2)))

;; 使用 / 做三选
(define-key pyim-mode-map "/"
  (lambda ()
    (interactive)
    (pyim-page-select-word-by-number 3)))

(require 'pyim-hyly)
(pyim-hyly-enable)

;;'(pyim-dicts
;;  (quote
;;   ((:name "hyly" :file "~/.emacs.d/site-lisp/input-method/pyim-hyly/pyim-hyly-min.pyim"))))


(defun pyim-autoselector-xingma (&rest args)
  "适用于型码输入法的自动上屏器.ARGS.

比如：五笔等型码输入法，重码率很低，90%以上的情况都是选择第一个词
条，自动选择可以减少按空格强制选词的机会。"
  (let* ((scheme-name (pyim-scheme-name))
         (class (pyim-scheme-get-option scheme-name :class))
         (n (pyim-scheme-get-option scheme-name :code-split-length)))
    (when (eq class 'xingma)
      (cond
       ((and (= (length (pyim-entered-get)) n)
             (= (length pyim-candidates) 1))
        '(:select current))
       ((> (length (pyim-entered-get)) n)
        '(:select last))
       (t nil)))))

(provide 'pyim-init)
