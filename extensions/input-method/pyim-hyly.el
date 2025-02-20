;;; pyim-hyly.el --- Some hyly dicts for pyim      -*- lexical-binding: t; -*-

;;; Code:
;; * 代码

(require 'pyim)
(require 'pyim-dict)
(require 'pyim-scheme)
(require 'pyim-cregexp-utils)


(pyim-scheme-add
 '(hyly
   :document "胡言乱语输入法。"
   :class xingma
   :first-chars "abcdefghijklmnopqrstuvwxyz;"
   :rest-chars "abcdefghijklmnopqrstuvwxyz;"
   :code-prefix "hyly/" ;词库中所有的 code 都以 "hyly/" 开头，防止和其它词库冲突。
   :code-prefix-history (".") ;词库以前使用 "." 做为 code-prefix.
   :code-split-length 4 ;默认将用户输入切成 4 个字符长的 code 列表（不计算 code-prefix）
   :code-maximum-length 5 ;词库中，code 的最大长度（不计算 code-prefix）
   :prefer-triggers nil
   :cregexp-support-p t))
   
;; 如果使用 popup page tooltip, 就需要加载 popup 包。
;; (require 'popup nil t)
;; (setq pyim-page-tooltip 'popup)


;; popon 包来绘制选词框
;; (require 'popon)
;; (setq pyim-page-tooltip 'popon)
   
;; 使用 posframe 来绘制选词框
(require 'posframe)
;;(setq pyim-page-tooltip 'posframe)

;; 按照优先顺序自动选择一个可用的 tooltip
(setq pyim-page-tooltip '(posframe popup minibuffer))      

;; (setq pyim-page-style 'one-line)

;; 如果使用 pyim-dregcache dcache 后端，就需要加载 pyim-dregcache 包。
;; (require 'pyim-dregcache)
;; (setq pyim-dcache-backend 'pyim-dregcache)


;; 显示 5 个候选词。
(setq pyim-page-length 5)
;; 云输入
;; (setq pyim-cloudim 'baidu)
;; (setq pyim-cloudim 'google)

;; 设置 pyim 探针
;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
;; 我自己使用的中英文动态切换规则是：
;; 1. 光标只有在注释里面时，才可以输入中文。
;; 2. 光标前是汉字字符时，才能输入中文。
;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
(setq-default pyim-english-input-switch-functions
               '(pyim-probe-dynamic-english
                 pyim-probe-isearch-mode
                 pyim-probe-program-mode
                 pyim-probe-org-structure-template))

(setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning
                pyim-probe-punctuation-after-punctuation))

;; 金手指设置，可以将光标处的编码（比如：拼音字符串）转换为中文。
(global-set-key (kbd "M-j") 'pyim-convert-string-at-point)
;; 按 "C-<return>" 将光标前的 regexp 转换为可以搜索中文的 regexp.
(define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)

;; (define-key pyim-mode-map ";"
;;   (lambda ()
;;     (interactive)
;;     (pyim-select-word-by-number 2)))
    

(setq default-input-method "pyim")
(setq pyim-default-scheme 'hyly)
(global-set-key (kbd "C-\\") 'toggle-input-method)
;;(pyim-default-scheme 'hyly)

;; 使用魔术转换器
;; 用户可以将待选词 “特殊处理” 后再 “上屏”，比如 “简体转繁体” 或者 “输入中文，上屏英文” 之类的。
;; 用户需要设置 `pyim-outcome-magic-converter', 比如：下面这个例子实现，输入 “二呆”，
;; “一个超级帅的小伙子” 上屏 :-)

(defun my-converter (string)
  (if (equal string "傻逼")
      "“文明用语”"
    string))
(setq pyim-outcome-magic-converter #'my-converter)

;; (setq-default pyim-punctuation-translate-p '(yes)) ;使用全角标点。
;; (setq-default pyim-punctuation-translate-p '(no))  ;使用半角标点。
(setq-default pyim-punctuation-translate-p '(auto))   ;中文使用全角标点，英文使用半角标点。
   
;; 开启代码搜索中文功能（比如拼音，五笔码等）
(pyim-isearch-mode 1)
(setq-default pyim-english-input-switch-functions
              '(pyim-probe-program-mode 
              pyim-probe-org-seepd-commands
               pyim-rpobe-isearch-mode
                pyim-probe-org-structure-template
                 pyim-probe-dynamic-english))
;;;###autoload
(defun pyim-hyly-enable ()
  "Add huyanluanyu dict to pyim."
  (interactive)
  (let* ((dir (file-name-directory
               (locate-library "pyim-hyly.el")))
         (file (concat dir "pyim-hyly.pyim")))
    (when (file-exists-p file)
      (pyim-extra-dicts-add-dict
       `(:name "hyly-dict" :file ,file :elpa t)))))

;; * Footer
(provide 'pyim-hyly)

;; | 输入法快捷键          | 功能                       |
;; |-----------------------+----------------------------|
;; | C-n 或 M-n 或 + 或 .  | 向下翻页                   |
;; | C-p 或 M-p 或 - 或 ,  | 向上翻页                   |
;; | C-f                   | 选择下一个备选词           |
;; | C-b                   | 选择上一个备选词           |
;; | SPC                   | 确定输入                   |
;; | RET 或 C-m            | 字母上屏                   |
;; | C-c                   | 取消输入                   |
;; | C-g                   | 取消输入并保留已输入的中文 |
;; | TAB                   | 模糊音调整                 |
;; | DEL 或 BACKSPACE      | 删除最后一个字符           |
;; | C-DEL 或  C-BACKSPACE | 删除最后一个拼音           |
;; | M-DEL 或  M-BACKSPACE | 删除最后一个拼音           |
;; | F1,F2,F3,F4           | 以词定字                   |


;; 手动加词和删词
;;1. `pyim-convert-string-at-point' 金手指命令，可以比较方便的添加和删除词条，比如：
;;   1. 在 "你好" 后面输入2, 然后运行金手指命令，可以将 “你好” 加入个人词库。
;;   2. 在 “你好” 后面输入2-, 然后运行金手指命令，可以将 “你好” 从个人词库删除。
;;   3. 如果用户选择了一个词条，则运行金手指命令可以将选择的词条加入个人词库。
;;2. `pyim-create-Ncchar-word-at-point' 这是一组命令，从光标前提取N个汉字字符组成字
;;   符串，并将其加入个人词库。
;;3. `pyim-outcome-trigger' 以默认设置为例：在 “我爱吃红烧肉” 后输入 “5v”，可以将
;;   “爱吃红烧肉”这个词条保存到用户个人词库。
;;4. `pyim-create-word-from-selection', 选择一个词条，运行这个命令后，就可以将这个
;;   词条添加到个人词库。
;;5. `pyim-delete-word' 从个人词库中删除当前高亮选择的词条。

;;; pyim-hyly.el ends here
