(require 'thing-edit)
(defhydra hydra-thing-edit (:hint nil)
  "
  ^Copy^                          ^Cut^
  ------------------------------------------
  _w_ Word                         _W_ Word
  _s_ Symbol                       _S_ Symbol
  _m_ Email                        _M_ Email
  _f_ Filename                     _F_ Filename
  _u_ URL                          _G_ Page
  _x_ Sexp                         _T_ Sentence
  _g_ Page                         _O_ Whitespace
  _t_ Sentence                     _I_ List
  _o_ Whitespace                   _C_ Comment
  _i_ List                         _H_ Defun
  _c_ Comment                      _P_ Parentheses
  _h_ Defun                        _L_ Line
  _p_ Parentheses                  _A_ To line beginning
  _l_ Line                         _E_ To line end
  _a_ To line beginning            _n_ Show file name
  _e_ To line end
  "
  ("w" thing-copy-word)
  ("s" thing-copy-symbol)
  ("m" thing-copy-email)
  ("f" thing-copy-filename)
  ("u" thing-copy-url)
  ("x" thing-copy-sexp)
  ("g" thing-copy-page)
  ("t" thing-copy-sentence)
  ("o" thing-copy-whitespace)
  ("i" thing-copy-list)
  ("c" thing-copy-comment)
  ("h" thing-copy-defun)
  ("p" thing-copy-parentheses)
  ("l" thing-copy-line)
  ("a" thing-copy-to-line-beginning)
  ("e" thing-copy-to-line-end)

  ("W" thing-cut-word)
  ("S" thing-cut-symbol)
  ("M" thing-cut-email)
  ("F" thing-cut-filename)
  ("G" thing-cut-page)
  ("T" thing-cut-sentence)
  ("O" thing-cut-whitespace)
  ("I" thing-cut-list)
  ("C" thing-cut-comment)
  ("H" thing-cut-defun)
  ("P" thing-cut-parentheses)
  ("L" thing-cut-line)
  ("A" thing-cut-to-line-beginning)
  ("E" thing-cut-to-line-end)
  ("n" show-file-name)
  
  ("q" nil "Cancel")
  )

(global-set-key (kbd "C-c e") 'hydra-thing-edit/body)

;; 标题显示完整路径
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
;; 复制当前文件绝对路径到剪贴板
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (let ((name (buffer-file-name)))
    (kill-new name)
    (message name)))

(provide 'init-thing-edit)
