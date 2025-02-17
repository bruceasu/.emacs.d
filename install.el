(require-package 'transwin)
(require-package 'ace-window)
(require-package 'popper)

;;在 Windows 上，如果你用 Emacs GUI 版本，默认有时会找不到 PATH 中的程序，
;;可以在 Emacs 配置里手动添加环境变量，或者使用
;; exec-path-from-shell 等包（但这个包通常在 macOS/Linux下更常用；
;;Windows 下可能需要手工设置 exec-path）。
(require-package 'exec-path-from-shell)
;; (setq exec-path (append exec-path '("C:/path/to/python" "C:/path/to/node")))
;; (setenv "PATH" (concat (getenv "PATH") ";C:\\path\\to\\python;C:\\path\\to\\node"))




;; (require-package 'counsel) ; counsel => swiper => ivy
;; (require-package 'counsel-bbdb)
;; (require-package 'counsel-gtags)
;; (require-package 'counsel-css)
(require-package 'bbdb)

(require-package 'ivy)
(require-package 'ivy-posframe)
(require-package 'find-file-in-project)
(require-package 'swiper)

(require-package 'pinyinlib)
(require-package 'ace-pinyin)
(require-package 'goto-chg)
(require-package 'avy)
(require-package 'avy-zap)

(require-package 'company)
(require-package 'company-box)

(require-package 'which-key)

;; efficiency
(require-package 'helpful)
(require-package 'wc-mode)
(require-package 'ws-butler)
(require-package 'async)
(require-package 'popup) ; some old package need it
(require-package 'htmlize) ; prefer stable version
(require-package 'diminish)
(require-package 'unfill)
(require-package 'rainbow-delimiters)

;; 在WIndows下效率极低，速度好慢，冇必要使用。

(require-package 'magit)
(require-package 'fringe-helper)
(require-package 'git-gutter) ; dependent to fringe-helper
(require-package 'git-modes)
(require-package 'web-mode)
(require-package 'yaml-mode)
(require-package 'js2-mode)
(require-package 'rjsx-mode) ; use my package in extensions
(require-package 'csv-mode)
(require-package 'graphql-mode)
(require-package 'typescript-mode)
(require-package 'nvm)

(require-package 'sudo-edit)
(require-package 'pdf-tools) ;; use the package in extension
