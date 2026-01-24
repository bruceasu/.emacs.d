;;在 Windows 上，如果你用 Emacs GUI 版本，默认有时会找不到 PATH 中的程序，
;;可以在 Emacs 配置里手动添加环境变量，或者使用
;; exec-path-from-shell 等包（但这个包通常在 macOS/Linux下更常用；
;;Windows 下可能需要手工设置 exec-path）。
(use-package exec-path-from-shell)
;; (setq exec-path (append exec-path '("C:/path/to/python" "C:/path/to/node")))
;; (setenv "PATH" (concat (getenv "PATH") ";C:\\path\\to\\python;C:\\path\\to\\node"))




;; (use-package counsel) ; counsel => swiper => ivy
;; (use-package counsel-bbdb)
;; (use-package counsel-gtags)
;; (use-package counsel-css)
(use-package bbdb)

(use-package ivy)
(use-package ivy-posframe)
(use-package find-file-in-project)
(use-package swiper)

(use-package pinyinlib)
(use-package ace-pinyin)
(use-package goto-chg)
(use-package avy)
(use-package avy-zap)

(use-package sudo-edit)
