(require-package 'transwin)
(require-package 'ace-window)
(require-package 'popper)

;;(require-package 'git-timemachine)
(require-package 'exec-path-from-shell)
(require-package 'findr) ;; a light file search tools.
(require-package 'find-by-pinyin-dired)
(require-package 'jump)

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
;;(require-package 'lua-mode)
(require-package 'yaml-mode)
(require-package 'js2-mode)
(require-package 'rjsx-mode) ; use my package in extensions
(require-package 'csv-mode)
                                        ;(require-package 'emmet-mode)
;;(require-package 'groovy-mode)
;; magit sometime use packages which not released yet
;; so we place it at the end to make sure other packages are installed first
(require-package 'graphql-mode)
;;(require-package 'auto-yasnippet)
(require-package 'typescript-mode)
(require-package 'nvm)

;;(require-package 'elpy) ;; python
;;(require-package 'request) ;; a http client
;;(require-package 'websocket) ; for debug debugging of browsers
;;(require-package 'simple-httpd)
;;(require-package 'highlight-symbol)
;;(require-package 'cpputils-cmake)
;;(require-package 'rust-mode)
;;(require-package 'cmake-mode)
;;(require-package 'sage-shell-mode)

(require-package 'sudo-edit)
(require-package 'pdf-tools) ;; use the package in extension
