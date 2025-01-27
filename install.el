(require-package 'paredit) ;; useful for lisp
(require-package 'tagedit) ;; useful for html
(require-package 'cliphist)
(require-package 'iedit)
(require-package 'wgrep) ;; eidt the grep / rg result then apply to the origin buffer. Cancel is supportted.

(require-package 'windmove)
(require-package 'transwin)
(require-package 'ace-window)
(require-package 'popper)

;;(require-package 'git-timemachine)
(require-package 'exec-path-from-shell)
(require-package 'findr) ;; a light file search tools.
(require-package 'find-by-pinyin-dired)
(require-package 'jump)

(require-package 'counsel) ; counsel => swiper => ivy
(require-package 'counsel-bbdb)
(require-package 'counsel-gtags)
(require-package 'counsel-css)
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

(require-package 'which-key)

;; efficiency
;;(require-package 'esup)
(require-package 'helpful)
(require-package 'wc-mode)
(require-package 'ws-butler)
(require-package 'async)
;;(require-package 'amx)
(require-package 'popup) ; some old package need it
(require-package 'htmlize) ; prefer stable version
(require-package 'diminish)
;;(require-package 'scratch)

(require-package 'unfill)
(when  sys/linuxp
  (require-package 'eww-lnum) ;; pluin for eww, a built-in web browser
)
(require-package 'rainbow-delimiters)

;; Tools

;;(require-package 'request) ;; a http client
;;(require-package 'websocket) ; for debug debugging of browsers
;;(require-package 'simple-httpd)
;;(require-package 'cpputils-cmake)
;;(require-package 'rust-mode)
;;(require-package 'auto-package-update)
(require-package 'keyfreq)

;; Test tools


;;(unless sys/win32p
;;  (use-package daemons)                 ; system services/daemons
;;  )

;;(use-package bind-key)
;;Enhance M-x, use counsel-M-x

(require-package 'company)
(require-package 'company-box)

;; 在WIndows下效率极低，速度好慢，冇必要使用。
(when sys/linuxp
 (require-package 'magit)
 (require-package 'fringe-helper)
 (require-package 'git-gutter) ; dependent to fringe-helper
 (require-package 'git-modes)
 )

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

;;(require-package 'lsp-mode)
;;(require-package 'lsp-ui)
(require-package 'dap-mode)

(require-package 'sudo-edit)
(require-package 'pdf-tools) ;; use the package in extension
