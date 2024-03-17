;; init-web.el --- Initialize web configurations.	-*- lexical-binding: t -*-
;;; Commentary:
;;
;; Web configurations.
;;

;;; Code:

(eval-when-compile
  (require '+custom)
   (require 'init-package)
  )

(unless (eq system-type 'windows-nt)
  ;; eww
  (use-package eww
    :ensure nil
    :init
    ;; Install: npm install -g readability-cli
    (when (executable-find "readable")
      (setq eww-retrieve-command '("readable"))))

  ;; Webkit browser
  (use-package xwidget
    :ensure nil
    :if (featurep 'xwidget-internal)
    :bind (("C-c C-z w" . xwidget-webkit-browse-url)
           :map xwidget-webkit-mode-map
           ("h"         . xwidget-hydra/body))
    :pretty-hydra
    ((:title (pretty-hydra-title "Webkit" 'faicon "nf-fa-chrome" :face 'nerd-icons-blue)
      :color amaranth :quit-key ("q" "C-g"))
     ("Navigate"
      (("b" xwidget-webkit-back "back")
       ("f" xwidget-webkit-forward "forward")
       ("r" xwidget-webkit-reload "refresh")
       ("SPC" xwidget-webkit-scroll-up "scroll up")
       ("DEL" xwidget-webkit-scroll-down "scroll down")
       ("S-SPC" xwidget-webkit-scroll-down "scroll down"))
      "Zoom"
      (("+" xwidget-webkit-zoom-in "zoom in")
       ("=" xwidget-webkit-zoom-in "zoom in")
       ("-" xwidget-webkit-zoom-out "zoom out"))
      "Misc"
      (("g" xwidget-webkit-browse-url "browse url" :exit t)
       ("u" xwidget-webkit-current-url "show url" :exit t)
       ("v" xwwp-follow-link "follow link" :exit t)
       ("w" xwidget-webkit-current-url-message-kill "copy url" :exit t)
       ("?" describe-mode "help" :exit t)
       ("Q" quit-window "quit" :exit t)))))

  )

(use-package typescript-mode
  :load-path "~/.emacs.d/extensions/typescript")

;; CSS
(use-package css-mode
  :init (setq css-indent-offset 2))

;; SCSS
(use-package scss-mode
  :init (setq scss-compile-at-save nil))

;; LESS
(unless (fboundp 'less-css-mode)
  (use-package less-css-mode))

;; JSON
(unless (fboundp 'js-json-mode)
  (use-package json-mode
   :load-path "~/.emacs.d/extensions/json-mode"))

;; JavaScript
(use-package js
  :init (setq js-indent-level 2))

(use-package js2-mode
  :mode (("\\.js\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :interpreter (("node" . js2-mode)
                ("node" . js2-jsx-mode))
  :hook ((js2-mode . js2-imenu-extras-mode)
         (js2-mode . js2-highlight-unused-variables-mode))
  :config
  ;; Use default keybindings for lsp
  (when suk-lsp
    (unbind-key "M-." js2-mode-map)))

(use-package rjsx-mode
  :load-path "~/.emacs.d/extensions/rjsx-mode"
  :mode ("\\.js\\'")
  :config
  (add-hook 'rjsx-mode-hook (lambda()
	                          (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
	                          (flycheck-select-checker 'javascript-eslint)))
  )

(use-package emmet-mode
  :defer 3
  :init (setq emmet-expand-jsx-className? t)
  :hook (web-mode typescript-mode js-mode js2-mode rjsx-mode css-mode scss-mode sgml-mode))


;; Format HTML, CSS and JavaScript/JSON
;; Install: npm -g install prettier
(when (executable-find "prettier")
  (use-package prettier
    :diminish
    :hook ((js-mode js2-mode css-mode sgml-mode web-mode) . prettier-mode)
    :init (setq prettier-pre-warm 'none)))
(use-package prettier-js
  :ensure t
  :defer 3
  :hook ((css-mode web-mode typescript-mode js-mode json-mode js2-mode) . prettier-js-mode))



;; Major mode for editing web templates
(use-package web-mode
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

;; Adds node_modules/.bin directory to `exec_path'
(use-package add-node-modules-path
  :hook ((web-mode js-mode js2-mode) . add-node-modules-path))

(provide 'init-lang-web)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-web.el ends here
