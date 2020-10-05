;; init-utils.el --- Initialize ultilities.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Suk

;; Author: Suk

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Some usefule Utilities.
;;

;;; Code:

(eval-when-compile
  (require '+const)
  (require '+custom))

(use-package hydra
  :ensure t)

(use-package hydra-posframe
   :load-path "~/.emacs.d/site-lisp/hydra-posframe/hydra-posframe.el"
   :hook (after-init . hydra-posframe-mode))

(use-package major-mode-hydra
  :ensure t
  :after hydra)

;; Rectangle
(use-package rect
  :ensure nil
  :bind (("<C-return>" . rectangle-mark-mode)))

;; 跳转窗口
;; (use-package ace-window
;;   :ensure t
;;   :init
;;   (progn
;;     (global-set-key [remap other-window] 'ace-window)
;; 	;; 设置标记
;;     (custom-set-faces
;;      '(aw-leading-char-face
;;        ((t (:inherit ace-jump-face-foreground :height 3.0 :foreground "magenta")))))))

;; On-the-fly spell checker
(use-package flyspell
  :ensure nil
  :diminish flyspell-mode
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (unbind-key "C-;" flyspell-mode-map)
                            (unbind-key "C-," flyspell-mode-map)
                            (unbind-key "C-." flyspell-mode-map))))
  :init
  (setq flyspell-issue-message-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

;; Framework for mode-specific buffer indexes
(use-package imenu
  :ensure nil
  :bind (("C-." . imenu)))

;; Treat undo history as a tree
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t undo-tree-enable-undo-in-region nil undo-tree-auto-save-history nil)
  ;; HACK: keep the diff window
  (with-no-warnings
	(make-variable-buffer-local 'undo-tree-visualizer-diff)
	(setq-default undo-tree-visualizer-diff t))

  )

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :bind ("C-x C-b" . ibuffer)
  :hook ((ibuffer . (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                        (ibuffer-do-sort-by-alphabetic)))))
  :config
  (setq ibuffer-projectile-prefix "Project: ")
  (setq ibuffer-filter-group-name-face 'font-lock-function-name-face)
  (with-eval-after-load 'counsel
    (defun my-ibuffer-find-file (file &optional wildcards)
      "Like `find-file', but default to the directory of the buffer at point."
      (interactive
       (let ((default-directory (let ((buf (ibuffer-current-buffer)))
                                  (if (buffer-live-p buf)
                                      (with-current-buffer buf
                                        default-directory)
                                    default-directory))))
         (counsel-find-file))))
    (advice-add #'ibuffer-find-file :override #'my-ibuffer-find-file)))


;; Hideshow
;; 折叠和收缩代码
(use-package hideshow 
  :ensure t 
  :diminish hs-minor-mode 
  :bind (:map prog-mode-map
              ("C-c TAB" . hs-toggle-hiding) 
              ("C-c p +" . hs-show-all)
              ) 
  :hook (prog-mode . hs-minor-mode))


;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :custom
  ;; 弹出方式，底部弹出
  (which-key-popup-type 'side-window)
  :config
  (which-key-mode)
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :hook (after-init . which-key-mode))

(use-package which-key-posframe
  :ensure t
  :load-path "~/.emacs.d/site-lisp/which-key-posframe/which-key-posframe.el"
  :config
  (which-key-posframe-mode))

;; Youdao Dictionay
(use-package youdao-dictionary
  :defer 2 
  :ensure t 
  :bind (("C-x y y" . youdao-dictionary-search-at-point)
		 ("C-x y Y" . youdao-dictionary-search-at-point-tooltip)
		 ("C-x y g" . 'youdao-dictionary-search-at-point-posframe)
		 ("C-x y p" . 'youdao-dictionary-play-voice-at-point)
		 ("C-x y r" . 'youdao-dictionary-search-and-replace)
		 ("C-x y i" . 'youdao-dictionary-search-from-input))
  :config
  ;; Cache documents
  (setq url-automatic-caching t)
  (which-key-add-key-based-replacements "C-x y" "有道翻译") 
  ;; Enable Chinese word segmentation support (支持中文分词)
  (setq youdao-dictionary-use-chinese-word-segmentation t))

;; Open files as another user
(unless sys/win32p
  (use-package sudo-edit))

;; Tramp
;;(use-package docker-tramp)


;; Persistent the scratch buffer
(use-package persistent-scratch
  :preface
  (defun my-save-buffer ()
    "Save scratch and other buffer."
    (interactive)
    (let ((scratch-name "*scratch*"))
      (if (string-equal (buffer-name) scratch-name)
          (progn
            (message "Saving %s..." scratch-name)
            (persistent-scratch-save)
            (message "Wrote %s" scratch-name))
        (save-buffer))))
  :hook (after-init . persistent-scratch-setup-default)
  :bind (:map lisp-interaction-mode-map
              ("C-x C-s" . my-save-buffer)))

(use-package daemons)                   ; system services/daemons
(use-package diffview)                  ; side-by-side diff view
(use-package htmlize)                   ; covert to html
(use-package list-environment)

;; emacs 调用 rime输入法的前端，强烈推荐
;; 		 (use-package rime
;; 		   :ensure t
;; 		   :custom
;; 		   (default-input-method "rime")
;; 		   :config
;; 		   (setq rime-user-data-dir "~/.config/fcitx/rime")

;; 		   (setq rime-posframe-properties
;; 				 (list :background-color "#333333"
;; 					   :foreground-color "#dcdccc"
;; 					   :font "Sarasa Mono SC-16"
;; 					   :internal-border-width 10))
;; 		   (setq default-input-method "rime"
;; 				 rime-show-candidate 'posframe))


;; (if (file-exists-p "~/.emacs.d/site-lisp/emacs-application-framework") 
;; 		   (use-package eaf
;; 			 :load-path "~/.emacs.d/site-lisp/emacs-application-framework" 
;; 			 :custom (eaf-find-alternate-file-in-dired t)
;; 			 (eaf-proxy-type "socks5")
;; 			 (eaf-proxy-host "127.0.0.1")
;; 			 (eaf-proxy-port "1088")
;; 			 (browse-url-browser-function 'eaf-open-browser)
;; 			 :config
;; 			 (defalias 'browse-web #'eaf-open-browser)
;; 			 (setq eaf-grip-token "32872f2ccde165e5d36548619681c7b7e7ec8793")
;; 			 (eaf-setq eaf-pdf-dark-mode "true")
;; 			 (eaf-setq eaf-browser-dark-mode "true") 
;; 			 (eaf-setq eaf-mindmap-dark-mode "true")
;; 			 (eaf-setq eaf-browser-enable-adblocker "true")
;; 			 (when (and
;; 					(> (car (circadian-now-time)) (car (circadian-sunrise)))
;; 					(< (car (circadian-now-time)) (car (circadian-sunset))))
;; 			   (progn
;; 				 (eaf-setq eaf-pdf-dark-mode "false")
;; 				 (eaf-setq eaf-browser-dark-mode "false") 
;; 				 (eaf-setq eaf-mindmap-dark-mode "false")))
;; 			 (eaf-setq eaf-browser-default-zoom "1.2")
;; 			 (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding) 
;; 			 (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding) 
;; 			 (eaf-bind-key take_photo "p" eaf-camera-keybinding))
;; 		 (message
;; 		  "你需要下载emacs-application-framework到~/.emacs.d/site-lisp/emacs-application-framework.才能启用EAF"))



;; Emacs下telegram的客户端，默认不开启
;;  tdlib :: telega需要
;; ArchLinux #archlinuxcn
;;	pacman -S libtd-git
;;(use-package 
 ;;  telega
 ;;  :ensure nil
 ;;  :commands telega 
 ;;  :init (setq telega-proxies 
 ;;              '((:server "localhost" 
 ;;                         :port 1088
 ;;                         :enable t 
 ;;                         :type (:@type "proxyTypeSocks5"
 ;;                                       )
 ;;                         ))) 
 ;;  (setq telega-chat-fill-column 65) 
 ;;  (setq telega-emoji-use-images nil) 
 ;;  :config
 ;;  (set-fontset-font t 'unicode (font-spec :family "Symbola") nil 'prepend) 
 ;;  (with-eval-after-load 'company (add-hook 'telega-chat-mode-hook (lambda () 
 ;;                                                                    (make-local-variable 'company-backends) 
 ;;                                                                    (dolist (it '(telega-company-botcmd telega-company-emoji)) 
 ;;                                                                      (push it company-backends))))) 
 ;;  (with-eval-after-load 'all-the-icons (add-to-list 'all-the-icons-mode-icon-alist '(telega-root-mode all-the-icons-fileicon "telegram" 
 ;;                                                                                                      :heigt 1.0 
 ;;                                                                                                      :v-adjust -0.2 
 ;;                                                                                                      :face all-the-icons-yellow)) 
 ;;                        (add-to-list 'all-the-icons-mode-icon-alist '(telega-chat-mode all-the-icons-fileicon "telegram" 
 ;;                                                                                       :heigt 1.0 
 ;;                                                                                       :v-adjust -0.2 
 ;;                                                                                       :face all-the-icons-blue))) 
 ;;  (telega-notifications-mode 1) 
 ;;  (telega-mode-line-mode 1))

 
;; Emacs下最好用的终端仿真器，需要编译库，默认不开启
;; libvterm
;; Ubuntu/Debian
;; apt install libvterm
;; ArchLinux
;; pacman -S libvterm
(use-package 
  vterm
  :ensure t
  :defer 2 
  :bind (:map leader-key
              ("o t" . 'vterm)))



;; Emacs下的pdf查看工具，默认非图形化不开启
(when (display-graphic-p)
  (use-package pdf-tools 
	:ensure t
	:hook ('doc-view-mode 'pdf-view-mode)))


;; 窗口管理器
(use-package windmove 
  :ensure t 
  :init (windmove-default-keybindings) 
  :config 
  :bind (:map leader-key
              ("w f" . #'windmove-right) 
              ("w b" . #'windmove-left) 
              ("w p" . #'windmove-up) 
              ("w n" . #'windmove-down) 
              ("w F" . #'window-move-right) 
              ("w B" . #'window-move-left) 
              ("w P" . #'window-move-up) 
              ("w N" . #'window-move-down)
              ("w h" . #'enlarge-window-horizontally)
              ("w l" . #'shrink-window-horizontally)
              ("w j" . #'enlarge-window)
              ("w k" . #'shrink-window)))
;; 折叠和收缩代码
(use-package hideshow 
  :ensure t 
  :diminish hs-minor-mode 
  :bind (:map prog-mode-map
              ("C-c TAB" . hs-toggle-hiding) 
              ("C-c p +" . hs-show-all)
              ) 
  :hook (prog-mode . hs-minor-mode))

;; 一个可以临时安装使用插件的插件
(use-package try 
  :ensure t)


;; 谷歌翻译，
(use-package google-translate
  :disabled
  ;;:ensure t
  :config (setq google-translate--tkk-url "http://translate.google.cn/" google-translate-base-url "http://translate.google.cn/translate\_a/single" google-translate-listen-url "https://translate.google.cn/translate\_tts" google-translate-default-target-language "zh-CN" google-translate-default-source-language "en"))

(use-package eww 
  :ensure t 
  :custom (eww-search-prefix "https://google.com/search?q="))

;; 看英语文档神器
(use-package english-teacher
  :load-path "~/.emacs.d/site-lisp/english-teacher"
  :custom
  (english-teacher-backend 'baidu)
  (english-teacher-show-result-function 'english-teacher-eldoc-show-result-function)
  :hook ((Info-mode
		  elfeed-show-mode
		  eww-mode
		  Man-mode
		  Woman-mode
		  help-mode) . english-teacher-follow-mode))



(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
