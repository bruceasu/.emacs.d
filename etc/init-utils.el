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

;; Rectangle
(use-package rect
  :ensure nil
  :bind (("<C-return>" . rectangle-mark-mode)))

;; Jump to Chinese characters
(use-package ace-pinyin
  :diminish ace-pinyin-mode
  :hook (after-init . ace-pinyin-global-mode))

;; 跳转窗口
(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key [remap other-window] 'ace-window)
	;; 设置标记
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-face-foreground :height 3.0 :foreground "magenta")))))))

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

;; Goto last change
(use-package goto-chg
  :bind ("C-," . goto-last-change)
  :config (advice-add #'goto-last-change :after #'recenter))

;; Framework for mode-specific buffer indexes
(use-package imenu
  :ensure nil
  :bind (("C-." . imenu)))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; Windows-scroll commands
(use-package pager
  :bind (("\C-v"   . pager-page-down)
         ([next]   . pager-page-down)
         ("\ev"    . pager-page-up)
         ([prior]  . pager-page-up)
         ([M-up]   . pager-row-up)
         ([M-kp-8] . pager-row-up)
         ([M-down] . pager-row-down)
         ([M-kp-2] . pager-row-down)))

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
(use-package hideshow
  :ensure nil
  :bind (:map hs-minor-mode-map
              ("C-`" . hs-toggle-hiding))
  :diminish hs-minor-mode)


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


;; 强大的字符跳转工具
(use-package avy 
  :ensure t
  :bind (("M-g :" . 'avy-goto-char)
         ("M-g '" . 'avy-goto-char-2)
         ("M-g \"" . 'avy-goto-char-timer)
         ("M-g f" . 'avy-goto-line)
         ("M-g w" . 'avy-goto-word-1)
         ("M-g e" . 'avy-goto-word-0)))

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


(if (file-exists-p "~/.emacs.d/site-lisp/emacs-application-framework") 
		   (use-package eaf
			 :load-path "~/.emacs.d/site-lisp/emacs-application-framework" 
			 :custom (eaf-find-alternate-file-in-dired t)
			 (eaf-proxy-type "socks5")
			 (eaf-proxy-host "127.0.0.1")
			 (eaf-proxy-port "1088")
			 (browse-url-browser-function 'eaf-open-browser)
			 :config
			 (defalias 'browse-web #'eaf-open-browser)
			 (setq eaf-grip-token "32872f2ccde165e5d36548619681c7b7e7ec8793")
			 (eaf-setq eaf-pdf-dark-mode "true")
			 (eaf-setq eaf-browser-dark-mode "true") 
			 (eaf-setq eaf-mindmap-dark-mode "true")
			 (eaf-setq eaf-browser-enable-adblocker "true")
			 (when (and
					(> (car (circadian-now-time)) (car (circadian-sunrise)))
					(< (car (circadian-now-time)) (car (circadian-sunset))))
			   (progn
				 (eaf-setq eaf-pdf-dark-mode "false")
				 (eaf-setq eaf-browser-dark-mode "false") 
				 (eaf-setq eaf-mindmap-dark-mode "false")))
			 (eaf-setq eaf-browser-default-zoom "1.2")
			 (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding) 
			 (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding) 
			 (eaf-bind-key take_photo "p" eaf-camera-keybinding))
		 (message
		  "你需要下载emacs-application-framework到~/.emacs.d/site-lisp/emacs-application-framework.才能启用EAF"))

;; Emacs下telegram的客户端，默认不开启
;;  tdlib :: telega需要
;; ArchLinux #archlinuxcn
;;	pacman -S libtd-git
(use-package 
  telega
  :ensure nil
  :commands telega 
  :init (setq telega-proxies 
              '((:server "localhost" 
                         :port 1088
                         :enable t 
                         :type (:@type "proxyTypeSocks5"
                                       )
                         ))) 
  (setq telega-chat-fill-column 65) 
  (setq telega-emoji-use-images nil) 
  :config
  (set-fontset-font t 'unicode (font-spec :family "Symbola") nil 'prepend) 
  (with-eval-after-load 'company (add-hook 'telega-chat-mode-hook (lambda () 
                                                                    (make-local-variable 'company-backends) 
                                                                    (dolist (it '(telega-company-botcmd telega-company-emoji)) 
                                                                      (push it company-backends))))) 
  (with-eval-after-load 'all-the-icons (add-to-list 'all-the-icons-mode-icon-alist '(telega-root-mode all-the-icons-fileicon "telegram" 
                                                                                                      :heigt 1.0 
                                                                                                      :v-adjust -0.2 
                                                                                                      :face all-the-icons-yellow)) 
                        (add-to-list 'all-the-icons-mode-icon-alist '(telega-chat-mode all-the-icons-fileicon "telegram" 
                                                                                       :heigt 1.0 
                                                                                       :v-adjust -0.2 
                                                                                       :face all-the-icons-blue))) 
  (telega-notifications-mode 1) 
  (telega-mode-line-mode 1))

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


;; Emacs下的音乐播放器，自带一个函数将~/Music下的所有音乐导入进Bongo再自动播放(bongo-init)
(use-package 
  bongo
  :ensure t
  :hook (after-init . bongo)
  :custom
  (bongo-mode-line-icon-size 10)
  :config
  (setq bongo-header-line-mode nil)
  (setq bongo-mode-line-indicator-mode nil)
  (setq bongo-global-lastfm-mode nil)
  (defun bongo-init () 
            (interactive) 
            (let ((buffer (current-buffer))) 
              (bongo) 
              (setq bongo-insert-whole-directory-trees "ask") 
              (bongo-insert-file "~/Music") 
              (bongo-insert-enqueue-region (point-min) 
                                           (point-max)) 
              (bongo-play-random) 
              (switch-to-buffer buffer)))
  :bind (:map leader-key
              ("b RET" . 'bongo-dwim) 
              ("b i" . 'bongo-init) 
              ("b x" . 'bongo-kill-region) 
              ("b d" . 'bongo-kill-line) 
              ("b _" . 'bongo-undo) 
              ("b SPC" . 'bongo-pause/resume) 
              ("b TAB" . 'bongo-toggle-collapsed) 
              ("b h" . 'bongo-seek-backward-10) 
              ("b l" . 'bongo-seek-forward-10) 
              ("b a" . 'bongo-insert-enqueue) 
              ("b n" . 'bongo-play-next) 
              ("b p" . 'bongo-play-previous) 
              ("b r" . 'bongo-play-random) 
              ("b s" . 'bongo-sprinkle)))

;; Emacs下的pdf查看工具，默认非图形化不开启
(push '(use-package 
		 pdf-tools 
		 :ensure t 
		 :hook ('doc-view-mode 'pdf-view-mode)) graphic-only-plugins-setting)


;; 窗口管理器
(use-package 
  windmove 
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
(use-package 
  hideshow 
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
(use-package 
  google-translate
  :disabled
  :config (setq google-translate--tkk-url "http://translate.google.cn/" google-translate-base-url "http://translate.google.cn/translate\_a/single" google-translate-listen-url "https://translate.google.cn/translate\_tts" google-translate-default-target-language "zh-CN" google-translate-default-source-language "en"))

;; 工作区
(use-package 
  perspeen
  :diminish 
  :ensure t 
  :init
  ;; (setq perspeen-use-tab t)
  (setq perspeen-keymap-prefix [C-tab]) 
  :config (perspeen-mode))

;; 快速查询你的问题
(use-package 
  howdoyou 
  :ensure t 
  :hook (after-init . howdoyou-mode))

;; emacs内置网页浏览器
(use-package 
  eww 
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

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         25)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-persp
  :after treemacs persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(provide 'init-treemacs)


(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
