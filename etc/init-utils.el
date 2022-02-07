;; init-utils.el --- Initialize ultilities.    -*- lexical-binding: t -*-

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
  (require '+custom)
  (require 'init-package))


;; 增强了搜索功能
(use-package swiper
  :bind
  (("C-s" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))

(use-package counsel
  :ensure t
  :diminish ivy-mode counsel-mode
  :defines (projectile-completion-system magit-completing-read-function)
  :bind (("C-s" . swiper)
         ("C-S-s" . swiper-all)
         ("C-x C-r" . 'counsel-recentf)
         ("C-x d" . 'counsel-dired)
         ("C-c C-r" . ivy-resume)
         ("C-c v p" . ivy-push-view)
         ("C-c v o" . ivy-pop-view)
         ("C-c v ." . ivy-switch-view)

         :map counsel-mode-map
         ([remap swiper] . counsel-grep-or-swiper)
         ("C-x C-r" . counsel-recentf)
         ("C-x j" . counsel-mark-ring)

         ("C-c L" . counsel-load-library)
         ("C-c P" . counsel-package)
         ("C-c f" . counsel-find-library)
         ("C-c g" . counsel-grep)
         ("C-c h" . counsel-command-history)
         ("C-c i" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c l" . counsel-locate)
         ("C-c r" . counsel-rg)
         ("C-c z" . counsel-fzf)

         ("C-c c L" . counsel-load-library)
         ("C-c c P" . counsel-package)
         ("C-c c a" . counsel-apropos)
         ("C-c c e" . counsel-colors-emacs)
         ("C-c c f" . counsel-find-library)
         ("C-c c g" . counsel-grep)
         ("C-c c h" . counsel-command-history)
         ("C-c c i" . counsel-git)
         ("C-c c j" . counsel-git-grep)
         ("C-c c l" . counsel-locate)
         ("C-c c m" . counsel-minibuffer-history)
         ("C-c c o" . counsel-outline)
         ("C-c c p" . counsel-pt)
         ("C-c c r" . counsel-rg)
         ("C-c c s" . counsel-ag)
         ("C-c c t" . counsel-load-theme)
         ("C-c c u" . counsel-unicode-char)
         ("C-c c w" . counsel-colors-web)
         ("C-c c z" . counsel-fzf)

         ;; Find counsel commands quickly
         ("<f6>" . (lambda ()
                     (interactive)
                     (counsel-M-x "^counsel ")))

         :map ivy-minibuffer-map
         ("C-w" . ivy-yank-word)

         ;; Search at point
         ;; "M-j": word-at-point
         ;; "M-n"/"C-w": symbol-at-point
         ;; Refer to https://www.emacswiki.org/emacs/SearchAtPoint#toc8
         ;; and https://github.com/abo-abo/swiper/wiki/FAQ
         ;; ("C-w" . (lambda ()
         ;;            (interactive)
         ;;            (insert (format "%s" (with-ivy-window (ivy-thing-at-point))))))

         :map counsel-find-file-map
         ("C-h" . counsel-up-directory)

         :map swiper-map
         ("M-%" . swiper-query-replace))
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :config
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers

  (setq ivy-use-selectable-prompt t)
  (setq ivy-use-virtual-buffers t) ; Enable bookmarks and recentf
  (setq ivy-height 10)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-on-del-error-function nil)
  (setq ivy-format-function 'ivy-format-function-arrow)
  (setq ivy-initial-inputs-alist nil)

  (setq ivy-re-builders-alist
        '((read-file-name-internal . ivy--regex-fuzzy)
          (t . ivy--regex-plus)))

  (setq swiper-action-recenter t)
  (setq counsel-find-file-at-point t)
  (setq counsel-yank-pop-separator "\n-------\n")

  ;; Use faster search tools: ripgrep or the silver search
  (let ((command
         (cond
          ((executable-find "rg")
           "rg -i -M 120 --no-heading --line-number --color never '%s' %s")
          ((executable-find "ag")
           "ag -i --noheading --nocolor --nofilename --numbers '%s' %s")
          (t counsel-grep-base-command))))
    (setq counsel-grep-base-command command))

  (when (executable-find "rg")
    (setq counsel-git-cmd "rg --files")
    (setq counsel-rg-base-command
          "rg -i -M 120 --no-heading --line-number --color never %s ."))

  ;; Integration with `projectile'
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy))

  ;; Integration with `magit'
  (with-eval-after-load 'magit
    (setq magit-completing-read-function 'ivy-completing-read))

  ;; Enhance fuzzy matching
  (use-package flx)

  ;; Enhance M-x
  (use-package amx)

  ;; Correcting words with flyspell via Ivy
  (use-package flyspell-correct-ivy
    :after flyspell
    :bind (:map flyspell-mode-map
                ([remap flyspell-correct-word-before-point] . flyspell-correct-previous-word-generic)))

  ;; Ivy integration for Projectile
  (use-package counsel-projectile
    :ensure t
    :hook ((counsel-mode . counsel-projectile-mode))
    :init (counsel-projectile-mode 1)
    :bind (:map leader-key
                ("p" . #'projectile-command-map)))

  ;; Tramp ivy interface
  (use-package counsel-tramp
    :bind (:map counsel-mode-map
                ("C-c c v" . counsel-tramp)))
  )

(use-package hydra
  :ensure t
  :defer 1)

(use-package hydra-posframe
  :load-path "~/.emacs.d/site-lisp/hydra-posframe/hydra-posframe.el"
  :defer 1
  :hook (after-init . hydra-posframe-mode))

(use-package major-mode-hydra
  :ensure t
  :defer 1
  :after hydra)

;; Rectangle
(use-package rect
  :ensure nil
  :defer 2
  :bind (("<C-return>" . rectangle-mark-mode)))



;; On-the-fly spell checker
(unless sys/win32p
  (use-package flyspell
    :ensure nil
    :defer 2
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
  )

;; Treat undo history as a tree, ^x u
(use-package undo-tree
  :ensure t
  :defer 2
  :diminish undo-tree-mode
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil)
  ;; HACK: keep the diff window
  (with-no-warnings
    (make-variable-buffer-local 'undo-tree-visualizer-diff)
    (setq-default undo-tree-visualizer-diff t))
  )

;; Group ibuffer's list by project root
(use-package ibuffer-projectile
  :bind ("C-x C-b" . ibuffer)
  :defer 1
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


;; Display available keybindings in popup
(use-package which-key
  :diminish which-key-mode
  :defer 2
  :custom
  ;; 弹出方式，底部弹出
  (which-key-popup-type 'side-window)
  :config
  (which-key-mode)
  :bind (:map help-map ("C-h" . which-key-C-h-dispatch))
  :hook (after-init . which-key-mode))


;; Open files as another user
(unless sys/win32p
  (use-package sudo-edit))

;; Persistent the scratch buffer
(use-package persistent-scratch
  :defer 2
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

(use-package daemons
  :defer 1)								; system services/daemons
(use-package diffview
  :defer 2)								; side-by-side diff view
(use-package htmlize
  :defer 2)								; covert to html
(use-package list-environment
  :defer 2)
;;(use-package restart-emacs)

;; Emacs下的pdf查看工具，默认非图形化不开启
(when (display-graphic-p)
  ;; Emacs下最好用的终端仿真器，需要编译库，默认不开启
  ;; libvterm
  ;; Ubuntu/Debian
  ;; apt install libvterm
  ;; ArchLinux
  ;; pacman -S libvterm
  (when sys/linuxp
    (use-package vterm
      :ensure t
      :defer 2
      :bind ("M-<f12>" . 'vterm))
    (use-package pdf-tools
      :ensure t
      :defer 2
      :hook ('doc-view-mode 'pdf-view-mode))
    )
  ;; 飘，晃眼
  ;; (use-package which-key-posframe
  ;;   :ensure nil
  ;;   :disabled
  ;;   :load-path "~/.emacs.d/site-lisp/which-key-posframe/which-key-posframe.el"
  ;;   :init
  ;;   (which-key-posframe-mode))

  ;; emacs 调用 rime输入法的前端，强烈推荐
  ;;          (use-package rime
  ;;            :ensure t
  ;;            :custom
  ;;            (default-input-method "rime")
  ;;            :config
  ;;            (setq rime-user-data-dir "~/.config/fcitx/rime")

  ;;            (setq rime-posframe-properties
  ;;                  (list :background-color "#333333"
  ;;                        :foreground-color "#dcdccc"
  ;;                        :font "Sarasa Mono SC-16"
  ;;                        :internal-border-width 10))
  ;;            (setq default-input-method "rime"
  ;;                  rime-show-candidate 'posframe))


  ;; (if (file-exists-p "~/.emacs.d/site-lisp/emacs-application-framework")
  ;;            (use-package eaf
  ;;              :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
  ;;              :custom (eaf-find-alternate-file-in-dired t)
  ;;              (eaf-proxy-type "socks5")
  ;;              (eaf-proxy-host "127.0.0.1")
  ;;              (eaf-proxy-port "1088")
  ;;              (browse-url-browser-function 'eaf-open-browser)
  ;;              :config
  ;;              (defalias 'browse-web #'eaf-open-browser)
  ;;              (setq eaf-grip-token "32872f2ccde165e5d36548619681c7b7e7ec8793")
  ;;              (eaf-setq eaf-pdf-dark-mode "true")
  ;;              (eaf-setq eaf-browser-dark-mode "true")
  ;;              (eaf-setq eaf-mindmap-dark-mode "true")
  ;;              (eaf-setq eaf-browser-enable-adblocker "true")
  ;;              (when (and
  ;;                     (> (car (circadian-now-time)) (car (circadian-sunrise)))
  ;;                     (< (car (circadian-now-time)) (car (circadian-sunset))))
  ;;                (progn
  ;;                  (eaf-setq eaf-pdf-dark-mode "false")
  ;;                  (eaf-setq eaf-browser-dark-mode "false")
  ;;                  (eaf-setq eaf-mindmap-dark-mode "false")))
  ;;              (eaf-setq eaf-browser-default-zoom "1.2")
  ;;              (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
  ;;              (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
  ;;              (eaf-bind-key take_photo "p" eaf-camera-keybinding))
  ;;          (message
  ;;           "你需要下载emacs-application-framework到~/.emacs.d/site-lisp/emacs-application-framework.才能启用EAF"))



  ;; Emacs下telegram的客户端，默认不开启
  ;;  tdlib :: telega需要
  ;; ArchLinux #archlinuxcn
  ;;    pacman -S libtd-git
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

  )



;; 一个可以临时安装使用插件的插件
;; (use-package try
;;   :disabled
;;   :ensure t
;;   :defer 2)

;;; browser
;; (use-package eww
;;   :ensure t
;;   :defer 2
;;   :custom (eww-search-prefix "https://google.com/search?q="))

(provide 'init-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
