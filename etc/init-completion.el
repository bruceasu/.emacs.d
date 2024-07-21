;;; init-completion.el --- Initialize completion configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Modern completion configuration.
;;

;;; Code:

(provide 'init-completion)
(eval-when-compile
  (require '+const)
  (require '+custom)
  (require '+fn)
  (require 'init-package)
  )

(use-package emacs
:init
;; TAB cycle if there are only few candidates
(setq completion-cycle-threshold 3)

;; Only list the commands of the current modes
(when (boundp 'read-extended-command-predicate)
  (setq read-extended-command-predicate
        #'command-completion-default-include-p))

;; Enable indentation+completion using the TAB key.
;; `completion-at-point' is often bound to M-TAB.
(setq tab-always-indent 'complete))

;; ¾ÝËµ¸ú lsp-bridge ³åÍ»
;;(require-package 'company)
;;(require-package 'company-native-complete)
;;(require-package 'company-c-headers)
;;(require-package 'company-statistics)
;;(use-package company
;;  :defer 2
;;  :diminish
;;  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
;;  :hook (after-init . global-company-mode)
;;  :init (setq company-tooltip-align-annotations t
;;              company-idle-delay 0 company-echo-delay 0
;;              company-minimum-prefix-length 1
;;              company-require-match nil
;;              company-dabbrev-ignore-case nil
;;              company-dabbrev-downcase nil
;;              company-show-numbers t)
;;  :config
;;  (setq switch-window-input-style 'minibuffer)
;;  (setq switch-window-increase 4)
;;  (setq switch-window-threshold 2)
;;  (setq switch-window-shortcut-sytle 'querty)
;;  (setq switch-window-qwerty-shortcuts
;;        '("a" "s" "d" "f" "j" "k" "l"))
;;  (setq company-minimum-prefix-length 1)
;;  (setq company-show-quick-access t)
;;  :bind (:map company-active-map
;;              ("C-n" . #'company-select-next)
;;              ("C-p" . #'company-select-previous)
;;              ("TAB" . company-complete-selection)
;;              ("M-h" . company-complete-selection)
;;              ("M-H" . company-complete-common)
;;              ("M-s" . company-search-candidates)
;;              ("M-S" . company-filter-candidates)
;;              ("M-n" . company-select-next)
;;              ("M-p" . company-select-previous))
;;  (:map leader-key
;;        ("c s" . #'company-yasnippet
;;         ))
;;  )
;;(use-package company-box
;;  :ensure nil)

;; Optionally use the `orderless' completion style.
(require-package 'orderless)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; Support Pinyin
(use-package pinyinlib
  :after orderless
  :autoload pinyinlib-build-regexp-string
  :init
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin))

(use-package vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook ((after-init . vertico-mode)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(when (childframe-completion-workable-p)
  (use-package vertico-posframe
    :hook (vertico-mode . vertico-posframe-mode)
    :init (setq vertico-posframe-poshandler
                #'posframe-poshandler-frame-center-near-bottom
                vertico-posframe-parameters
                '((left-fringe  . 8)
                  (right-fringe . 8)))
    )
  )

;; (use-package swiper
;;   :bind
;;   (
;;    ("C-x M-s" . swiper)
;;    ("C-x C-F"  . counsel-find-file)
;;    ("C-x C-M-x" . counsel-M-x)
;;    )
;;   :config
;;   (progn
;;     (ivy-mode 1)
;;     (setq ivy-use-virtual-buffers t)
;;     (setq ivy-display-style 'fancy)
;;     ;;(define-key read-expression-map (kbd "C-r") 'counsel-expression-history))
;;     ))

(use-package nerd-icons-completion
  :when (icons-displayable-p)
  :hook (vertico-mode . nerd-icons-completion-mode))
