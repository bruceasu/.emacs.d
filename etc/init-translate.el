;; init-translate.el --- Initialize translation ultilities.	-*- lexical-binding: t -*-

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
;; Some translation Utilities.
;;

;;; Code:

(eval-when-compile
  (require 'init-package))

(require 'sdcv)
(setq sdcv-say-word-p t)                ;say word after search

(setq sdcv-dictionary-data-dir
      (concat suk-emacs-root-dir "/share/sdcv-dict")) ;设置星际译王本地词典的目录

(setq sdcv-dictionary-simple-list    ;星际译王屏幕取词词典, 简单, 快速
      '("懒虫简明英汉词典"
        "懒虫简明汉英词典"
        "KDic11万英汉词典"))

(setq sdcv-dictionary-complete-list     ;星际译王的词典, 完全, 详细
      '(
        "懒虫简明英汉词典"
        "英汉汉英专业词典"
        "XDICT英汉辞典"
        "stardict1.3英汉辞典"
        "WordNet"
        "XDICT汉英辞典"
        "Jargon"
        "懒虫简明汉英词典"
        "FOLDOC"
        "新世纪英汉科技大词典"
        "KDic11万英汉词典"
        "朗道汉英字典5.0"
        "CDICT5英汉辞典"
        "新世纪汉英科技大词典"
        "牛津英汉双解美化版"
        "21世纪双语科技词典"
        "quick_eng-zh_CN"
        ))


;; 谷歌翻译，
(use-package google-translate
  ;;  :disabled
  :ensure t
  :defer 2
  :config (setq google-translate--tkk-url "http://translate.google.com/"
				google-translate-base-url "http://translate.google.com/translate\_a/single"
				google-translate-listen-url "https://translate.google.com/translate\_tts"
				google-translate-default-target-language "zh-CN"
				google-translate-default-source-language "en")
  (setq-default google-translate-enable-ido-completion t)
  :bind(
		;; 配置快捷键
		("C-x t s" . google-translate-at-point)
		("C-x t q" . google-translate-query-translate)
		("C-x t S" . google-translate-smooth-translate)
  ))
;; A multi dictionaries interface
(use-package fanyi
  :bind (("C-c d f" . fanyi-dwim)
         ("C-c d d" . fanyi-dwim2)
         ("C-c d h" . fanyi-from-history))
  :custom (fanyi-providers '(fanyi-haici-provider fanyi-longman-provider))

  (use-package go-translate
    :bind (("C-c d g" . gts-do-translate))
    :init (setq gts-translate-list '(("en" "zh") ("zh" "en")))))
    
;; Youdao Dictionay
(use-package youdao-dictionary
  :defer 2
  :ensure t
  :bind (
  		("C-c y"   . my-youdao-dictionary-search-at-point)
         ("C-c d Y" . youdao-dictionary-search-async)
         ("C-c d y" . youdao-dictionary-search-at-point)
		 ;;("C-x y Y" . youdao-dictionary-search-at-point-tooltip)
		 ;;("C-c d p" . 'youdao-dictionary-search-at-point-posframe)
		 ;;("C-c d v" . 'youdao-dictionary-play-voice-at-point)
		 ("C-c d r" . 'youdao-dictionary-search-and-replace)
		 ("C-c d i" . 'youdao-dictionary-search-from-input)
		 :map youdao-dictionary-mode-map
         ("h"       . my-youdao-dictionary-help)
         ("?"       . my-youdao-dictionary-help)
  :init
  (setq url-automatic-caching t)
  (setq youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词
  :config
  ;; Cache documents
  (setq url-automatic-caching t)
  (which-key-add-key-based-replacements "C-c d" "有道翻译")
  ;; Enable Chinese word segmentation support (支持中文分词)
  (with-no-warnings
    (with-eval-after-load 'hydra
      (defhydra youdao-dictionary-hydra (:color blue)
        ("p" youdao-dictionary-play-voice-of-current-word "play voice of current word")
        ("y" youdao-dictionary-play-voice-at-point "play voice at point")
        ("q" quit-window "quit")
        ("C-g" nil nil)
        ("h" nil nil)
        ("?" nil nil))
      (defun my-youdao-dictionary-help ()
        "Show help in `hydra'."
        (interactive)
        (let ((hydra-hint-display-type 'message))
          (youdao-dictionary-hydra/body))))

    (defun my-youdao-dictionary-search-at-point ()
      "Search word at point and display result with `posframe', `pos-tip' or buffer."
      (interactive)
      (if (posframe-workable-p)
          (youdao-dictionary-search-at-point-posframe)
        (youdao-dictionary-search-at-point)))

    (defun my-youdao-dictionary--posframe-tip (string)
      "Show STRING using `posframe-show'."
      (unless (posframe-workable-p)
        (error "Posframe not workable"))

      (if-let ((word (youdao-dictionary--region-or-word)))
          (progn
            (with-current-buffer (get-buffer-create youdao-dictionary-buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (youdao-dictionary-mode)
                (insert string)
                (set (make-local-variable 'youdao-dictionary-current-buffer-word) word)))
            (posframe-show
             youdao-dictionary-buffer-name
             :position (point)
             :left-fringe 8
             :right-fringe 8
             :max-width (/ (frame-width) 2)
             :max-height (/ (frame-height) 2)
             :background-color (face-background 'tooltip nil t)
             :internal-border-color (face-background 'posframe-border nil t)
             :internal-border-width 1)
            (unwind-protect
                (push (read-event) unread-command-events)
              (progn
                (posframe-hide youdao-dictionary-buffer-name)
                (other-frame 0)))
            (message "Nothing to look up"))))
    (advice-add #'youdao-dictionary--posframe-tip
                :override #'my-youdao-dictionary--posframe-tip)))
		
(provide 'init-translate)
