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


;; 谷歌翻译，
(use-package google-translate
  :disabled
  ;;:ensure t
  :defer 2
  :config (setq google-translate--tkk-url "http://translate.google.cn/"
				google-translate-base-url "http://translate.google.cn/translate\_a/single"
				google-translate-listen-url "https://translate.google.cn/translate\_tts"
				google-translate-default-target-language "zh-CN"
				google-translate-default-source-language "en"))

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

(provide 'init-translate)
