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
		;; (global-set-key "\C-ct" 'google-translate-at-point)
		;; (global-set-key "\C-cT" 'google-translate-query-translate)
		;; (global-set-key (kbd "\C-cq") 'google-translate-smooth-translate)
		("C-x t s" . google-translate-at-point)
		("C-x t q" . google-translate-query-translate)
		("C-x t S" . google-translate-smooth-translate)
  ))

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
