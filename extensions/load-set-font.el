;;; load-set-font.el --- load my fonts.
;;
;; Copyright (C) 2018 Suk
;;
;; Author: Suk
;;
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
;; Fonts Setting
;;
;;; Code:


;;===================================================
;; Fonts
;;===================================================
;;;###autoload
(defun suk-install-fonts ()
  "Install necessary fonts."
  (interactive)
  (all-the-icons-install-fonts)
  (nerd-icons-install-fonts))

(defun config-font-size (en-size cn-size)
  (when (display-graphic-p)
    (set-face-attribute 'default nil :family cn-size)
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font) charset
                        (font-spec :family cn-font :size cn-size)))))

(defun font-exist-p (fontname)
  "Test if this FONTNAME is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname))
        nil t)))

;; adjust the fonts
(defun available-font (font-list)
  "Get the first available font from FONT-LIST."
  (catch 'font
    (dolist (font font-list)
      (if (member font (font-family-list))
	  (throw 'font font)))))

(setq efl '(
        "asu-cjk-code"
	    "Cascadia Code"
	    "Source Code Pro"
	    "LXGW WenKai Mono"
	    "霞鹜文楷等宽"
	    "JetBrains Mono"
	    "Victor Mono"
	    "Consolas"
	    "Courier New"
	    "Monaco"
	    "Ubuntu Mono"
	    "lucida grande"
	    "lucida sans unicode"
	    "lucida"
	    "helvetica"
	    "Tahoma"
	    "Times New Roman"
	    "Migu 1M Less"
	    "DejaVu Sans Mono"
	    ))

(setq cfl '(
        "asu-cjk-code"
	    "asu-cjk-sans"
	    "asu-cjk-serif"
	    "KleeOne+CJK"
	    "LXGW WenKai"
	    "霞鹜文楷"
	    "LXGW WenKai Mono"
	    "霞鹜文楷等宽"
	    "仓耳今楷01-27533 W04"
	    "Hiragino Sans GB"
	    "Microsoft YaHei"
	    "微软雅黑"
	    "WenQuanYi Micro Hei"
	    "Sarasa Mono SC"
	    "Sarasa Mono HK"
	    "Sarasa Mono TC"
	    "Sarasa Mono JP"
	    "Simsun"
	    "宋体"
	    ))
(setq en-font "Time New Roman"
      cn-font "LXGW WenKai")

(setq cn-font (available-font cfl))
(setq en-font (available-font efl))

;; Fonts
(setq emacs-font-size-pair '(24 . 24))

(setq en-font-size (car emacs-font-size-pair)
      cn-font-size (cdr emacs-font-size-pair))

(defvar emacs-english-font en-font
  "The font name of English.")

(defvar emacs-cjk-font cn-font
  "The font name for CJK.")

(defvar emacs-font-size-pair (emacs-english-font . emacs-cjk-font)
  "Default font size pair for (latin . cjk).")


;; (config-font-size 17 18)
(config-font-size (default-value 'en-font-size) (default-value 'cn-font-size))


;; 解决卡顿
(setq inhibit-compacting-font-caches t)
;;只渲染当前屏幕语法高亮，加快显示速度
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(setq font-lock-maximum-size 5000000); set 5mb file size limit for fontification

(defun set-font (latin cjk size-pair)
  "Setup Emacs LATIN and CJK fonts with SIZE-PAIR on x window-system."
  (when (font-exist-p latin)
    (set-frame-font (format "%s:pixelsize=%d" latin (car size-pair)) t)
    ;; (dolist (face '(default fixed-pitch fixed-pitch-serif variable-pitch))
    ;;   (set-face-attribute face nil :family latin))
    )

  (when (font-exist-p cjk)
    ;;(set-face-attribute 'default nil :family "JetBrainsMono NFM")
    (dolist (charset
	         '(kana
               han
               symbol
               cjk-misc
               bopomofo
               japanese-jisx0208
               japanese-jisx0212
               katakana-jisx0201
               ;;latin
               symbol
               ))
      (set-fontset-font
       (frame-parameter nil 'font) charset
       (font-spec :family cjk :size (cdr size-pair))))
    (setq face-font-rescale-alist
 	      (mapcar (lambda (item) (cons item 1.0)) cfl))
    ))

(defun emacs-step-font-size (step)
  "Increase/Decrease Emacs's font by STEP size."
  (let ((scale-steps emacs-font-size-pair-list))
    (if (< step 0) (setq scale-steps (reverse scale-steps)))
    (setq emacs-font-size-pair
          (or (cadr (member emacs-font-size-pair scale-steps))
              emacs-font-size-pair))
    (when emacs-font-size-pair
      (message "emacs font size set to %.1f" (car emacs-font-size-pair))
      (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))

(defun increase-emacs-font-size ()
  "Decrease Emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size 1))

(defun decrease-emacs-font-size ()
  "Increase Emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size -1))

(when (fboundp 'variable-pitch-mmode)
  (set-face-attribute 'variable-pitch nil :family "asu-cjk-serif" :height 120))
(when (fboundp 'fixed-pitch-mode)
  (set-face-attribute 'fixed-pitch nil :family "asu-cjk-mono" :height 120))


(defvar loaded-font-type nil
  "Current font type.")
;;----------------------------------------------------------
(defun load-default-font ()
  "Load default font setting."
  (interactive)
  (unless (eq loaded-font-type 1)
    (setq emacs-english-font en-font)
    (setq emacs-cjk-font cn-font)
    ;; Setup font size based on emacs-font-size-pair
    (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)
    ;;(when (fboundp 'variable-pitch-mode) (variable-pitch-mode 1))
    ;; 还是等宽字体比较顺眼
    (when (fboundp 'fixed-pitch-mode) (fixed-pitch-mode 1))
    (setq loaded-font-type 1)
    (message "Set default font")
  ))

;;----------------------------------------------------------
(defun load-program-font ()
  "Load Program font."
  (interactive)
  (unless (eq loaded-font-type 2)
    (setq emacs-english-font "asu-cjk-code")
    ;;(setq emacs-english-font "M+CodeLat50 Nerd Font Mono")
    (setq emacs-cjk-font "asu-cjk-code")
    (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)
    (when (member "Symbols Nerd Font Mono" (font-family-list))
      (set-fontset-font t 'symbol "Symbols Nerd Font Mono")
      ;; FontAwesome 范围
      (set-fontset-font t '(#xf000 . #xf2e0) "Symbols Nerd Font Mono")
      ;; 扩展至可能包含 Material Design Icons 的范围
      (set-fontset-font t '(#xe000 . #xf8ff) "Symbols Nerd Font Mono"))
    (when (fboundp 'fixed-pitch-mode) (fixed-pitch-mode 1))
    (setq loaded-font-type 2)
    (message "Set program font")))

;;(add-hook 'prog-mode-hook 'load-program-font)
(add-hook 'prog-mode-hook  (lambda () (variable-pitch-mode -1)))

;;----------------------------------------------------------
(defun load-org-font ()
  "Load org mode font."
  (interactive)
  (unless (eq loaded-font-type 3)
    ;;(load-default-font)
    ;; (set-face-attribute FACE FRAME &rest ARGS)
    ;; or (set-face-attribute FACE FRAME &rest ARGS)
    ;; 字体和字号
    ;; :family 指定字体家族，如 "Monaco"、"DejaVu Sans Mono"。
    ;; :height 指定字体大小，以 1/10 点为单位，如 120 表示 12 点大小。
    ;; :weight 指定字重，如 'normal、'bold。
    ;; :width 指定字体宽度，如 'normal、'condensed。
    ;; 颜色
    ;; :foreground 指定前景色，即文字颜色。
    ;; :background 指定背景色。
    ;; 下划线、删除线等
    ;; :underline 指定是否添加下划线，可以是 t、nil 或颜色名称/值。
    ;; :overline 指定是否添加上划线。
    ;; :strike-through 或 :strike 指定是否添加删除线。
    ;; 其他
    ;; :slant 指定字体倾斜样式，如 'italic、'oblique、'normal。
    ;; :inherit 指定从其他 face 继承属性。
    ;; 关于 :font 参数
    ;; :font 参数可以用于直接指定整个字体描述字符串，
    ;; 这是一种快捷方式，它允许你同时指定字体家族、大小等信息，格式通常是 "家族-大小"，
    ;; 如 "Monaco-12"。使用 :font 时，可能无法单独指定字重、宽度等属性。
    ;; (set-face-attribute 'default nil
    ;;                     :family "asu-cjk-sans" ; 字体家族，如 "Sarasa Mono SC"
    ;;                     :height 120 ; 字号，120 表示 12pt
    ;;                     :weight 'normal ; 字体粗细
    ;;                     :width 'normal) ; 字体宽度
    ;;(variable-pitch-mode -1)
    ;; 设置英文字体并指定字号。
    (setq emacs-english-font "asu-cjk-code")
    ;;(setq emacs-english-font "M+CodeLat50 Nerd Font Mono")
    ;; 给相应的字符集设置中文字体。
    ;; abcdefg
    ;; hij汉字
    (setq emacs-cjk-font "asu-cjk-code")
    (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair)
    ;; (face-remap-add-relative 'default nil :font "asu-cjk-code")

    ;;(set-face-attribute 'org-block nil :family "M+CodeLat50 Nerd Font Mono")
;;    (set-face-attribute 'org-code nil :family "M+CodeLat50 Nerd Font Mono")
    (set-face-attribute 'org-table nil :family "M+CodeLat50 Nerd Font Mono")
    ;;(set-face-attribute 'org-table nil :family "asu-cjk-mono")
    ;; (set-face-attribute 'org-table nil :family "asu-cjk-code")
    ;; (set-face-attribute 'org-level-1 nil :family "asu-cjk-sans")
    ;; (set-face-attribute 'org-level-2 nil :family "asu-cjk-sans")
    ;; (set-face-attribute 'org-level-3 nil :family "asu-cjk-sans")
    ;; (set-face-attribute 'org-level-4 nil :family "asu-cjk-sans")
    ;; (set-face-attribute 'org-level-5 nil :family "asu-cjk-sans")
    ;; (set-face-attribute 'org-level-6 nil :family "asu-cjk-sans")
    ;; (set-face-attribute 'org-level-7 nil :family "asu-cjk-sans")
    ;; (set-face-attribute 'org-level-8 nil :family "asu-cjk-sans")
    ;; (when (member "Symbols Nerd Font Mono" (font-family-list))
    ;; (set-fontset-font t 'symbol "Symbols Nerd Font Mono")
    ;; ;; FontAwesome 范围
    ;; (set-fontset-font t '(#xf000 . #xf2e0) "Symbols Nerd Font Mono")
    ;; ;; 扩展至可能包含 Material Design Icons 的范围
    ;; (set-fontset-font t '(#xe000 . #xf8ff) "Symbols Nerd Font Mono"))
    (when (fboundp 'mixed-pitch-mode) (mixed-pitch-mode 1))

    (setq loaded-font-type 3)
    (message "Set org-mode font")))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook 'load-org-font)
)


;;; ----------------------------------------------------------

;; 解决 daemon 时， 字体无效。
(when (and (fboundp 'daemonp) (daemonp))
  (add-hook 'after-make-frame-functions
            (lambda (frame)
              (with-selected-frame frame
                (load-default-font)))))


(when sys/win32p
  ;; (set-frame-font "Simsun 12")
  ;; setup change size font, base on emacs-font-size-pair-list
  (global-set-key (kbd "C-M-=") 'increase-emacs-font-size)
  (global-set-key (kbd "C-M--") 'decrease-emacs-font-size)
  )

(add-hook 'after-init-hook #'load-default-font)

(provide 'load-set-font)
;;; load-set-font.el ends here.
