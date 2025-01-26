;;; early-init.el --- Early Init File -*- lexical-binding: t; no-byte-compile: t -*-
;; NOTE: early-init.el is now generated from readme.org.  Please edit that file instead

;; 将垃圾收集推迟到启动过程中的更后面。
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.8)
(setq inhibit-startup-message t)

;; 喺 Emacs 27 及之后版本，软件包初始化会喺 `user-init-file` 加载之
;; 前进行，但系会喺 `early-init-file` 之后进行。我哋必须阻止 Emacs
;; 提前做呢个操作！
(setq package-enable-at-startup nil)
;;唔好允许从软件包缓存加载（原因一样）。
(setq package-quickstart nil)
;; `use-package` 喺 29 版本开始内置。它必须喺加载 `use-package` 之
;; 前设置。
(setq use-package-enable-imenu-support t)
;; 系统默认编码
(set-language-environment 'utf-8)
;; 更干净嘅 GUI
;; 通过尽早禁用呢啲 UI 元素，避免出现未加样式嘅 Emacs 瞬间显示。
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; 调整 Emacs 窗口大小可能系更改字体时非常昂贵嘅操作。
;; 通过禁用呢个操作，当字体大过系统默认字体时，我哋可以轻松将启动时间减少一半。
(setq frame-inhibit-implied-resize t)
(setq default-frame-alist
      (append '((width                . 140)  ; Frame幅
                (height               . 40 )  ; Frame高
                (left                 . 170 ) ; 配置左位置
                (top                  . 30 )  ; 配置上位置
                (line-spacing         . 0  )  ; 文字間隔
                (left-fringe          . 12 )  ; 左fringe幅
                (right-fringe         . 12 )  ; 右fringe幅
                (menu-bar-lines       . 1  )  ; 菜单行
                (cursor-type          . bar)  ; 光标種別
                (alpha                . 100)  ; 透明度
                )
              default-frame-alist))
(setq initial-frame-alist default-frame-alist)

;; 元素一部分禁用
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1)) ; keep the menu is better.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(toggle-frame-maximized)
;; 緩衝區，无令立殺之。
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
(setq find-file-visit-truename t)
(provide 'early-init)
;;(global-unset-key (kbd "C-SPC"))

;;; early-init.el ends here
