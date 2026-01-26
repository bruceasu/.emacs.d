;; ~/.emacs-minimal.el

;; 将垃圾收集推迟到启动过程中的更后面。
(setq gc-cons-threshold most-positive-fixnum gc-cons-percentage 0.8)
(setq inhibit-startup-message t)
(setq idle-update-delay 1.0)

;; 系统默认编码
(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)            ;缓存文件编码
(setq default-file-name-coding-system 'utf-8)              ;文件名编码
(setq default-keyboard-coding-system 'utf-8)               ;键盘输入编码
(setq default-process-coding-system '(utf-8 . utf-8))      ;进程输出输入编码
(setq default-sendmail-coding-system 'utf-8)               ;发送邮件编码
(setq default-terminal-coding-system 'utf-8)               ;终端编码


(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq buffer-file-coding-system 'utf-8)
(setq session-save-file-coding-system 'utf-8)


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
(setq initial-major-mode 'text-mode) ; org-mode
(setq default-major-mode 'text-mode)
(toggle-frame-maximized)
;; 緩衝區，无令立殺之。
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
(setq find-file-visit-truename t)
(defvar suk-emacs-root-dir      (file-truename "D:/green/emacs-29.2/.emacs.d"))
(defvar suk-emacs-config-dir    (expand-file-name "etc" suk-emacs-root-dir))
(defvar suk-emacs-extension-dir (expand-file-name "extensions" suk-emacs-root-dir))
(defvar suk-emacs-share-dir     (expand-file-name "share" suk-emacs-root-dir))
(defvar suk-emacs-themes-dir    (expand-file-name "themes" suk-emacs-share-dir))
(defvar suk-emacs-var-dir       (expand-file-name "var" suk-emacs-root-dir))
(defvar suk-emacs-tmp-dir       (expand-file-name "tmp" suk-emacs-var-dir))
(defvar suk-emacs-backup-dir    (expand-file-name "backup" suk-emacs-tmp-dir))

;; OS 嘅 HOME 目录。
(defvar user-home-dir (getenv "HOME"))
(if (eq system-type 'windows-nt) (defvar user-home-dir (getenv "USERPROFILE")))
;; blink search
(setq blink-search-db-path (expand-file-name "blink-search.db" suk-emacs-tmp-dir))
;; 历史
(setq savehist-file (concat suk-emacs-var-dir "/history"))
;; Amx
(setq amx-save-file (concat suk-emacs-var-dir "/amx-items"))
;; 自动保存
(setq auto-save-list-file-prefix (concat suk-emacs-var-dir "/auto-save-list/.saves-"))
;; Eshell
(setq eshell-directory-name (concat suk-emacs-var-dir "/eschell"))
(setq eshell-history-file-name (concat eshell-directory-name "/history"))
;; projectitle-bookmarks
(setq projectile-known-projects-file (concat suk-emacs-var-dir "/projectile-bookmarks.eld"))
(setq backup-directory-alist `(("" . ,suk-emacs-tmp-dir)))
;; Bookmark
(setq bookmark-default-file (concat suk-emacs-var-dir "/emacs.bmk"))
;; Diary
(setq diary-file (concat user-home-dir "/diary"))

;;(setq server-socket-dir suk-emacs-var-dir)

;; Clear to avoid analyzing files when loading remote files.
(setq file-name-handler-alist nil)
;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)


 (unless (file-exists-p suk-emacs-var-dir)
  (make-directory suk-emacs-var-dir t))
 (setq projectile-cache-file (expand-file-name "projectile.cache" suk-emacs-var-dir))

;; Ignore `cl` expiration warnings
(setq byte-compile-warnings '(cl-function))

;; 王勇的版本 https://manateelazycat.github.io/emacs/2022/03/02/emacs-load-directory-recursively.html
(require 'cl-lib)
(defun add-subdirs-to-load-path (search-dir isFirst)
  (interactive)
  (when isFirst
    ;; The original version did not add the first search-dir itself to
    ;; the `load path`. The recursive search-dir was added before the
    ;; recursion.
    (add-to-list 'load-path search-dir))
  (let* ((dir (file-name-as-directory search-dir)))
    (dolist (subdir
             ;; goleui bat bityiu ge mukluk, taising Emacs kaidung cudou.
             (cl-remove-if
              #'(lambda (subdir)
                  (or
                   ;; m hai mangin
                   (not (file-directory-p (concat dir subdir)))
                   ;; yiceui haamin ge mukluk
                   (member subdir '("." ".." ; Linux/Uniux haitung ge  dongcin mukluk tungmaai fu mukluk
                                    "dist" "node_modules" "__pycache__" ; takding ge yüyin seunggwaan ge mukluk
                                    "RCS" "CVS" "rcs" "cvs" ".git" ".github")))) ; baanbun hungjai mukluk
              (directory-files dir)))
      (let ((subdir-path (concat dir (file-name-as-directory subdir))))
        ;; mukluk bauhaam  .el .so .dll ge mangin di louging sinji gaa dou `load-path` binleung
        (when (cl-some #'(lambda (subdir-file)
                           (and (file-regular-p (concat subdir-path subdir-file))
                                ;; .so .dll 文件指非Elisp语言编写的Emacs动态库
                                (member (file-name-extension subdir-file) '("el" "so" "dll"))))
                       (directory-files subdir-path))

          ;; jüyi: add-to-list ge daisaam go caamsou bitseuiwai t, timgaa dou meibou,
          ;; kokbou gwongdou yausin
          (add-to-list 'load-path subdir-path t))

        ;; geieuuk daigwai sausok ji mukluk.
        (add-subdirs-to-load-path subdir-path nil)))))

;; 加载定义的目录
(add-subdirs-to-load-path suk-emacs-config-dir t)
(add-subdirs-to-load-path suk-emacs-extension-dir t)
(add-subdirs-to-load-path suk-emacs-themes-dir t)

(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=28p
  (>= emacs-major-version 28)
  "Emacs is 28 or above.")

(defconst emacs/>=29p
  (>= emacs-major-version 29)
  "Emacs is 29 or above.")

(defconst emacs/>=30p
  (>= emacs-major-version 30)
  "Emacs is 30 or above.")

;; (load-if-exists (expand-file-name "suk.el" suk-emacs-root-dir))  
;;(load-theme 'wombat t)   ;; 只加载 wombat 主题，或你喜欢的主题

(require 'lazycat-theme)
(lazycat-theme-load-dark)
;; Fonts

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
(setq emacs-font-size-pair '(14 . 14))

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
    (when (fboundp 'mixed-pitch-mode) (mixed-pitch-mode nil))
    (when (fboundp 'variable-pitch-mode) (variable-pitch-mode nil))



(global-set-key (kbd "C-<f11>") 'toggle-frame-fullscreen)      ;; 快捷键 C-<f11> 用于切换全屏模式
(global-set-key (kbd "M-s-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "RET") #'newline-and-indent)              ;; 回车键 RET 用于创建新行并对齐
(global-set-key (kbd "S-<return>") #'comment-indent-new-line)  ;; Shift + 回车键用于取消对齐创建的新行
  