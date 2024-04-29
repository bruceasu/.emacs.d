;; -*- coding: utf-8; lexical-binding: t; -*-
;;; init-dired.el --- Dired configuration

;;; Require
(eval-when-compile
  (require '+const)
  (require '+custom)
  (require 'dired)
  (require 'dired-x)
  (require 'dired-details)                ;Dired详细信息
  (require 'dired-details+)
  )

;; Quick sort dired buffers via hydra
(use-package dired-quick-sort
  :bind (:map dired-mode-map
         ("S" . hydra-dired-quick-sort/body)))

;; Show git info in dired
(use-package dired-git-info
  :bind (:map dired-mode-map
         (")" . dired-git-info-mode)))

;; Allow rsync from dired buffers
(use-package dired-rsync
  :bind (:map dired-mode-map
         ("C-c C-r" . dired-rsync)))

;; Colorful dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Shows icons
(use-package nerd-icons-dired
  :diminish
  :when (icons-displayable-p)
  :custom-face
  (nerd-icons-dired-dir-face ((t (:inherit nerd-icons-dsilver :foreground unspecified))))
  :hook (dired-mode . nerd-icons-dired-mode))

;; Extra Dired functionality
(use-package dired-aux :ensure nil)

;;; Code:
;; Guess a default target directory
(setq dired-dwim-target t)

;; Always delete and copy recursively
(setq dired-recursive-deletes 'always
      dired-recursive-copies 'always)

;; Show directory first
(setq dired-listing-switches "-alh --group-directories-first")
(let ((cmd (cond (sys/mac-x-p "open")
                 (sys/linux-x-p "xdg-open")
                 (sys/win32p "start")
                 (t ""))))
  (setq dired-guess-shell-alist-user
        `(("\\.pdf\\'" ,cmd)
          ("\\.docx\\'" ,cmd)
          ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
          ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
          ("\\.\\(?:xcf\\)\\'" ,cmd)
          ("\\.csv\\'" ,cmd)
          ("\\.tex\\'" ,cmd)
          ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
          ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
          ("\\.html?\\'" ,cmd)
          ("\\.md\\'" ,cmd)))
  (when sys/macp
    (if (executable-find "gls")
        (progn
          ;; Use GNU ls as `gls' from `coreutils' if available.
          (setq insert-directory-program "gls")
          ;; Using `insert-directory-program'
          (setq ls-lisp-use-insert-directory-program t))
      (progn
        ;; Suppress the warning: `ls does not support --dired'.
        (setq dired-use-ls-dired nil)
        (setq dired-listing-switches "-alh"))))

  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

(setq dired-recursive-copies t)         ;可以递归的进行拷贝
(setq dired-recursive-deletes t)        ;可以递归的删除目录
(setq dired-recursive-deletes 'always)  ;删除东西时不提示
(setq dired-recursive-copies 'always)   ;拷贝东西时不提示
(setq dired-listing-switches "-aluh")   ;传给 ls 的参数
(setq dired-details-hidden-string "") ;设置隐藏dired里面详细信息的字符串
(setq directory-free-space-args "-Pkh") ;目录空间选项
(setq dired-omit-size-limit nil)        ;dired忽略的上限
(setq dired-dwim-target t)              ;Dired试着猜处默认的目标目录
(setq my-dired-omit-status t)           ;设置默认忽略文件
(setq my-dired-omit-regexp "^\\.?#\\|^\\..*") ;设置忽略文件的匹配正则表达式
(setq my-dired-omit-extensions '(".cache")) ;设置忽略文件的扩展名列表
(add-hook 'dired-after-readin-hook #'(lambda ()
                                       (progn
                                         (require 'dired-extension)
                                         (dired-sort-method)))) ;先显示目录, 然后显示文件
(add-hook
 'dired-mode-hook
 #'(lambda ()
     (require 'dired-extension)
     (dired-omit-method)                ;隐藏文件的方法
     ))
(setq dired-guess-shell-alist-user      ;设置文件默认打开的模式
      '(
        ;; 压缩包
        (list "\\.rar$" "unrar e -ad")
        (list "\\.tar.bz2$" "tar jxvf")
        (list "\\.gz$" "gzip -d")
        ;; 其他
        (list "\\.exe$" "wine")))

;;; ### Dired ###
;;; --- 文件浏览器
(lazy-load-set-keys
 '(
   ("h" . dired-next-subdir)            ;下一个子目录
   ("l" . dired-prev-subdir)            ;上一个子目录
   ("n" . dired-next-dirline)           ;下一个目录
   ("p" . dired-prev-dirline)           ;上一个目录
   ("P" . dired-do-kill-lines)          ;删除标记的行
   ("5" . dired-translate-to-html)      ;转换到HTML格式
   ("9" . auto-install-from-dired)      ;自动从EmacsWiki安装标记的文件
   ("I" . image-dired)                  ;打开浏览模式
   ("W" . dired-x-find-file)            ;查找文件
   ("J" . awesome-tab-backward-tab)
   ("K" . awesome-tab-forward-tab)
   ("X" . traverse-cp-or-mv-extfiles-in-dir) ;拷贝或移动目录下指定扩展名的文件
   ("V" . traverse-dired-browse-archive)     ;浏览压缩文件
   ("," . dired-diff)                        ;比较文件
   ("SPC" . scroll-up)                       ;向下翻页
   ("e" . scroll-down)                       ;向上翻页
   ("c" . kill-this-buffer)                  ;关闭当前标签
   ("/" . copy-buffer-file-name-as-kill)     ;显示路径或名称
   ;;("s" . one-key-menu-dired-sort)           ;排序
   ;;("F" . one-key-menu-dired-filter)         ;过滤
   ("w" . wdired-change-to-wdired-mode)      ;切换到dired编辑模式
   )
 dired-mode-map
 )

(lazy-load-local-keys
 '(
   ("/" . copy-buffer-file-name-as-kill))
 dired-mode-map
 "buffer-extension")


(lazy-load-local-keys
 '(
   ("M-o" . dired-toggle-omit)          ;切换忽略状态
   ("?" . dired-get-size)               ;得到文件的大小
   ("[" . dired-rename-with-copy)       ;重命名函数
   ("'" . dired-up-directory-single)    ;返回上一级目录
   ("4" . dired-serial-rename)          ;批量重命名
   ("7" . dired-move-to-last-file)      ;移动到最后一个文件
   ("8" . dired-move-to-first-file)     ;移动到第一个文件
   ("k" . dired-previous-file-line)     ;上一行
   ("j" . dired-next-file-line)         ;下一行
   ;;("{" . dired-gnome-open-file)        ;用GNOME方式打开文件
   ("E" . dired-touch-now)              ;Touch命令
   ("f" . dired-find-file+)             ;打开当前文件或目录
   ("C-m" . dired-find-file+)           ;打开当前文件或目录
   )
 dired-mode-map
 "dired-extension")


;;;###autoload
(defun my-file-usage ()
  (interactive)
  (message (shell-command-to-string "du -kh")))

;;;###autoload
(defun my-computer-sleep-now ()
  "Make my computer sleep now."
  (interactive)
  (let* ((cmd (cond
               (sys/win32p
                "rundll32.exe PowrProf.dll,SetSuspendState")
               (sys/cygwinp
                "rundll32.exe PowrProf.dll,SetSuspendState")
               (sys/macp
                "pmset sleepnow")
               (t
                "sudo pm-suspend"))))
    (shell-command cmd)))


;; {{ dired
(with-eval-after-load 'dired
  (defun my-replace-dired-base (base)
    "Change file name in `wdired-mode'"
    (let* ((fp (dired-file-name-at-point))
           (fb (file-name-nondirectory fp))
           (ext (file-name-extension fp))
           (dir (file-name-directory fp))
           (nf (concat base "." ext)))
      (when (yes-or-no-p (format "%s => %s at %s?"
                                 fb nf dir))
        (rename-file fp (concat dir nf)))))
  (defun my-extract-mp3-from-video ()
    "Extract mp3 from current video file using ffmpeg."
    (interactive)
    (let* ((video-file (file-name-nondirectory (dired-file-name-at-point)))
           (params (split-string (string-trim (read-string "Please input start-second [total seconds] (e.g, \"6 10\" or \"05:30 5\") or just press enter: "))
                                 " +"))
           (start (car params))
           (total (if (eq (length params) 1) "5" (nth 1 params)))
           cmd)
      (cond
       ((string= start "")
        ;; extract audio to MP3 with sample rate 44.1Khz (CD quality), stereo, and 2 channels
        (setq cmd (format "ffmpeg -i \"%s\" -vn -ar 44100 -ac 2 -ab 192 -f mp3 \"%s\""
                          video-file
                          (concat (file-name-base video-file) ".mp3"))))
       (t
        (setq cmd (format "ffmpeg -i \"%s\" -vn -ss %s -t %s -acodec copy \"%s\""
                          video-file
                          start
                          total
                          (format "%s-%s-%s.mp3" (file-name-base video-file) start total)))))
      (shell-command (concat cmd " &"))))

  (defun my-extract-mkv-subtitle ()
    "Use mkvtoolnix to extract mkv subtitle."
    (interactive)
    (let* ((file (file-name-nondirectory (dired-file-name-at-point)))
           (ext (file-name-extension file))
           (default-directory (file-name-directory (dired-file-name-at-point)))
           trunks
           track-number)
      (cond
       ((not (string= "mkv" ext))
        (message "Only mkv files can be processed."))
       ((not (executable-find "mkvextract"))
        (message "Please install mkvtoolnix."))
       (t
        ;; split output into trunks
        (setq trunks (split-string (shell-command-to-string (format "mkvinfo \"%s\"" file))
                                   "| ?\\+ [A-Z][^\n]+[\n]*"))
        ;; only interested english subtitle trunk
        (setq trunks (cl-remove-if-not
                      (lambda (trunk)
                        (string-match "Track type: subtitles" trunk))
                      trunks))
        ;; If there is more than one subtitle, process English track only
        (when (> (length trunks) 1)
          (setq trunks (cl-remove-if-not
                        (lambda (trunk)
                          (string-match "Language: eng" trunk))
                        trunks)))
        (when (and (> (length trunks) 0)
                   (string-match "Track number: \\([0-9]+\\)" (car trunks)))

          ;; only extract the track number from the first truck
          (setq track-number (1- (string-to-number (match-string 1 (car trunks)))))
          (shell-command (format "mkvextract tracks \"%s\" %s:\"%s.srt\" > /dev/null 2>&1"
                                 file
                                 track-number
                                 (file-name-base file))))))))

  (defun my-record-wav-by-mp3 ()
    "Record a wav using meta data from current mp3 file."
    (interactive)
    (let* ((mp3-file (file-name-nondirectory (dired-file-name-at-point)))
           (base (file-name-base mp3-file))
           (params (split-string base  "-"))
           (output-file (concat base ".wav"))
           (total (string-to-number (nth (1- (length params)) params)))
           cmd)
      (if (= total 0) (setq total 4))
      (setq cmd (format "arecord -fdat -d %s \"%s\""
                        total
                        output-file))
      (message "Start recording %s seconds wav ..." total)
      (my-async-shell-command cmd)))
  (defun my-play-both-mp3-and-wav ()
    "Play wav and mp3."
    (interactive)
    (let* ((audio-file (file-name-nondirectory (dired-file-name-at-point)))
           (base (file-name-base audio-file))
           (ext (file-name-extension audio-file) )
           (cmd (format "mplayer -quiet \"%s\" \"%s\""
                        audio-file
                        (concat base "." (if (string= ext "mp3") "wav" "mp3")))))
      (my-async-shell-command cmd)))
  (defun my-copy-file-info (fn)
    "Copy file or directory info."
    (let* ((file-name (dired-file-name-at-point)))
      (when (file-directory-p file-name)
        (setq file-name (directory-file-name file-name)))
      (message "%s => clipboard & yank ring"
               (copy-yank-str (funcall fn file-name)))))

  (with-eval-after-load 'hydra
    (defhydra my-hydra-dired (:color blue)
      "
^Misc^                      ^File^              ^Copy Info^
-----------------------------------------------------------------
[_vv_] Video => Mp3        [_R_] Move           [_pp_] Path
[_aa_] Record by mp3       [_cf_] New           [_nn_] Name
[_zz_] Play wav&mp3        [_rr_] Rename        [_bb_] Base name
[_sa_] Fetch subtitle(s)   [_C_]  Copy          [_dd_] directory
[_se_] Extract subtitle    [_rb_] Change base
[_aa_] Recording Wav       [_df_] Diff 2 files
[_ee_] Mkv => Srt          [_ff_] Find
[_+_] Create directory     [_du_] File usage
[_mp_] Mplayer extra opts
"
      ("mp" my-mplayer-setup-extra-opts)
      ("sa" shenshou-download-subtitle)
      ("se" shenshou-extract-subtitle-from-zip)
      ("pp" (my-copy-file-info 'file-truename))
      ("nn" (my-copy-file-info 'file-name-nondirectory))
      ("bb" (my-copy-file-info 'file-name-base))
      ("dd" (my-copy-file-info 'file-name-directory))
      ("rb" (my-replace-dired-base (car kill-ring)))
      ("vv" my-extract-mp3-from-video)
      ("ee" my-extract-mkv-subtitle)
      ("aa" my-record-wav-by-mp3)
      ("zz" my-play-both-mp3-and-wav)
      ("C" dired-do-copy)
      ("R" dired-do-rename)
      ("cf" find-file)
      ("du" my-file-usage)
      ("df" my-ediff-files)
      ("rr" dired-toggle-read-only)
      ("ff" (lambda (regexp)
              (interactive "sMatching regexp: ")
              (find-lisp-find-dired default-directory regexp)))
      ("+" dired-create-directory)
      ("q" nil)))

  (defun dired-mode-hook-hydra-setup ()
    (local-set-key (kbd "y") 'my-hydra-dired/body))
  (add-hook 'dired-mode-hook 'dired-mode-hook-hydra-setup))

;; }}

(provide 'init-dired)

;;; init-dired.el ends here
