(provide 'init-org-export)

(defvar suk-emacs-share-dir
  (expand-file-name "share" user-emacs-directory)
  "Directory that stores shared data files when not launched from init.el.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXPORTER
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
;; Inline images in HTML instead of producting links to the image
(setq org-html-inline-images t)
;; Use (setq org-manual.css from the norang website for export document stylesheets)
;; (setq org-html-head-extra "<link rel=\"stylesheet\" href=\"org-manual.css\" type=\"text/css\" />")
(setq org-html-head-include-default-style nil)
;; Do not generate internal css formatting for HTML exports
(setq org-export-htmlize-output-type (quote css))
;; Increase default number of headings to export
(setq org-export-headline-levels 6)
(setq org-export-coding-system 'utf-8)
(setq org-table-export-default-format "orgtbl-to-csv")
;; Do not generate internal css formatting for HTML exports
(setq org-export-htmlize-output-type 'css)
(setq org-export-with-timestamps nil)
;; _ 不转义，相当于#+OPTIONS: ^:{} ~:{}
(setq org-export-with-sub-superscripts "{}") ; 处理
(setq org-use-sub-superscripts '{}) ; 显示

;; Embed inline CSS read from a file.
;;;###autoload
(defun null-or-unboundp (var)
  "Return t if VAR is either unbound or nil, otherwise return nil."
  (or (not (boundp var))
      (null (symbol-value var))))

;;;###autoload
(defun my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           ;;(path  org-css-file)
           (homestyle (and (or (null dir) (null (file-exists-p path)))
                           (not (null-or-unboundp 'my-org-inline-css-file))))
           (final (if homestyle my-org-inline-css-file path)))
      (if (file-exists-p final)
          (progn
            (setq-local org-html-head-include-default-style nil)
            (setq-local org-html-head
                        (concat
                         "<style type=\"text/css\">\n"
                         "<!--/*--><![CDATA[/*><!--*/\n"
                         (with-temp-buffer
                           (insert-file-contents final)
                           (buffer-string))
                         "/*]]>*/-->\n"
                         "</style>\n")))))))

(add-hook 'org-export-before-processing-hook #'my-org-inline-css-hook)

;; https://github.com/marsmining/ox-twbs
;; M-x package-install [RET] ox-twbs [RET]
;; If the installation doesn’t work try refreshing the package list:
;; M-x package-refresh-contents [RET]
;; usage: org-twbs-export-to-html

(when (locate-library "ox-twbs")
  (use-package ox-twbs))
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "s-\\") 'my-org-publish-buffer)))

(defun my-org-publish-buffer ()
  (interactive)
  (save-buffer)
  (org-twbs-export-to-html))
(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 70)
            (turn-on-font-lock)
            (when (and (display-graphic-p) (fboundp 'load-org-font))
              (load-org-font))
            ))
(defun my-execute-function-and-shell-command-async ()
  "执行自定义函数逻辑，然后异步调用外部 shell 命令，传递当前文件名的扩展名替换为 .html 的路径作为参数。"
  (interactive)
  ;; 检查当前缓冲区是否关联有文件
  (if (buffer-file-name)
      (let* ((current-file (buffer-file-name))
             ;; 使用 file-name-sans-extension 获取不带扩展名的文件名
             ;;(base-name (file-name-sans-extension current-file))
             ;; 构建新的文件名，替换扩展名为 .html
             ;;(html-file (my-cygpath-convert (concat base-name ".html")))
             ;; 定义要执行的外部命令，这里以 ls -l 为例
             ;; (command (if sys/win32p
             ;;              ;;"echo You may need to cleanup the html file."
             ;;              (format "c:/green/msys64/usr/bin/bash.exe --login /c/green/htm-cleanup.sh %s" html-file)
             ;;            (format "htm-cleanup.sh %s" (shell-quote-argument html-file))))
             ;; 定义输出缓冲区名称
             ;;(output-buffer "*Shell Command Output*")
             )
        (my-org-publish-buffer)
        ;; 执行自定义函数逻辑
        (message "正在处理文件: %s" current-file)

        ;; 异步调用外部 shell 命令
        ;;(async-shell-command command output-buffer)

        ;; 显示提示信息
        ;;(message "已启动异步命令: %s" command)
        )
    (message "当前缓冲区没有关联的文件。")))

;; 转换msys路径（为了在Windows下使用Bash）
(defun my-cygpath-convert (win-path)
  "Use cygpath to convert WIN-PATH to /cygdrive style."
  (let ((output (shell-command-to-string
                 (format "D:/green/msys64/usr/bin/cygpath -u \"%s\"" win-path))))
    (string-trim output)))

;; 绑定快捷键 C-c e 到上述函数
(global-set-key (kbd "C-S-e") #'my-execute-function-and-shell-command-async)

;;(global-set-key (kbd "C-S-e") #'my-org-publish-buffer)
(defun org-export-docx ()
  (interactive)
  (let ((docx-file (concat (file-name-sans-extension (buffer-file-name)) ".docx"))
        (template-file (concat  suk-emacs-share-dir "/template/template.docx"))
        (shell-command (format "pandoc %s -o %s --reference-doc=%s" (buffer-file-name) docx-file template-file))
        (message "Convert finish: %s" docx-file))))

;; covert to html
(when (locate-library "htmlize")
  (use-package htmlize :defer 2))
;;(use-package ob-sagemath)

(require 'ox-html)
;; 导出：
;; C-c C-e h h        ;; 整文件
;; C-c C-e C-s h h    ;; 子树（推荐） 光标放在标题上时，执行
