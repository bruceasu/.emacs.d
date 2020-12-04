;;; suk-org --- My org-mode settings.
;;; Commentary:
;;;
;;; My own org-mode config

;;; Code:
;;; The following lines are always needed. Choose your own keys.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Publishing and Exporting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'org-static-blog)

;; (setq org-static-blog-publish-title "Free World")
;; (setq org-static-blog-publish-url "https://bruceasu.github.io/")
;; (setq org-static-blog-publish-directory "d:/03_projects/suk/bruceasu.github.io/")
;; (setq org-static-blog-posts-directory "d:/03_projects/suk/bruceasu.github.io/_src/")
;; (setq org-static-blog-drafts-directory "d:/03_projects/suk/bruceasu.github.io/_drafts/")
;; (setq org-static-blog-enable-tags t)
;; (setq org-static-blog-langcode "zh")
;; (setq  org-static-blog-index-length 99)
;; ;;(setq org-export-with-toc nil)
;; (setq org-export-with-section-numbers nil)

;; ;; This header is inserted into the <head> section of every page:
;; ;;   (you will need to create the style sheet at
;; ;;    ~/projects/blog/static/style.css
;; ;;    and the favicon at
;; ;;    ~/projects/blog/static/favicon.ico)
;; (setq org-static-blog-page-header
;; "<meta name=\"author\" content=\"Bruce\">
;; <meta name=\"referrer\" content=\"no-referrer\">
;; <link href= \"/styles/org-manual.css\" rel=\"stylesheet\" type=\"text/css\" />
;; <link rel=\"icon\" href=\"static/favicon.ico\">
;; <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
;; <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
;; <link rel=\"stylesheet\" href=\"/styles/font.css\">
;; <link rel=\"stylesheet\" media=\"screen and (min-width: 600px)\" href=\"/styles/post.css\">
;; <link rel=\"stylesheet\" media=\"screen and (max-width: 600px)\" href=\"/styles/post_mobile.css\">
;; <link rel=\"stylesheet\" media=\"screen and (min-width: 600px)\" href=\"/styles/navigatebar.css\">
;; <link rel=\"stylesheet\" media=\"screen and (max-width: 600px)\" href=\"/styles/navigatebar_mobile.css\">
;; <link rel=\"stylesheet\" href=\"/theme/highlight.css\">

;; ")

;; ;; This preamble is inserted at the beginning of the <body> of every page:
;; ;;   This particular HTML creates a <div> with a simple linked headline
;; (setq org-static-blog-page-preamble
;; "<div class=\"navigatebar\">
;;     <div class=\"navigatebar-button navigatebar-mine\">
;;         <a href=\"/index.html\">Free World</a>
;;     </div>
;;     <div class=\"navigatebar-slogan\">
;;         「生活可以更简单, 欢迎来到我的开源世界」
;;     </div>
;;     <div class=\"navigatebar-button\">
;;         <a href=\"/index.html\">Home</a>
;;     </div>
;;     <div class=\"navigatebar-button\">
;;         <a href=\"/tags.html\">Tags</a>
;;     </div>
;;     <div class=\"navigatebar-button\">
;;         <a href=\"/rss.xml\">Feeds</a>
;;     </div>
;;     <div class=\"navigatebar-button navigatebar-about\">
;;         <a href=\"/about.html\">About</a>
;;     </div>
;; </div>

;;   ")

;; ;; This postamble is inserted at the end of the <body> of every page:
;; ;; This particular HTML creates a <div> with a link to the archive page and a licensing stub.
;; (setq org-static-blog-page-postamble
;;       (concat "<div id=\"archive\" style=\"padding-top: 3em; padding-bottom: 2em;\"><a href=\"/archive.html\">"
;;               (org-static-blog-gettext 'other-posts)
;;               "</a></div><script src=\"/js/av-min-1.5.0.js\"></script>"))

;; (defun my-org-inline-css-hook (exporter)
;;   "Insert custom inline css to EXPORTER."
;;   (when (eq exporter 'html)
;;     (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
;;            (path (concat dir "org-manual.css"))
;;            (homestyle (or (null dir) (null (file-exists-p path))))
;;            (final (if homestyle "~/.emacs.d/documents/org-manual.css" path))) ;; <- set your own style file path
;;       (setq org-html-head-include-default-style nil)
;;       (setq org-html-head (concat
;;                            "<style type=\"text/css\">\n"
;;                            "<!--/*--><![CDATA[/*><!--*/\n"
;;                            (with-temp-buffer
;;                              (insert-file-contents final)
;;                              (buffer-string))
;;                            "/*]]>*/-->\n"
;;                            "</style>\n")))))

;; (add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)


;; (add-to-list 'auto-mode-alist (cons (concat org-static-blog-posts-directory ".*\\.org\\'") 'org-static-blog-mode))

;; (defun org-static-blog-generate-post-path (post-filename post-datetime)
;;   "Custom html path."
;;   (concat (format-time-string "%Y/%m/%d" post-datetime)
;;           "/"
;;           (file-name-nondirectory post-filename)))

;; (require 'org-download)
;; ;; Drag-and-drop to `dired`
;; (add-hook 'dired-mode-hook 'org-download-enable)


;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)) 'append)

;; The following custom-set-faces create the highlights
(custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t))




(add-to-list 'Info-default-directory-list "~/org/doc")
;; flyspell mode for spell checking everywhere
;; (add-hook 'org-mode-hook 'turn-on-flyspell 'append)

(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(provide 'suk-org)
;;; suk-org.el ends here
