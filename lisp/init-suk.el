(eval-when-compile
  (require 'init-custom))

;; 英语自动补全
(require 'company-english-helper)
;; 中文自动翻译为英语
;; youdao / google
(setq insert-translated-name-translate-engine "google")
(require 'insert-translated-name)

;;  https://github.com/manateelazycat/lazy-search
(require 'lazy-search)
;; Bind any keystroke you like to `lazy-search'.
;;
(global-set-key (kbd "M-s") 'lazy-search)

;; bbyac
(require 'bbyac)
(bbyac-global-mode 1)

;; 输入法
;;(require 'zyoy)
;;(require 'rain)
;;(require 'he)
;;(require 'he-quick)
;; (require 'flypy)
(require 'pyim-init)

(require 'load-abbrev)
(require 'load-calendar)
(require 'load-epa)
(require 'ox-html5presentation)
;;(require 'redo)
;;(require 'tabbar)
;;(require 'load-tabbar)

;(require 'color-theme-molokai)
;(color-theme-molokai)

(require 'org-download)
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)

(require 'load-awsome-pair)
(require 'auto-save)

(setq auto-save-silent t)   ; quietly save
(setq auto-save-delete-trailing-whitespace t)  ; automatically delete spaces at the end of the line when saving

(require 'aweshell)

(require 'org-static-blog)

(setq org-static-blog-publish-title "Free World")
(setq org-static-blog-publish-url "https://bruceasu.github.io/")
(setq org-static-blog-publish-directory "d:/03_projects/suk/bruceasu.github.io/")
(setq org-static-blog-posts-directory "d:/03_projects/suk/bruceasu.github.io/_src/")
(setq org-static-blog-drafts-directory "d:/03_projects/suk/bruceasu.github.io/_drafts/")
(setq org-static-blog-enable-tags t)
(setq org-static-blog-langcode "zh")
(setq  org-static-blog-index-length 99)
;;(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

;; This header is inserted into the <head> section of every page:
;;   (you will need to create the style sheet at
;;    ~/projects/blog/static/style.css
;;    and the favicon at
;;    ~/projects/blog/static/favicon.ico)
(setq org-static-blog-page-header
"<meta name=\"author\" content=\"Bruce\">
<meta name=\"referrer\" content=\"no-referrer\">
<link href= \"/styles/org-manual.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
<link rel=\"stylesheet\" href=\"/styles/font.css\">
<link rel=\"stylesheet\" media=\"screen and (min-width: 600px)\" href=\"/styles/post.css\">
<link rel=\"stylesheet\" media=\"screen and (max-width: 600px)\" href=\"/styles/post_mobile.css\">
<link rel=\"stylesheet\" media=\"screen and (min-width: 600px)\" href=\"/styles/navigatebar.css\">
<link rel=\"stylesheet\" media=\"screen and (max-width: 600px)\" href=\"/styles/navigatebar_mobile.css\">
<link rel=\"stylesheet\" href=\"/theme/highlight.css\">

")

;; This preamble is inserted at the beginning of the <body> of every page:
;;   This particular HTML creates a <div> with a simple linked headline
(setq org-static-blog-page-preamble
"<div class=\"navigatebar\">
    <div class=\"navigatebar-button navigatebar-mine\">
        <a href=\"/index.html\">Free World</a>
    </div>
    <div class=\"navigatebar-slogan\">
        「生活可以更简单, 欢迎来到我的开源世界」
    </div>
    <div class=\"navigatebar-button\">
        <a href=\"/index.html\">Home</a>
    </div>
    <div class=\"navigatebar-button\">
        <a href=\"/tags.html\">Tags</a>
    </div>
    <div class=\"navigatebar-button\">
        <a href=\"/rss.xml\">Feeds</a>
    </div>
    <div class=\"navigatebar-button navigatebar-about\">
        <a href=\"/about.html\">About</a>
    </div>
</div>

  ")

;; This postamble is inserted at the end of the <body> of every page:
;;   This particular HTML creates a <div> with a link to the archive page
;;   and a licensing stub.
(setq org-static-blog-page-postamble
      (concat "<div id=\"archive\" style=\"padding-top: 3em; padding-bottom: 2em;\"><a href=\"/archive.html\">"
              (org-static-blog-gettext 'other-posts)
              "</a></div><script src=\"/js/av-min-1.5.0.js\"></script>"))


(add-to-list 'auto-mode-alist (cons (concat org-static-blog-posts-directory ".*\\.org\\'") 'org-static-blog-mode))
;; (defun org-static-blog-generate-post-path (post-filename post-datetime)
;;   "Custom html path."
;;   (concat (format-time-string "%Y/%m/%d" post-datetime)
;;           "/"
;;           (file-name-nondirectory post-filename)))

(defun org-static-blog-publish-force nil
  "Publish blog."
  (interactive)
  (org-static-blog-publish t))

(use-package windmove
  :bind
  (("<f2> <right>" . windmove-right)
   ("<f2> <left>" . windmove-left)
   ("<f2> <up>" . windmove-up)
   ("<f2> <down>" . windmove-down)
   ))

;; Frequently-accessed files

;; Registers allow you to jump to a file or other location quickly.
;; To jump to a register, use C-x r j followed by the letter of the register.
;; Using registers for all these file shortcuts is probably a bit of
;; a waste since I can easily define my own keymap, but since I rarely
;; go beyond register A anyway. Also, I might as well add shortcuts for refiling.

(defvar my/refile-map (make-sparse-keymap))

(defmacro my/defshortcut (key file)
  `(progn
     (set-register ,key (cons 'file ,file))
     (define-key my/refile-map
       (char-to-string ,key)
       (lambda (prefix)
         (interactive "p")
         (let ((org-refile-targets '(((,file) :maxlevel . 6)))
               (current-prefix-arg (or current-prefix-arg '(4))))
           (call-interactively 'org-refile))))))


(define-key my/refile-map "," 'my/org-refile-to-previous-in-file)

;; (my/defshortcut ?e "~/.emacs.d/Sacha.org")
;; (my/defshortcut ?E "~/code/emacs-news/index.org")
;; (my/defshortcut ?i "~/cloud/orgzly/Inbox.org")
;; (my/defshortcut ?o "~/cloud/orgzly/organizer.org")
;; (my/defshortcut ?s "~/personal/sewing.org")
;; (my/defshortcut ?b "~/personal/business.org")
;; (my/defshortcut ?p "~/personal/google-inbox.org")
;; (my/defshortcut ?P "~/personal/google-ideas.org")
;; (my/defshortcut ?B "~/Dropbox/books")
(my/defshortcut ?n "~/notes")
;; (my/defshortcut ?N "~/sync/notes/QuickNote.md")
;; (my/defshortcut ?w "~/Dropbox/public/sharing/index.org")
;; (my/defshortcut ?W "~/Dropbox/public/sharing/blog.org")
;; (my/defshortcut ?j "~/personal/journal.org")
;; (my/defshortcut ?J "~/cloud/a/Journal.csv")
;; (my/defshortcut ?I "~/Dropbox/Inbox")
;; (my/defshortcut ?g "~/sachac.github.io/evil-plans/index.org")
;; (my/defshortcut ?c "~/code/dev/elisp-course.org")
;; (my/defshortcut ?C "~/personal/calendar.org")
;; (my/defshortcut ?l "~/dropbox/public/sharing/learning.org")
;; (my/defshortcut ?q "~/sync/notes/QuickNote.md")
;; (my/defshortcut ?Q "~/personal/questions.org")

(provide 'init-suk)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-suk.el ends here
