;;; Commentary: suk-org
;;; suk-org --- My org-mode settings.


;;; Code:
;;; The following lines are always needed. Choose your own keys.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Publishing and Exporting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Explicitly load required exporters
(require 'ox-html)
(require 'ox-latex)
(require 'ox-ascii)


; List of projects
;; (setq org-publish-project-alist
;;       (quote (
;;               ; org-mode-doc-org this document
;;               ; org-mode-doc-extra are images and css files that need to be included
;;               ; org-mode-doc is the top-level project that gets published
;;               ; This uses the same target directory as the 'doc' project
;;               ("org-mode-doc-org"
;;                :base-directory "d:/temp/blog"
;;                :publishing-directory "d:/temp/out"
;;                :recursive t
;;                :section-numbers nil
;;                :table-of-contents nil
;;                :base-extension "org"
;;                :publishing-function (org-html-publish-to-html)
;;                :plain-source t
;;                :htmlized-source t
;;                :style-include-default nil
;;                :html-head "<link rel=\"stylesheet\" href=\"/org.css\" type=\"text/css\" />"
;;                :author-info nil
;;                :creator-info nil)
;;               ("org-mode-doc-extra"
;;                :base-directory "~/org/"
;;                :publishing-directory "/tmp/htdocs"
;;                :base-extension "css\\|pdf\\|png\\|jpg\\|gif\\|org"
;;                :publishing-function org-publish-attachment
;;                :recursive t
;;                :author nil)
;;               ("org-mode-doc"
;;                :components ("org-mode-doc-org" "org-mode-doc-extra"))
;;               )))

; I'm lazy and don't want to remember the name of the project to publish when I modify
; a file that is part of a project.  So this function saves the file, and publishes
; the project that includes this file
;
; It's bound to C-S-F12 so I just edit and hit C-S-F12 when I'm done and move on to the next thing
(defun bh/save-then-publish (&optional force)
  (interactive "P")
  (save-buffer)
  (org-save-all-org-buffers)
  (let ((org-html-head-extra)
        (org-html-validation-link "<a href=\"http://validator.w3.org/check?uri=referer\">Validate XHTML 1.0</a>"))
    (org-publish-current-project force)))

(global-set-key (kbd "C-S-<f12>") 'bh/save-then-publish)




(defgroup org-static-blog nil
  "Settings for a static blog generator using org-mode"
  :version "1.2.1"
  :group 'applications)

(defcustom org-static-blog-publish-url "https://example.com/"
  "URL of the blog."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-publish-title "Example.com"
  "Title of the blog."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-publish-directory "~/blog/"
  "Directory where published HTML files are stored."
  :group 'org-static-blog)

(defcustom org-static-blog-posts-directory "~/blog/posts/"
  "Directory where published ORG files are stored.
When publishing, posts are rendered as HTML, and included in the
index, archive, tags, and RSS feed."
  :group 'org-static-blog)

(defcustom org-static-blog-drafts-directory "~/blog/drafts/"
  "Directory where unpublished ORG files are stored.
When publishing, draft are rendered as HTML, but not included in
the index, archive, tags, or RSS feed."
  :group 'org-static-blog)

(defcustom org-static-blog-index-file "index.html"
  "File name of the blog landing page.
The index page contains the most recent
`org-static-blog-index-length` full-text posts."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-index-length 5
  "Number of articles to include on index page."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-archive-file "archive.html"
  "File name of the list of all blog posts.
The archive page lists all posts as headlines."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-tags-file "tags.html"
  "File name of the list of all blog posts by tag.
The tags page lists all posts as headlines."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-enable-tags nil
  "Show tags below posts, and generate tag pages."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-enable-deprecation-warning t
  "Show deprecation warnings."
  :group 'org-static-blog)

(defcustom org-static-blog-rss-file "rss.xml"
  "File name of the RSS feed."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-page-header ""
  "HTML to put in the <head> of each page."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-page-preamble ""
  "HTML to put before the content of each page."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-page-postamble ""
  "HTML to put after the content of each page."
  :group 'org-static-blog
  :safe t)

(defcustom org-static-blog-langcode "zh"
  "Language code for the blog content."
  :group 'org-static-blog
  :safe t)

;; localization support
(defconst org-static-blog-texts
  '((other-posts
     ("en" . "Other posts")
     ("zh" . "其它文章"))
    (date-format
     ("en" . "%d %b %Y")
     ("zh" . "%Y-%m-%d"))
    (tags
     ("en" . "Tags")
     ("zh" . "标签"))
    (archive
     ("en" . "Archive")
     ("zh" . "归档"))
    (posts-tagged
     ("en" . "Posts tagged")
     ("zh" . "标签文章"))
    (no-prev-post
     ("en" . "There is no previous post")
     ("zh" . "没有前一篇文章"))
    (no-next-post
     ("en" . "There is no next post")
     ("zh" . "没有下一篇文章"))
    (title
     ("en" . "Title: ")
     ("zh" . "标题: "))
    (filename
     ("en" . "Filename: ")
     ("zh" . "文件名: "))))

(defun org-static-blog-gettext (text-id)
  "Return localized text.
Depends on org-static-blog-langcode and org-static-blog-texts."
  (let* ((text-node (assoc text-id org-static-blog-texts))
     (text-lang-node (if text-node
                 (assoc org-static-blog-langcode text-node)
               nil)))
    (if text-lang-node
    (cdr text-lang-node)
      (concat "[" (symbol-name text-id) ":" org-static-blog-langcode "]"))))



(defun org-static-blog-needs-publishing-p (post-filename)
  "Check whether POST-FILENAME was changed since last render."
  (let ((pub-filename
         (org-static-blog-matching-publish-filename post-filename)))
    (not (and (file-exists-p pub-filename)
              (file-newer-than-file-p pub-filename post-filename)))))


(defun org-static-blog-get-post-filenames ()
  "Returns a list of all posts."
  (directory-files-recursively
   org-static-blog-posts-directory ".*\\.org$"))

(defun org-static-blog-get-draft-filenames ()
  "Returns a list of all drafts."
  (directory-files-recursively
   org-static-blog-drafts-directory ".*\\.org$"))

(defun org-static-blog-file-buffer (file)
  "Return the buffer open with a full filepath, or nil."
  (require 'seq)
  (make-directory (file-name-directory file) t)
  (car (seq-filter
         (lambda (buf)
           (string= (with-current-buffer buf buffer-file-name) file))
         (buffer-list))))
(defun org-static-blog-get-date (post-filename)
  "Extract the `#+date:` from POST-FILENAME as date-time."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents post-filename)
      (goto-char (point-min))
      (if (search-forward-regexp "^\\#\\+date:[ ]*<\\([^]>]+\\)>$" nil t)
      (date-to-time (match-string 1))
    (time-since 0)))))

(defun org-static-blog-get-title (post-filename)
  "Extract the `#+title:` from POST-FILENAME."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents post-filename)
      (goto-char (point-min))
      (search-forward-regexp "^\\#\\+title:[ ]*\\(.+\\)$")
      (match-string 1))))

(defun org-static-blog-get-tags (post-filename)
  "Extract the `#+filetags:` from POST-FILENAME as list of strings."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents post-filename)
      (goto-char (point-min))
      (if (search-forward-regexp "^\\#\\+filetags:[ ]*:\\(.*\\):$" nil t)
          (split-string (match-string 1) ":")
    (if (search-forward-regexp "^\\#\\+filetags:[ ]*\\(.+\\)$" nil t)
            (split-string (match-string 1))
      )))))


(defun org-static-blog-get-tag-tree ()
  "Return an association list of tags to filenames.
e.g. `(('foo' 'file1.org' 'file2.org') ('bar' 'file2.org'))`"
  (let ((tag-tree '()))
    (dolist (post-filename (org-static-blog-get-post-filenames))
      (let ((tags (org-static-blog-get-tags post-filename)))
        (dolist (tag tags)
          (if (assoc-string tag tag-tree t)
              (push post-filename (cdr (assoc-string tag tag-tree t)))
            (push (cons tag (list post-filename)) tag-tree)))))
    tag-tree))

(defun org-static-blog-get-absolute-url (relative-url)
  "Returns absolute URL based on the RELATIVE-URL passed to the function.

For example, when `org-static-blog-publish-url` is set to 'https://example.com/'
and `relative-url` is passed as 'archive.html' then the function
will return 'https://example.com/archive.html'."
  (concat org-static-blog-publish-url relative-url))


(defun org-static-blog-open-previous-post ()
  "Opens previous blog post."
  (interactive)
  (let ((posts (sort (org-static-blog-get-post-filenames)
                     (lambda (x y)
                       (time-less-p (org-static-blog-get-date y)
                                    (org-static-blog-get-date x)))))
        (current-post (buffer-file-name)))
    (while (and posts
                (not (string-equal
                      (file-name-nondirectory current-post)
                      (file-name-nondirectory (car posts)))))
      (setq posts (cdr posts)))
    (if (> (list-length posts) 1)
        (find-file (cadr posts))
      (message (org-static-blog-gettext 'no-prev-post)))))

(defun org-static-blog-open-next-post ()
  "Opens next blog post."
  (interactive)
  (let ((posts (sort (org-static-blog-get-post-filenames)
                     (lambda (x y)
                       (time-less-p (org-static-blog-get-date x)
                                    (org-static-blog-get-date y)))))
        (current-post (buffer-file-name)))
    (while (and posts
                (not (string-equal
                      (file-name-nondirectory current-post)
                      (file-name-nondirectory (car posts)))))
      (setq posts (cdr posts)))
    (if (> (list-length posts) 1)
        (find-file (cadr posts))
      (message (org-static-blog-gettext 'no-next-post)))))


;;;###autoload
(defun org-static-blog-create-new-post ()
  "Creates a new blog post.
Prompts for a title and proposes a file name. The file name is
only a suggestion; You can choose any other file name if you so
choose."
  (interactive)
  (let ((title (read-string (org-static-blog-gettext 'title))))
    (find-file (concat
                org-static-blog-posts-directory
                (read-string (org-static-blog-gettext 'filename)
                 (concat (format-time-string "%Y-%m-%d-" (current-time))
                     (replace-regexp-in-string "\s" "-" (downcase title))
                     ".org"))))
    (insert "#+title: " title "\n"
            "#+date: " (format-time-string "<%Y-%m-%d %H:%M>") "\n"
            "#+keywords: \n"
            "#+description: \n"
            "#+filetags: ")))


;; Key bindings
(define-key org-mode-map (kbd "C-c C-f") 'org-static-blog-open-next-post)
(define-key org-mode-map (kbd "C-c C-b") 'org-static-blog-open-previous-post)
(define-key org-mode-map (kbd "C-c C-n") 'org-static-blog-create-new-post)



;; (setq org-static-blog-publish-title "My Static Org Blog")
;; (setq org-static-blog-publish-url "https://staticblog.org/")
(setq org-static-blog-publish-directory "d:/temp/blog/")
(setq org-static-blog-posts-directory "d:/temp/blog/_src/posts")

;; (setq org-static-blog-enable-tags t)
;; (setq org-export-with-toc nil)
;; (setq org-export-with-section-numbers nil)

;; ;; This header is inserted into the <head> section of every page:
;; ;;   (you will need to create the style sheet at
;; ;;    ~/projects/blog/static/style.css
;; ;;    and the favicon at
;; ;;    ~/projects/blog/static/favicon.ico)
;; (setq org-static-blog-page-header
;; "<meta name=\"author\" content=\"John Dow\">
;; <meta name=\"referrer\" content=\"no-referrer\">
;; <link href= \"static/style.css\" rel=\"stylesheet\" type=\"text/css\" />
;; <link rel=\"icon\" href=\"static/favicon.ico\">")

;; ;; This preamble is inserted at the beginning of the <body> of every page:
;; ;;   This particular HTML creates a <div> with a simple linked headline
;; (setq org-static-blog-page-preamble
;; "<div class=\"header\">
;;   <a href=\"https://staticblog.org\">My Static Org Blog</a>
;; </div>")

;; ;; This postamble is inserted at the end of the <body> of every page:
;; ;;   This particular HTML creates a <div> with a link to the archive page
;; ;;   and a licensing stub.
;; (setq org-static-blog-page-postamble
;; "<div id=\"archive\">
;;   <a href=\"https://staticblog.org/archive.html\">Other posts</a>
;; </div>
;; <center><a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\"><img alt=\"Creative Commons License\" style=\"border-width:0\" src=\"https://i.creativecommons.org/l/by-sa/3.0/88x31.png\" /></a><br /><span xmlns:dct=\"https://purl.org/dc/terms/\" href=\"https://purl.org/dc/dcmitype/Text\" property=\"dct:title\" rel=\"dct:type\">bastibe.de</span> by <a xmlns:cc=\"https://creativecommons.org/ns#\" href=\"https://bastibe.de\" property=\"cc:attributionName\" rel=\"cc:attributionURL\">Bastian Bechtold</a> is licensed under a <a rel=\"license\" href=\"https://creativecommons.org/licenses/by-sa/3.0/\">Creative Commons Attribution-ShareAlike 3.0 Unported License</a>.</center>")
;; #+end_src

;; In order for this to work, you will also need to create a style sheet
;; at /~/projects/blog/static/style.css/, which might for example change
;; the appearance of the ~#preamble~, the ~#content~, and the
;; ~#postamble~.




;; Always hilight the current agenda line
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)) 'append)

;; The following custom-set-faces create the highlights
(custom-set-faces
;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t))

(setq org-agenda-include-diary nil)
(setq org-agenda-diary-file "~/org/diary.org")


;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; Start the weekly agenda on Monday
(setq org-agenda-start-on-weekday 1)

;; Display tags farther right
(setq org-agenda-tags-column -102)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Attachments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-id-method (quote uuidgen))
;; Say you want to attach a file x.sql to your current task. Create
;; the file data in /tmp/x.sql and save it.
;;
;; Attach the file with C-c C-a a and enter the filename: x.sql.
;; This generates a unique ID for the task and adds the file in the
;; attachment directory.
;;
;; * Attachments                                                        :ATTACH:
;;   :PROPERTIES:
;;   :Attachments: x.sql
;;   :ID:       f1d38e9a-ff70-4cc4-ab50-e8b58b2aaa7b
;;   :END:
;;
;; The attached file is saved in
;; data/f1/d38e9a-ff70-4cc4-ab50-e8b58b2aaa7b/. Where it goes
;; exactly isn't important for me – as long as it is saved and
;; retrievable easily. Org-mode copies the original file /tmp/x.sql
;; into the appropriate attachment directory.
;;
;; Tasks with attachments automatically get an ATTACH tag so you can
;; easily find tasks with attachments with a tag search.
;;
;; To open the attachment for a task use C-c C-a o. This prompts for
;; the attachment to open and TAB completion works here.
;;
;; The ID changes for every task header when a new ID is generated.
;;
;; It's possible to use named directories for attachments but I
;; haven't needed this functionality yet – it's there if you need
;; it.
;;
;; I store my org-mode attachments with my org files in a
;; subdirectory data. These are automatically added to my git
;; repository along with any other org-mode changes I've made.


(defun bh/show-org-agenda ()
  (interactive)
  (if org-agenda-sticky
      (switch-to-buffer "*Org Agenda( )*")
    (switch-to-buffer "*Org Agenda*"))
  (delete-other-windows))


(add-to-list 'Info-default-directory-list "~/org/doc")
;; flyspell mode for spell checking everywhere
;; (add-hook 'org-mode-hook 'turn-on-flyspell 'append)

(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(run-at-time "00:59" 3600 'org-save-all-org-buffers)


(provide 'suk-org)
;;; suk-org.el ends here
