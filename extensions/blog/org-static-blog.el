;;; org-static-blog.el --- a simple org-mode based static blog generator

;; Author: Bastian Bechtold
;; URL: https://github.com/bastibe/org-static-blog
;; Version: 1.2.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Static blog generators are a dime a dozen. This is one more, which
;; focuses on being simple. All files are simple org-mode files in a
;; directory. The only requirement is that every org file must have a
;; #+TITLE and a #+DATE, and optionally, #+FILETAGS.

;; This file is also available from marmalade and melpa-stable.

;; Set up your blog by customizing org-static-blog's parameters, then
;; call M-x org-static-blog-publish to render the whole blog or
;; M-x org-static-blog-publish-file filename.org to render only only
;; the file filename.org.

;; Above all, I tried to make org-static-blog as simple as possible.
;; There are no magic tricks, and all of the source code is meant to
;; be easy to read, understand and modify.

;; If you have questions, if you find bugs, or if you would like to
;; contribute something to org-static-blog, please open an issue or
;; pull request on Github.

;; Finally, I would like to remind you that I am developing this
;; project for free, and in my spare time. While I try to be as
;; accomodating as possible, I can not guarantee a timely response to
;; issues. Publishing Open Source Software on Github does not imply an
;; obligation to *fix your problem right now*. Please be civil.

;;; Code:

(require 'package)
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))
(require 'org)
(require 'ox-html)
;;(require 'ox-beamer)
;;(require 'ox-latex)
;;(require 'ox-ascii)

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

(defcustom org-static-blog-langcode "en"
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
     ("zh" . "前面没有文章了"))
    (no-next-post
     ("en" . "There is no next post")
     ("zh" . "后面没有文章了"))
    (title
     ("en" . "Title: ")
     ("zh" . "标题： "))
    (filename
     ("en" . "Filename: ")
     ("zh" . "文件名："))))

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


;;;###autoload
(defun org-static-blog-publish (&optional force-render)
  "Render all blog posts, the index, archive, tags, and RSS feed.
Only blog posts that changed since the HTML was created are
re-rendered.

With a prefix argument, all blog posts are re-rendered
unconditionally."
  (interactive "P")
  (dolist (file (append (org-static-blog-get-post-filenames)
                        (org-static-blog-get-draft-filenames)))
    (when (or force-render (org-static-blog-needs-publishing-p file))
      (org-static-blog-publish-file file)))
  ;; don't spam too many deprecation warnings:
  (let ((org-static-blog-enable-deprecation-warning nil))
    (org-static-blog-assemble-index)
    (org-static-blog-assemble-rss)
    (org-static-blog-assemble-archive)
    (if org-static-blog-enable-tags
        (org-static-blog-assemble-tags))))

(defun org-static-blog-needs-publishing-p (post-filename)
  "Check whether POST-FILENAME was changed since last render."
  (let ((pub-filename
         (org-static-blog-matching-publish-filename post-filename)))
    (not (and (file-exists-p pub-filename)
              (file-newer-than-file-p pub-filename post-filename)))))

(defun org-static-blog-matching-publish-filename (post-filename)
  "Generate HTML file name for POST-FILENAME."
  (concat org-static-blog-publish-directory
          (org-static-blog-get-post-public-path post-filename)))

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

;; This macro is needed for many of the following functions.
(defmacro org-static-blog-with-find-file (file contents &rest body)
  "Executes BODY in FILE. Use this to insert text into FILE.
The buffer is disposed after the macro exits (unless it already
existed before)."
  `(save-excursion
     (let ((current-buffer (current-buffer))
           (buffer-exists (org-static-blog-file-buffer ,file))
           (result nil)
       (contents ,contents))
       (if buffer-exists
           (switch-to-buffer buffer-exists)
         (find-file ,file))
       (erase-buffer)
       (insert contents)
       (setq result (progn ,@body))
       (basic-save-buffer)
       (unless buffer-exists
         (kill-buffer))
       (switch-to-buffer current-buffer)
       result)))

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
      (if (search-forward-regexp "^\\#\\+title:[ ]*\\(.+\\)$" nil t)
          (match-string 1)
        (file-name-nondirectory post-filename)))))

(defun org-static-blog-get-description (post-filename)
  "Extract the `#+description:` from POST-FILENAME."
  (let ((case-fold-search t))
    (with-temp-buffer
      (insert-file-contents post-filename)
      (goto-char (point-min))
      (if (search-forward-regexp "^\\#\\+description:[ ]*\\(.+\\)$" nil t)
         (match-string 1)
         ""))))

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

(defun org-static-blog-get-body (post-filename &optional exclude-title)
  "Get the rendered HTML body without headers from POST-FILENAME.
Preamble and Postamble are excluded, too."
  (with-temp-buffer
    (insert-file-contents (org-static-blog-matching-publish-filename post-filename))
    (buffer-substring-no-properties
     (progn
       (goto-char (point-min))
       (if exclude-title
           (progn (search-forward "<div class=\"title\">")
                  (search-forward "</div>"))
         (search-forward "<div id=\"content\">"))
       (point))
     (progn
       (goto-char (point-max))
       (search-backward "<div id=\"postamble\" class=\"status\">")
       (search-backward "</div>")
       (point)))))

(defun org-static-blog-get-absolute-url (relative-url)
  "Returns absolute URL based on the RELATIVE-URL passed to the function.

For example, when `org-static-blog-publish-url` is set to 'https://example.com/'
and `relative-url` is passed as 'archive.html' then the function
will return 'https://example.com/archive.html'."
  (concat org-static-blog-publish-url relative-url))

(defun org-static-blog-get-post-url (post-filename)
  "Returns absolute URL to the published POST-FILENAME.

This function concatenates publish URL and generated custom filepath to the
published HTML version of the post."
  (org-static-blog-get-absolute-url
          (org-static-blog-get-post-public-path post-filename)))

(defun org-static-blog-get-post-public-path (post-filename)
  "Returns post filepath in public directory.

This function retrieves relative path to the post file in posts or drafts
directories, the date of the post from its contents and then passes it to
`org-static-blog-generate-post-path` to generate custom filepath for the published
HTML version of the post."
  (org-static-blog-generate-post-path
   (org-static-blog-get-relative-path post-filename)
   (org-static-blog-get-date post-filename)))

(defun org-static-blog-get-relative-path (post-filename)
  "Remove absolute directory path from POST-FILENAME and changes
file extention from `.org` to `.html`. Returns filepath to HTML
file relative to posts or drafts directories.

Works with both posts and drafts directories.

For example, when `org-static-blog-posts-directory` is set to
'~/blog/posts' and `post-filename` is passed as
'~/blog/posts/my-life-update.org' then the function will return
'my-life-update.html'."
  (replace-regexp-in-string ".org$" ".html"
                            (replace-regexp-in-string
                             (concat "^\\("
                                     (file-truename org-static-blog-posts-directory)
                                     "\\|"
                                     (file-truename org-static-blog-drafts-directory)
                                     "\\)")
                             ""
                             post-filename)))

(defun org-static-blog-generate-post-path (post-filename post-datetime)
  "Return post public path based on POST-FILENAME and POST-DATETIME.

By default, this function returns post filepath unmodified, so script will
replicate file and directory structure of posts and drafts directories.

Override this function if you want to generate custom post URLs different
from how they are stored in posts and drafts directories.

For example, there is a post in posts directory with the
file path `hobby/charity-coding.org` and dated `<2019-08-20 Tue>`.

In this case, the function will receive following argument values:
- post-filename: 'hobby/charity-coding'
- post-datetime: datetime of <2019-08-20 Tue>

and by default will return 'hobby/charity-coding', so that the path
to HTML file in publish directory will be 'hobby/charity-coding.html'.

If this function is overriden with something like this:

(defun org-static-blog-generate-post-path (post-filename post-datetime)
  (concat (format-time-string \"%Y/%m/%d\" post-datetime)
          \"/\"
          (file-name-nondirectory post-filename)))

Then the output will be '2019/08/20/charity-coding' and this will be
the path to the HTML file in publish directory and the url for the post."
  post-filename)

;; -----------------------------------------------
;;;###autoload
(defun org-static-blog-publish-file (post-filename)
  "Publish a single POST-FILENAME.
The index, archive, tags, and RSS feed are not updated."
  (interactive "f")
  (org-static-blog-with-find-file
   (org-static-blog-matching-publish-filename post-filename)
   (concat
    "<!DOCTYPE html>\n"
    "<html lang=\"" org-static-blog-langcode "\">\n"
    "<head>\n"
    "<meta charset=\"UTF-8\">\n"
    "<link rel=\"alternate\"\n"
    "      type=\"application/rss+xml\"\n"
    "      href=\"" (org-static-blog-get-absolute-url org-static-blog-rss-file) "\"\n"
    "      title=\"RSS feed for " org-static-blog-publish-url "\"/>\n"
    "<title>" (org-static-blog-get-title post-filename) "</title>\n"
    org-static-blog-page-header
    "</head>\n"
    "<body>\n"
         org-static-blog-page-preamble
    "    <div class=\"content-area\">\n"
         (org-static-blog-post-preamble post-filename)
    "        <div id=\"content\">\n"
            (org-static-blog-render-post-content post-filename)"\n"
    "        </div>"
    "    </div>\n"
    "    <div id=\"postamble\" class=\"status\">"
        org-static-blog-page-postamble
    "    </div>\n"
    "</body>\n"
    "</html>\n")))

(defun org-static-blog-render-post-content (post-filename)
  "Render POST-FILENAME blog content as bare HTML without header."

  (let ((org-html-doctype "html5")
        (org-html-html5-fancy t))
    (save-excursion
      (let ((current-buffer (current-buffer))
        (buffer-exists (org-static-blog-file-buffer post-filename))
        (result nil))
    (if buffer-exists
        (switch-to-buffer buffer-exists)
      (find-file post-filename))
    (setq result
          (org-export-as 'org-static-blog-post-bare nil nil nil nil))
    (basic-save-buffer)
    (unless buffer-exists
      (kill-buffer))
    (switch-to-buffer current-buffer)
    result))))

(defun org-static-blog-post-preamble (post-filename)
  "Return the formatted date and headline of the POST-FILENAME.

This function is called for every post and prepended to the post body.
Modify this function if you want to change a posts headline."
  (concat
   (org-static-blog-post-preamble-title post-filename)
   (org-static-blog-post-preamble-tag post-filename)
   (org-static-blog-post-preamble-date post-filename)
   ))

(defun org-static-blog-post-preamble-title (post-filename)
"Return the formatted title of the POST-FILENAME.

This function is called for every post and prepended to the post body.
Modify this function if you want to change a posts headline."
  (concat
   "<div class=\"title\">"
   (org-static-blog-get-title post-filename)
   "</div>\n"))

(defun org-static-blog-post-preamble-date (post-filename)
"Return the formatted date of the POST-FILENAME.

This function is called for every post and prepended to the post body.
Modify this function if you want to change a posts headline."
  (concat "<div class=\"char-counter\">" (format-time-string (org-static-blog-gettext 'date-format)
                           (org-static-blog-get-date post-filename))
   "</div>\n"))

(defun org-static-blog-post-preamble-tag (post-filename)
  "Returns the tag list of the post.

This function is called for every post and appended to the post body.
Modify this function if you want to change a posts footline."
  (let ((taglist-content ""))
    (when (and (org-static-blog-get-tags post-filename) org-static-blog-enable-tags)
      (setq taglist-content (concat "<div class=\"category-area\">"))
      (dolist (tag (org-static-blog-get-tags post-filename))
        (setq taglist-content (concat taglist-content "<a href=\""
                                      (org-static-blog-get-absolute-url (concat "tags.html#" (downcase tag)))
                                      "\"><div class=\"category\">" tag "</div></a> ")))
      (setq taglist-content (concat taglist-content "</div>\n")))
    taglist-content))

;; -----------------------------------------------

(org-export-define-derived-backend 'org-static-blog-post-bare 'html
  :translate-alist '((template . (lambda (contents info) contents))))

(defun org-static-blog-assemble-index ()
  "Assemble the blog index page.
The index page contains the last `org-static-blog-index-length`
posts as full text posts."
  (let ((post-filenames (org-static-blog-get-post-filenames)))
    ;; reverse-sort, so that the later `last` will grab the newest posts
    (setq post-filenames (sort post-filenames (lambda (x y) (time-less-p (org-static-blog-get-date x)
                                                                         (org-static-blog-get-date y)))))
    (org-static-blog-assemble-multipost-page
     (concat org-static-blog-publish-directory org-static-blog-index-file)
     (last post-filenames org-static-blog-index-length))))

(defun org-static-blog-assemble-multipost-page
    (pub-filename post-filenames &optional front-matter)
  "Assemble a page that contains multiple posts one after another.
Posts are sorted in descending time."
  (setq post-filenames (sort post-filenames (lambda (x y) (time-less-p (org-static-blog-get-date y)
                                                                  (org-static-blog-get-date x)))))
  (org-static-blog-with-find-file
   pub-filename
   (concat
    "<!DOCTYPE html>\n"
    "<html lang=\"" org-static-blog-langcode "\">\n"
    "<head>\n"
    "<meta charset=\"UTF-8\">\n"
    "<link rel=\"alternate\"\n"
    "      type=\"application/rss+xml\"\n"
    "      href=\"" (org-static-blog-get-absolute-url org-static-blog-rss-file) "\"\n"
    "      title=\"RSS feed for " org-static-blog-publish-url "\"/>\n"
    "<title>" org-static-blog-publish-title "</title>\n"
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
    "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n"
    "<link rel=\"stylesheet\" href=\"styles/font.css\">\n"
    "<link rel=\"stylesheet\" media=\"screen and (min-width: 600px)\" href=\"styles/home.css\">\n"
    "<link rel=\"stylesheet\" media=\"screen and (max-width: 600px)\" href=\"styles/home_mobile.css\">\n"
    "</head>\n"
    "<body>\n"
    "  <img class=\"cover\" src=\"images/sixshot2.jpg\" />
         <div class=\"content\">
          <div class=\"topbar\">
            <div class=\"topbar-mine\">Free World</div>
            <div class=\"topbar-button\">
                <a href=\"index.html\">Home</a>
            </div>
            <div class=\"topbar-button\">
                <a href=\"tags.html\">Tags</a>
            </div>
            <div class=\"topbar-button\">
              <a href="\/rss.xml\">Feeds</a>
            </div>
            <div class=\"topbar-button\">
                <a href=\"about.html\">About</a>
            </div>
          </div>
          <div class=\"slogan\">
            「生活可以更简单, 欢迎来到我的开源世界」
          </div>\n"
    "    <ul id=\"list-content\">
             <div class=\"char-total-counter\"></div>\n"
    (when front-matter front-matter)
    (apply 'concat (mapcar 'org-static-blog-get-list-item post-filenames))
    "    </ul>"
    "    <div class=\"motto\">Smells Like Teen Spirit</div>\n"
    "    <div id=\"archive\" style=\"padding-top:1em; padding-left: 1em; padding-bottom: 2em;\">"
    "<a href=\"" (org-static-blog-get-absolute-url org-static-blog-archive-file) "\">" (org-static-blog-gettext 'other-posts) "</a>"
    "    </div>\n"
    "  </div>\n"
    "  <script src=\"js/av-min-1.5.0.js\"></script>\n"
    "  <!-- IE support
    <script src=\"https://cdnjs.cloudflare.com/ajax/libs/es5-shim/4.5.7/es5-shim.min.js\"></script>
    <script src=\"https://cdnjs.cloudflare.com/ajax/libs/es5-shim/4.5.7/es5-sham.min.js\"></script>
    <script src=\"https://cdnjs.cloudflare.com/ajax/libs/json3/3.3.2/json3.min.js\"></script>
     -->
    <script src=\"js/es5-shim.min.js\"></script>
    <script src=\"js/es5-sham.min.js\"></script>
    <script src=\"js/json3.min.js\"></script>
    <script src=\"js/template-web.js\"></script>"
    "</body>\n"
    "</html>\n")))

(defun org-static-blog-get-list-item (post-filename)
  "获取 POST-FILENAME 在首页列表项目."
  (message (concat "processing " post-filename))
  (concat
   "<li class=\"post-line\">\n"
   "  <a class=\"post-title\" href=\"" (org-static-blog-get-post-url post-filename) "\">"
   (org-static-blog-get-title post-filename)
   "  </a>\n"
   "  <div class=\"post-summary\">" (org-static-blog-get-description post-filename) "</div>\n"
   "  <div class=\"post-info\">\n"
   (org-static-blog-post-preamble-tag post-filename)
   "   <div class=\"right-info-area\">\n"
   "     <div class=\"char-counter\"></div>\n"
   "     <div class=\"post-date\">\n"
         (format-time-string (org-static-blog-gettext 'date-format)
                             (org-static-blog-get-date post-filename))
   "     </div>\n"
   "   </div>\n"
   " </div>\n"
   "</li>\n"
  )
)

;; -----------------------------------------------

(defun org-static-blog-assemble-rss ()
  "Assemble the blog RSS feed.
The RSS-feed is an XML file that contains every blog post in a
machine-readable format."
  (let ((rss-filename (concat org-static-blog-publish-directory org-static-blog-rss-file))
        (rss-items nil))
    (dolist (post-filename (org-static-blog-get-post-filenames))
      (let ((rss-date (org-static-blog-get-date post-filename))
            (rss-text (org-static-blog-get-rss-item post-filename)))
        (add-to-list 'rss-items (cons rss-date rss-text))))
    (setq rss-items (sort rss-items (lambda (x y) (time-less-p (car y) (car x)))))
    (org-static-blog-with-find-file
     rss-filename
     (concat "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
         "<rss version=\"2.0\">\n"
         "<channel>\n"
         "<title>" org-static-blog-publish-title "</title>\n"
         "<description>" org-static-blog-publish-title "</description>\n"
         "<link>" org-static-blog-publish-url "</link>\n"
         "<lastBuildDate>" (format-time-string "%a, %d %b %Y %H:%M:%S %z" (current-time)) "</lastBuildDate>\n"
         (apply 'concat (mapcar 'cdr rss-items))
         "</channel>\n"
         "</rss>\n"))))

(defun org-static-blog-get-rss-item (post-filename)
  "Assemble RSS item from post-filename.
The HTML content is taken from the rendered HTML post."
  (concat
   "<item>\n"
   "  <title>" (org-static-blog-get-title post-filename) "</title>\n"
   "  <description><![CDATA["
   (org-static-blog-get-body post-filename t) ; exclude headline!
   "]]></description>\n"
   (let ((categories ""))
     (when (and (org-static-blog-get-tags post-filename) org-static-blog-enable-tags)
       (dolist (tag (org-static-blog-get-tags post-filename))
         (setq categories (concat categories
                                  "  <category>" tag "</category>\n"))))
     categories)
   "  <link>"
   (org-static-blog-get-post-url post-filename)
   "</link>\n"
   "  <pubDate>"
   (format-time-string "%a, %d %b %Y %H:%M:%S %z" (org-static-blog-get-date post-filename))
   "</pubDate>\n"
   "</item>\n"))

;; -----------------------------------------------

(defun org-static-blog-assemble-archive ()
  "Re-render the blog archive page.
The archive page contains single-line links and dates for every
blog post, but no post body."
  (let ((archive-filename (concat org-static-blog-publish-directory org-static-blog-archive-file))
        (archive-entries nil)
        (post-filenames (org-static-blog-get-post-filenames)))
    (setq post-filenames (sort post-filenames (lambda (x y) (time-less-p
                                                        (org-static-blog-get-date y)
                                                        (org-static-blog-get-date x)))))
    (org-static-blog-with-find-file
     archive-filename
     (concat
      "<!DOCTYPE html>\n"
      "<html lang=\"" org-static-blog-langcode "\">\n"
      "<head>\n"
      "<meta charset=\"UTF-8\">\n"
      "<link rel=\"alternate\"\n"
      "      type=\"application/rss+xml\"\n"
      "      href=\"" (org-static-blog-get-absolute-url org-static-blog-rss-file) "\"\n"
      "      title=\"RSS feed for " org-static-blog-publish-url "\">\n"
      "<title>" org-static-blog-publish-title "</title>\n"
      org-static-blog-page-header
      "</head>\n"
      "<body>\n"
      org-static-blog-page-preamble
      "<div id=\"content\">\n"
      "<h1 class=\"title\">" (org-static-blog-gettext 'archive) "</h1>\n"
      (apply 'concat (mapcar 'org-static-blog-get-post-summary post-filenames))
      "</body>\n"
      "</html>"))))

;; -----------------------------------------------

(defun org-static-blog-get-post-summary (post-filename)
  "Assemble post summary for an archive page.
This function is called for every post on the archive and
tags-archive page. Modify this function if you want to change an
archive headline."
  (concat
   "<article class=\"archive-item\"><span style=\"padding-right:1.5em;\">"
   (format-time-string (org-static-blog-gettext 'date-format) (org-static-blog-get-date post-filename))
   "</span>"
   "<a href=\"" (org-static-blog-get-post-url post-filename) "\">" (org-static-blog-get-title post-filename) "</a>\n"
   "</article>"

   ))

(defun org-static-blog-assemble-tags ()
  "Render the tag archive and tag pages."
  (org-static-blog-assemble-tags-archive)
  ;; 每个标签生成一个页面。
  ;; (dolist (tag (org-static-blog-get-tag-tree))
  ;;   (org-static-blog-assemble-multipost-page
  ;;    (concat org-static-blog-publish-directory "tag-" (downcase (car tag)) ".html")
  ;;    (cdr tag)
  ;;    (concat "<h1 class=\"title\">" (org-static-blog-gettext 'posts-tagged) " \"" (car tag) "\":</h1>")))

  )

(defun org-static-blog-assemble-tags-archive-tag (tag)
  "Assemble single TAG for all filenames."
  (let ((post-filenames (cdr tag)))
    (setq post-filenames
      (sort post-filenames (lambda (x y) (time-less-p (org-static-blog-get-date x)
                             (org-static-blog-get-date y)))))
    (concat "<div class=\"archive-group\"><div id=\"" (downcase (car tag)) "\"></div><p></p>"
      "<a name=\"" (downcase (car tag)) "\"><div class=\"category-head\">「" (downcase (car tag)) "」</div></a>\n"
      "<a name=\"" (downcase (car tag)) "\"></a>\n"
      (apply 'concat (mapcar 'org-static-blog-get-post-summary post-filenames))
      "</div>\n")))

(defun org-static-blog-assemble-tags-archive ()
  "Assemble the blog tag archive page.
The archive page contains single-line links and dates for every
blog post, sorted by tags, but no post body."
  (let ((tags-archive-filename (concat org-static-blog-publish-directory org-static-blog-tags-file))
        (tag-tree (org-static-blog-get-tag-tree)))
    (setq tag-tree (sort tag-tree (lambda (x y) (string-greaterp (car y) (car x)))))
    (org-static-blog-with-find-file
     tags-archive-filename
     (concat
      "<!DOCTYPE html>\n"
      "<html lang=\"" org-static-blog-langcode "\">\n"
      "<head>\n"
      "  <meta charset=\"UTF-8\">\n"
      "  <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n"
      "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"

      "  <link rel=\"stylesheet\" href=\"styles/font.css\">\n"
      "  <link rel=\"stylesheet\" media=\"screen and (min-width: 600px)\" href=\"styles/tags.css\">\n"
      "  <link rel=\"stylesheet\" media=\"screen and (max-width: 600px)\" href=\"styles/tags_mobile.css\">\n"
      "  <link rel=\"stylesheet\" media=\"screen and (min-width: 600px)\" href=\"styles/navigatebar.css\">\n"
      "  <link rel=\"stylesheet\" media=\"screen and (max-width: 600px)\" href=\"styles/navigatebar_mobile.css\">\n"
      "<link rel=\"alternate\"\n"
      "      type=\"application/rss+xml\"\n"
      "      href=\"" (org-static-blog-get-absolute-url org-static-blog-rss-file) "\"\n"
      "      title=\"RSS feed for " org-static-blog-publish-url "\">\n"
      "<title>" org-static-blog-publish-title "</title>\n"
      "</head>\n"
      "<body>\n"
      org-static-blog-page-preamble
      "  <div class=\"tag-box\">\n"
      "    <div id=\"archives\">\n"
      (apply 'concat (mapcar 'org-static-blog-assemble-tags-archive-tag tag-tree))
      "    </div>\n"
      "    <div class=\"categories-area\">\n"
      (apply 'concat (mapcar 'org-static-blog-assemble-tags-archive-tag-area tag-tree))
      "    </div>\n"
      "  </div>"
      "</body>\n"
      "</html>\n"))))

(defun org-static-blog-assemble-tags-archive-tag-area (tag)
  "Assemble TAGs for all filenames."
  (let ((post-filenames (cdr tag)))
    (setq post-filenames
      (sort post-filenames (lambda (x y) (time-less-p (org-static-blog-get-date x)
                             (org-static-blog-get-date y)))))
    (concat
     "<a class=\"archive-item\" href=\"tags.html#" (downcase (car tag)) "\">\n"
     "  <div class=\"category-name\">" (downcase (car tag)) "</div>\n"
     "  <div class=\"category-number\"></div>\n"
     "</a>\n")))
;; -----------------------------------------------

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

(defun org-static-blog-open-matching-publish-file ()
  "Opens HTML for post."
  (interactive)
  (find-file (org-static-blog-matching-publish-filename (buffer-file-name))))

;;;###autoload
(defun org-static-blog-create-new-post-subdir (subdir)
  "Creates a new blog post.
Prompts for a title and proposes a file name. The file name is
only a suggestion; You can choose any other file name if you so
choose."
  (interactive)
  (let ((title (read-string (org-static-blog-gettext 'title))))
    (find-file (concat
                org-static-blog-posts-directory
                "/" subdir "/"
                (read-string (org-static-blog-gettext 'filename)
                 (concat (format-time-string "%Y-%m-%d-" (current-time))
                     (replace-regexp-in-string "\s" "-" (downcase title))
                     ".org"))))
    (insert "#+title: " title "\n"
            "#+date: " (format-time-string "<%Y-%m-%d %H:%M>") "\n"
            "#+filetags: \n#+description: \n")))

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
            "#+filetags: \n#+description: \n")))

;;;###autoload
(define-derived-mode org-static-blog-mode org-mode "OSB"
  "Blogging with org-mode and emacs."
  (run-mode-hooks)
  :group 'org-static-blog)

;; Key bindings
(define-key org-static-blog-mode-map (kbd "C-c C-f") 'org-static-blog-open-next-post)
(define-key org-static-blog-mode-map (kbd "C-c C-b") 'org-static-blog-open-previous-post)
(define-key org-static-blog-mode-map (kbd "C-c C-p") 'org-static-blog-open-matching-publish-file)
;(define-key org-static-blog-mode-map (kbd "C-c C-n") 'org-static-blog-create-new-post)
(define-key org-static-blog-mode-map (kbd "C-c C-m") '(org-static-blog-create-new-post-subdir wiki))
(define-key org-static-blog-mode-map (kbd "C-c C-n") '(org-static-blog-create-new-post-subdir posts))

(provide 'org-static-blog)
;; -----------------------------------------------

;; (setq org-static-blog-publish-title "Free World")
;; (setq org-static-blog-publish-url "http://localhost:8000/")
;; (setq org-static-blog-publish-directory "./")
;; (setq org-static-blog-posts-directory "_src/")
;; (setq org-static-blog-drafts-directory "_drafts/")
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
;;     <div class=\"navigatebar-button navigatebar-about\">
;;         <a href=\"/about.html\">About</a>
;;     </div>
;; </div>

;;   ")

;; ;; This postamble is inserted at the end of the <body> of every page:
;; ;;   This particular HTML creates a <div> with a link to the archive page
;; ;;   and a licensing stub.
;; (setq org-static-blog-page-postamble
;;       (concat "<div id=\"archive\" style=\"pading-top: 3em; padding-bottom: 2em;\"><a href=\"/archive.html\">"
;;               (org-static-blog-gettext 'other-posts)
;;               "</a></div><script src=\"/js/av-min-1.5.0.js\"></script>"))


;; (add-to-list 'auto-mode-alist (cons (concat org-static-blog-posts-directory ".*\\.org\\'") 'org-static-blog-mode))


;; (defun myweb-publish nil
;;   "Publish myweb."
;;   (interactive)
;;   (org-static-blog-publish t))

;;; org-static-blog.el ends here
