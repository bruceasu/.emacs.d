;; init-bookmark.el --- bookmark configurations.    -*- lexical-binding: t -*-
;;; Commentary:
;;
;; Bookmark configuration.
;;

;;; Code:


;; 同步更新书签文件
;; 或者退出时保存
(setq bookmark-save-flag 1)

;;C-x r m (name)  M-x bookmark-set  设置书签
;;C-x r b (name)  M-x bookmark-jump  跳转到书签
;;C-x r l         M-x bookmark-bmenu-list  书签列表
;;                M-x bookmark-delete  删除书签
;;                M-x bookmark-load  读取存储书签文件

;; =========================================================
;; 方便快速跳到bookmark
;; ---------------------------------------------------------
;;;###autoload
(defun suk/ska-point-to-register()
  "Store cursorposition _fast_ in a register. Use ska-jump-to-register to jump back to the stored position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))
;; ---------------------------------------------------------
;;;###autoload
(defun suk/ska-jump-to-register()
  "Switch between current cursorposition and position that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
    (jump-to-register 8)
    (set-register 8 tmp)))

;; use init-key.el to load and bind the functions.
;;(global-set-key  [C-f7] 'suk/ska-point-to-register)
;;(global-set-key  [f7] 'suk/ska-jump-to-register)


;; bookmark

;; 这个是 Emacs 自带的功能，通过 C-x r m 调用 bookmark-set 函数，将文件夹设置为
;; bookmark。设置成功之后，就可以使用 C-x r j 跳转到文件夹当中。除书签功能之处，
;; 也可以使用 register 功能跳转到某个文件。

;; C-x r m <RET>
;; Set the bookmark for the visited file, at point.
;; C-x r m bookmark <RET>
;; Set the bookmark named bookmark at point (bookmark-set).
;; C-x r M bookmark <RET>
;; Like C-x r m, but don't overwrite an existing bookmark.
;; C-x r b bookmark <RET>
;; Jump to the bookmark named bookmark (bookmark-jump).
;; C-x r l
;; List all bookmarks (list-bookmarks).
;; M-x bookmark-save
;; Save all the current bookmark values in the default bookmark file.
;; M-x bookmark-load <RET> filename <RET>
;; Load a file named filename that contains a list of bookmark values. You can use this command, as well as bookmark-write, to work with other files of bookmark values in addition to your default bookmark file.
;; M-x bookmark-write <RET> filename <RET>
;; Save all the current bookmark values in the file filename.
;; M-x bookmark-delete <RET> bookmark <RET>
;; Delete the bookmark named bookmark.
;; M-x bookmark-insert-location <RET> bookmark <RET>
;; Insert in the buffer the name of the file that bookmark bookmark points to.
;; M-x bookmark-insert <RET> bookmark <RET>
;; Insert in the buffer the contents of the file that bookmark bookmark points to.


;; Registers allow you to jump to a file or other location quickly.
;; To jump to a register, use C-x r j followed by the letter of the register.
;; Using registers for all these file shortcuts is probably a bit of
;; a waste since I can easily define my own keymap, but since I rarely
;; go beyond register A anyway. Also, I might as well add shortcuts for refiling.
(require 'bookmark)
(defvar my-refile-map (make-sparse-keymap))
(defmacro my-defshortcut (key file)
  `(progn
     (set-register ,key (cons 'file ,file))
     (define-key my-refile-map
       (char-to-string ,key)
       (lambda (prefix)
         (interactive "p")
         (let ((org-refile-targets '(((,file) :maxlevel . 6)))
               (current-prefix-arg (or current-prefix-arg '(4))))
           (call-interactively 'org-refile))))))

;;(define-key my-refile-map "," 'my-org-refile-to-previous-in-file)
(my-defshortcut ?e "~/.emacs.d/init.el")
(my-defshortcut ?E "~/.emacs.d/custom.el")
;; (my-defshortcut ?i "~/cloud/orgzly/Inbox.org")
;; (my-defshortcut ?o "~/cloud/orgzly/organizer.org")
;; (my-defshortcut ?s "~/personal/sewing.org")
;; (my-defshortcut ?b "~/personal/business.org")
;; (my-defshortcut ?p "~/personal/google-inbox.org")
;; (my-defshortcut ?P "~/personal/google-ideas.org")
;; (my-defshortcut ?B "~/Dropbox/books")
(my-defshortcut ?n "~/notes")
;; (my-defshortcut ?N "~/sync/notes/QuickNote.md")
;; (my-defshortcut ?w "~/Dropbox/public/sharing/index.org")
;; (my-defshortcut ?W "~/Dropbox/public/sharing/blog.org")
;; (my-defshortcut ?j "~/personal/journal.org")
;; (my-defshortcut ?J "~/cloud/a/Journal.csv")
;; (my-defshortcut ?I "~/Dropbox/Inbox")
;; (my-defshortcut ?g "~/sachac.github.io/evil-plans/index.org")
;; (my-defshortcut ?c "~/code/dev/elisp-course.org")
;; (my-defshortcut ?C "~/personal/calendar.org")
;; (my-defshortcut ?l "~/dropbox/public/sharing/learning.org")
;; (my-defshortcut ?q "~/sync/notes/QuickNote.md")
;; (my-defshortcut ?Q "~/personal/questions.org")

(defun my-bookmark-set ()
  "Set and save bookmark.
If bookmark with same file name already exists, override it quietly."
  (interactive)
  (my-ensure 'bookmark)
  (bookmark-maybe-load-default-file)

  (let* ((filename (cond
                    ((eq major-mode 'eww-mode)
                     (eww-current-url))
                    (t
                     buffer-file-name)))
         existing-bookmark)
    (when (setq existing-bookmark
                (cl-find-if (lambda (b)
                              (let* ((f (cdr (assoc 'filename (cdr b)))))
                                (when (and f (file-exists-p f))
                                  (setq f (file-truename f)))
                                (string= f filename)))
                            bookmark-alist))
      ;; extract name of existing bookmark
      (setq existing-bookmark (car existing-bookmark)))
    (bookmark-set existing-bookmark)

    ;; save bookmark now
    (bookmark-save)

    (when existing-bookmark
      (message "Saved into existing bookmark \"%s\"" existing-bookmark))))


(defun my-bookmark-goto ()
  "Open ANY bookmark."
  (interactive)
  (my-ensure 'bookmark)
  (bookmark-maybe-load-default-file)
  ;; do the real thing
  (let* ((cands (delq nil (mapcar #'my-build-bookmark-candidate
                                  (and (boundp 'bookmark-alist)
                                       bookmark-alist))))
         (selected (completing-read "bookmarks:" cands)))
    (when selected
      (bookmark-jump (cdr (assoc selected cands))))))

(with-eval-after-load 'bookmark
  (defun my-build-bookmark-candidate (bookmark)
    "Re-shape BOOKMARK."
    (let* ((key (cond
                 ((and (assoc 'filename bookmark) (cdr (assoc 'filename bookmark)))
                  (format "%s (%s)" (car bookmark) (cdr (assoc 'filename bookmark))))
                 ((and (assoc 'location bookmark) (cdr (assoc 'location bookmark)))
                  (format "%s (%s)" (car bookmark) (cdr (assoc 'location bookmark))))
                 (t
                  (car bookmark)))))
      ;; key will be displayed
      ;; re-shape the data so full bookmark be passed to ivy-read
      (cons key bookmark)))

  ;; use my own bookmark if it exists
  (let ((file "~/var/.emacs.bmk"))
    (when (file-exists-p file)
      (setq bookmark-default-file file))))

;; Bookmark
(provide 'init-bookmark)
;;; init-bookmark.el ends here
