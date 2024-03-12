;; init-bookmark.el --- bookmark configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2018 Suk

;; Author: Suk

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


;; Bookmark
(use-package bookmark
  :ensure nil
  :config
  (with-no-warnings
    ;; Display icons for bookmarks
    (defun my-bookmark-bmenu--revert ()
      "Re-populate `tabulated-list-entries'."
      (let (entries)
        (dolist (full-record (bookmark-maybe-sort-alist))
          (let* ((name       (bookmark-name-from-full-record full-record))
                 (annotation (bookmark-get-annotation full-record))
                 (location   (bookmark-location full-record))
                 (file       (file-name-nondirectory location))
                 (type       (let ((fmt "%-8.8s"))
                               (cond ((null location)
                                      (propertize (format fmt "NOFILE") 'face 'warning))
                                     ((file-remote-p location)
                                      (propertize (format fmt "REMOTE") 'face 'mode-line-buffer-id))
                                     ((not (file-exists-p location))
                                      (propertize (format fmt "NOTFOUND") 'face 'error))
                                     ((file-directory-p location)
                                      (propertize (format fmt "DIRED") 'face 'warning))
                                     (t (propertize (format fmt "FILE") 'face 'success)))))
                 (icon       (if (icons-displayable-p)
                                 (cond
                                  ((file-remote-p location)
                                   (nerd-icons-codicon "nf-cod-radio_tower"))
                                  ((file-directory-p location)
                                   (nerd-icons-icon-for-dir location))
                                  ((not (string-empty-p file))
                                   (nerd-icons-icon-for-file file)))
                               "")))
            (push (list
                   full-record
                   `[,(if (and annotation (not (string-equal annotation "")))
                          "*" "")
                     ,icon
                     ,(if (display-mouse-p)
                          (propertize name
                                      'font-lock-face 'bookmark-menu-bookmark
                                      'mouse-face 'highlight
                                      'follow-link t
                                      'help-echo "mouse-2: go to this bookmark in other window")
                        name)
                     ,type
                     ,@(if bookmark-bmenu-toggle-filenames
                           (list (propertize location 'face 'completions-annotations)))])
                  entries)))
        (tabulated-list-init-header)
        (setq tabulated-list-entries entries))
      (tabulated-list-print t))
    (advice-add #'bookmark-bmenu--revert :override #'my-bookmark-bmenu--revert)

    (defun my-bookmark-bmenu-list ()
      "Display a list of existing bookmarks.
The list is displayed in a buffer named `*Bookmark List*'.
The leftmost column displays a D if the bookmark is flagged for
deletion, or > if it is flagged for displaying."
      (interactive)
      (bookmark-maybe-load-default-file)
      (let ((buf (get-buffer-create bookmark-bmenu-buffer)))
        (if (called-interactively-p 'interactive)
            (pop-to-buffer buf)
          (set-buffer buf)))
      (bookmark-bmenu-mode)
      (bookmark-bmenu--revert))
    (advice-add #'bookmark-bmenu-list :override #'my-bookmark-bmenu-list)

    (define-derived-mode bookmark-bmenu-mode tabulated-list-mode "Bookmark Menu"
      (setq truncate-lines t)
      (setq buffer-read-only t)
      (setq tabulated-list-format
            `[("" 1) ;; Space to add "*" for bookmark with annotation
              ("" ,(if (icons-displayable-p) 2 0)) ;; Icons
              ("Bookmark" ,bookmark-bmenu-file-column bookmark-bmenu--name-predicate)
              ("Type" 9)
              ,@(if bookmark-bmenu-toggle-filenames
                    '(("File" 0 bookmark-bmenu--file-predicate)))])
      (setq tabulated-list-padding bookmark-bmenu-marks-width)
      (setq tabulated-list-sort-key '("Bookmark" . nil))
      (add-hook 'tabulated-list-revert-hook #'bookmark-bmenu--revert nil t)'
      (setq revert-buffer-function #'bookmark-bmenu--revert)
      (tabulated-list-init-header))))
(provide 'init-bookmark)
;;; init-bookmark.el ends here
