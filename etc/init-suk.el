(eval-when-compile
  (require '+custom))


(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (save-excursion
	(indent-region (point-min) (point-max) nil)))


;;绑定到F7键
(global-set-key [S-f7] 'indent-buffer)

(defun xah-narrow-to-region ()
  "Same as `narrow-to-region', but if no selection, narrow to the current block.
Version 2022-01-22"
  (interactive)
  (if (region-active-p)
      (progn
        (narrow-to-region (region-beginning) (region-end)))
    (progn
      (let ($p1 $p2)
        (save-excursion
          (if (re-search-backward "\n[ \t]*\n" nil "move")
              (progn (goto-char (match-end 0))
                     (setq $p1 (point)))
            (setq $p1 (point)))
          (if (re-search-forward "\n[ \t]*\n" nil "move")
              (progn (goto-char (match-beginning 0))
                     (setq $p2 (point)))
            (setq $p2 (point))))
        (narrow-to-region $p1 $p2)))))

;; 英语自动补全
;; (require 'company-english-helper)

(when sys/linuxp


  )

(require 'load-abbrev)
;;(require 'init-calendar)
;;(require 'load-epa)
;;(require 'redo)
;;(require 'tabbar)
;;(require 'load-tabbar)

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
