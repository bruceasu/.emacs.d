(provide 'init-org-doc)
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
;; * Attachments                                     :ATTACH:
;;   :PROPERTIES:
;;   :Attachments: x.sql
;;   :ID:       f1d38e9a-ff70-4cc4-ab50-e8b58b2aaa7b
;;   :END:
;;
;; The attached file is saved in
;; data/f1/d38e9a-ff70-4cc4-ab50-e8b58b2aaa7b/. Where it goes
;; exactly isn't important for me 鈥?as long as it is saved and
;; retrievable easily. Org-mode copies the original file /tmp/x.sql
;; into the appropriate attachment directory.
;;
;; Tasks with attachments automatically get an ATTACH tag so you can
;; easily find tasks with attachments with a tag search.
;;
;; To open the attachment for a task use C-c C-a o. This prompts for
;; the attachment to open and TAB completion works here.

;; org-mode
(when (locate-library "toc-org")
  (use-package toc-org))
