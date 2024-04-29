;; -*- coding: utf-8; lexical-binding: t; -*-

;; {{ bbdb setup
(defun my-message-mode-hook-setup ()
  "Email composing set up."
  (bbdb-initialize 'message)
  (bbdb-initialize 'gnus)
  (local-set-key (kbd "TAB") 'bbdb-complete-name))
(add-hook 'message-mode-hook 'my-message-mode-hook-setup)

(with-eval-after-load 'gmail2bbdb
  ;; import Gmail contacts in vcard format into bbdb
  (setq gmail2bbdb-bbdb-file "~/.bbdb")
  (setq gmail2bbdb-exclude-people-without-name t))
;; }}

(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages."
  (interactive)
  (gnus-group-list-all-groups 5))

;; gnus+davmail bug, so I have to use pop3 for davmail
;; http://permalink.gmane.org/gmane.emacs.gnus.general/83301
;; but deleting all the mails on server is crazy
(setq pop3-leave-mail-on-server t)

(add-hook 'gnus-group-mode-hook
          ;; list all the subscribed groups even they contain zero un-read messages
          (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups )))

(defun my-use-gmail-smtp-server ()
  "Use Gmail SMTP server."
  (interactive)
  (setq smtpmail-default-smtp-server "smtp.gmail.com")
  (setq smtpmail-smtp-service 587))

(defun my-use-hotmail-smtp-server ()
  "Use hotmail SMTP server."
  (interactive)
  (setq smtpmail-default-smtp-server "smtp.office365.com")
  (setq smtpmail-smtp-service 587))

(setq message-send-mail-function 'smtpmail-send-it)
;; feel free to override this smtp set up in "~/.custom.el" or "~/.gnus.el"
(my-use-hotmail-smtp-server)

;; Ignore certificate hostname.
(setq starttls-extra-arguments '("--insecure"))

(defun message-select-forwarded-email-tags ()
  "Select the <#mml-or-what-ever> tags in `message-mode'."
  (interactive)
  (let (rlt)
    (when (search-forward "<#")
      (push-mark (point) t t)
      (goto-char (point-max))
      (search-backward ">")
      (forward-char)
      (setq rlt t))
    rlt))

(defun message-copy-select-forwarded-email-tags ()
  "Copy the <#mml-or-what-ever> tags in `message-mode'."
  (interactive)
  (save-excursion
    (cond
     ((message-select-forwarded-email-tags)
      (copy-region-as-kill (region-beginning) (region-end))
      (message "forwarded email tags copied!"))
     (t (message "NO forwarded email tags found!")))))


(with-eval-after-load 'gnus
  ;; {{ show email sent by `git send-email' in gnus
  ;; @see http://www.fnal.gov/docs/products/emacs/emacs/gnus_3.html#SEC60
  ;; QUOTED: If you are using an unthreaded display for some strange reason ...
  ;; Yes, when I search email in IMAP folder, emails are not threaded
  (setq gnus-article-sort-functions
        '((not gnus-article-sort-by-date)
          (not gnus-article-sort-by-number)))
  (local-require 'gnus-article-treat-patch)
  (setq gnus-article-patch-conditions
        '( "^@@ -[0-9]+,[0-9]+ \\+[0-9]+,[0-9]+ @@" )))

;; }}

(with-eval-after-load 'find-file-in-project
  (with-eval-after-load 'hydra
    (defhydra my-hydra-diff (:color blue)
      "
[_k_] Previous hunk
[_j_] Next hunk
[_p_] Previous file
[_n_] Next file
"
      ("k" diff-hunk-prev)
      ("j" diff-hunk-next)
      ("p" diff-file-prev)
      ("n" diff-file-next)
      ("q" nil)))
  ;; {{ gnus
  (defun ffip-diff-mode-hook-hydra-setup ()
    (local-set-key (kbd "C-c C-y") #'my-hydra-diff/body))
  (add-hook 'ffip-diff-mode-hook #'ffip-diff-mode-hook-hydra-setup))

;; gnus-summary-mode
(with-eval-after-load 'gnus-sum
  (with-eval-after-load 'hydra
    (defhydra my-hydra-gnus-summary (:color blue)
      "
[_F_] Forward (C-c C-f)             [_s_] Show thread
[_e_] Resend (S D e)                [_h_] Hide thread
[_r_] Reply                         [_n_] Refresh (/ N)
[_R_] Reply with original           [_!_] Mail -> disk
[_w_] Reply all (S w)               [_d_] Disk -> mail
[_W_] Reply all with original (S W) [_c_] Read all
[_G_] Search current folder         [_#_] Mark
[_b_] Switch Gnus buffer            [_A_] Show Raw article
"
      ("s" gnus-summary-show-thread)
      ("h" gnus-summary-hide-thread)
      ("n" gnus-summary-insert-new-articles)
      ("F" gnus-summary-mail-forward)
      ("!" gnus-summary-tick-article-forward)
      ("b" dianyou-switch-gnus-buffer)
      ("d" gnus-summary-put-mark-as-read-next)
      ("c" gnus-summary-catchup-and-exit)
      ("e" gnus-summary-resend-message-edit)
      ("R" gnus-summary-reply-with-original)
      ("r" gnus-summary-reply)
      ("W" gnus-summary-wide-reply-with-original)
      ("w" gnus-summary-wide-reply)
      ("#" gnus-topic-mark-topic)
      ("A" gnus-summary-show-raw-article)
      ("G" dianyou-group-make-nnir-group)
      ("q" nil))
    ;; y is not used by default
    (define-key gnus-summary-mode-map "y" 'my-hydra-gnus-summary/body)))

;; gnus-article-mode
(with-eval-after-load 'gnus-art
  (with-eval-after-load 'hydra
    (defhydra my-hydra-gnus-article (:color blue)
      "
[_o_] Save attachment        [_F_] Forward
[_v_] Play video/audio       [_r_] Reply
[_d_] CLI to download stream [_R_] Reply with original
[_b_] Open external browser  [_w_] Reply all (S w)
[_;_] Click link/button      [_W_] Reply all with original (S W)
[_b_] Switch Gnus buffer
"
      ("F" gnus-summary-mail-forward)
      ("r" gnus-summary-reply)
      ("R" gnus-article-reply-with-original)
      ("w" gnus-summary-wide-reply)
      ("W" gnus-article-wide-reply-with-original)
      ("o" (lambda () (interactive) (let* ((file (gnus-mime-save-part))) (when file (copy-yank-str file)))))
      ("v" my-browser-open-with-mplayer)
      ("d" my-browser-download-rss-stream)
      ("b" my-browser-open-link-or-image-or-url)
      (";" eww-lnum-follow)
      ("b" dianyou-switch-gnus-buffer)
      ("q" nil))
    ;; y is not used by default
    (define-key gnus-article-mode-map "y" 'my-hydra-gnus-article/body)))

;; message-mode
(with-eval-after-load 'message
  (with-eval-after-load 'hydra
    (defhydra my-hydra-message (:color blue)
      "
[_c_] Complete mail address [_H_] convert to html mail
[_a_] Attach file           [_p_] Paste image from clipboard
[_s_] Send mail (C-c C-c)
[_b_] Switch Gnus buffer
[_i_] Insert email address
"
      ("c" counsel-bbdb-complete-mail)
      ("a" mml-attach-file)
      ("s" message-send-and-exit)
      ("b" dianyou-switch-gnus-buffer)
      ("i" dianyou-insert-email-address-from-received-mails)
      ("H" org-mime-htmlize)
      ("p" dianyou-paste-image-from-clipboard)
      ("q" nil))

    )
  (defun message-mode-hook-hydra-setup ()
    (local-set-key (kbd "C-c C-y") 'my-hydra-message/body))
  (add-hook 'message-mode-hook 'message-mode-hook-hydra-setup))

;; }}

(provide 'init-gnus)
