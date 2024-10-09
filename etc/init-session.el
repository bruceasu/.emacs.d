;;; init-session.el --- Init for session save/restore
;;; Require
(require 'auto-save)
(require 'basic-toolkit)

;;; Code:

(setq desktop-load-locked-desktop t) ; don't popup dialog ask user, load anyway
(setq desktop-restore-frames nil)    ; don't restore any frame

(defun emacs-session-restore ()
  "Restore emacs session."
  (interactive)
  (ignore-errors
    ;; Kill other windows.
    (delete-other-windows)
    ;; Kill unused buffers.
    (kill-unused-buffers)
    ;; Restore session.
    (desktop-read "~/.emacs.d/var/")
    ))

(defun emacs-session-save (&optional arg)
  "Save emacs session."
  (interactive "p")
  (ignore-errors
    (if (equal arg 4)
        ;; Kill all buffers if with prefix argument.
        (mapc 'kill-buffer (buffer-list))
      ;; Kill unused buffers.
      (kill-unused-buffers)
      ;; Save all buffers before exit.
      (auto-save-buffers))
    ;; Save session.
    (make-directory "~/.emacs.d/var/" t)
    (desktop-save "~/.emacs.d/var/")
    ;; Exit emacs.
    (kill-emacs)))
(global-set-key  (kbd "S-<f9>") 'emacs-session-save)
(provide 'init-session)

;;; init-session.el ends here
