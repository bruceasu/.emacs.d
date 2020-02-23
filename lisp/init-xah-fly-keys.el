;; init-xah-fly-keys.el


;;; Commentary:
;;
;; xah-fly-keys configurations.
;;
;; HOW TO USE

;; M-x xah-fly-keys to toggle the mode on/off.

;; Important command/insert mode switch keys:

;; xah-fly-command-mode-activate (press 【<home>】 or 【F8】 or 【Alt+Space】 or 【menu】)

;; xah-fly-insert-mode-activate  (when in command mode, press qwerty letter key f. (Dvorak key u))

;; When in command mode:
;; 【f】 (or Dvorak 【u】) activates insertion mode.
;; 【Space】 is a leader key. For example, 【SPACE r】 (Dvorak 【SPACE p】) calls query-replace. Press 【SPACE C-h】 to see the full list.
;; 【Space Space】 also activates insertion mode.
;; 【Space Enter】 calls execute-extended-command.
;; 【a】 calls execute-extended-command or smex or helm (if they are installed).

;; The leader key sequence basically replace
;; ALL Emacs commands that starts with C-x key.

;; When using xah-fly-keys, you don't need to press Control or Meta,
;; with the following exceptions:

;; C-c for major mode commands.
;; C-g for cancel.
;; C-q for quoted-insert.
;; C-h for getting a list of keys following a prefix/leader key.

;; Leader key

;; You NEVER need to press Ctrl+x

;; Any emacs command that has a keybinding starting with C-x,
;; has also a key sequence binding in xah-fly-keys. For example,
;; 【C-x b】 switch-to-buffer is 【SPACE f】 (Dvorak 【SPACE u】)
;; 【C-x C-f】 find-file is 【SPACE i e】 (Dvorak 【SPACE c .】)
;; 【C-x n n】 narrow-to-region is 【SPACE l l】 (Dvorak 【SPACE n n】)
;; The first key we call it leader key. In the above examples,
;; the SPACE is the leader key.

;; When in command mode, the 【SPACE】 is a leader key.

;; globally, the leader key is the 【f9】 key. 【f9】 is leader key regardless it's in command mode or insert mode.

;; the following standard keys with Control are supported:

;;  ;; 【Ctrl+tab】 'xah-next-user-buffer
;;  ;; 【Ctrl+shift+tab】 'xah-previous-user-buffer
;;  ;; 【Ctrl+v】 paste
;;  ;; 【Ctrl+w】 close
;;  ;; 【Ctrl+z】 undo
;;  ;; 【Ctrl+n】 new
;;  ;; 【Ctrl+o】 open
;;  ;; 【Ctrl+s】 save
;;  ;; 【Ctrl+shift+s】 save as
;;  ;; 【Ctrl+shift+t】 open last closed
;;  ;; 【Ctrl++】 'text-scale-increase
;;  ;; 【Ctrl+-】 'text-scale-decrease

;; To disable both Control and Meta shortcut keys, add the following lines to you init.el before (require 'xah-fly-keys):
;; (setq xah-fly-use-control-key nil)
;; (setq xah-fly-use-meta-key nil)

;; I highly recommend setting 【capslock】 to send 【Home】. So that it acts as `xah-fly-command-mode-activate'.
;; see
;; How to Make the CapsLock Key do Home Key
;; http://ergoemacs.org/misc/capslock_do_home_key.html

;;; Code:

;; (use-package xah-fly-keys
;;   :ensure t
;;   :config
;;   (xah-fly-keys-set-layout "qwerty")
;;   (xah-fly-keys 1)
;;   )

(require 'xah-fly-keys)
(xah-fly-keys-set-layout "qwerty")
(xah-fly-keys 1)


;; possible layout values:

;; "azerty"
;; "azerty-be"
;; "colemak"
;; "colemak-mod-dh"
;; "dvorak"
;; "programer-dvorak"
;; "qwerty"
;; "qwerty-abnt"
;; "qwertz"
;; "workman"

(provide 'init-xah-fly-keys)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-yasnippet.el ends here
