;; init-basic.el --- Initialize basic configurations.	-*- lexical-binding: t -*-

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
;; Basic configuration.
;;

;;; Code:

(eval-when-compile
  (require '+const)
  (require '+custom))

;; Personal information
(setq user-full-name suk-full-name)
(setq user-mail-address suk-mail-address)

;; Key Modifiers
(when sys/win32p
  ;; make PC keyboard's Win key or other to type Super or Hyper
  ;; (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)    ; Left Windows key
  (setq w32-apps-modifier 'hyper)       ; Menu/App key
  ;; (w32-register-hot-key [s-])
  (w32-register-hot-key [s-t])
  ;; scroll-bar
  (set-scroll-bar-mode 'right))

;; Environment
(when (or sys/mac-x-p sys/linux-x-p)
  (use-package exec-path-from-shell
    :init
    (setq exec-path-from-shell-check-startup-files nil)
    (setq exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH"))
    (setq exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-initialize)))

;; =========================================================
;; 换行符设置(只是设定保存文件的换行符，并不是转换文件)
;; Dos2Unix/Unix2Dos
;; ---------------------------------------------------------
(defun dos2unix ()
  "Convert the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun unix2dos ()
  "Convert the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; =========================================================
;; 另外一种解决乱码的办法，就是用命令
;; C-x <RET> r or M-x revert-buffer-with-coding-system or C-x C-m r
;; 来用指定的编码重新读入这个文件。
;; 以DOS格式重读文件（UNIX格式类似）
;; C-x C-m r dos
;;
;; 1. 查看当前 buffer 的编码：M-x describe-coding-system
;; 2. 列出所有编码：C-x <RET> r <TAB>
;; 3. 以指定编码重读当前buffer：C-x <RET> r utf-8，（revert-buffer-with-coding-system）
;; 4. 改变当前buffer的编码：C-x <RET> f utf-8，（set-buffer-file-coding-system）
;; 5. 设定下一步操作的编码格式：C-x <RET> c，（universal-coding-system-argument）
;; Revert buffer
(defun suk/revert-current-buffer ()
  "Revert the current buffer. key \\[suk/revert-current-buffer]."
  (interactive)
  (message "Revert this buffer.")
  (revert-buffer t t))

(defun suk/revert-buffer-no-confirm ()
  "执行`revert-buffer'时不需要确认. key \\[suk/revert-current-buffer]."
  (interactive)
  (when (buffer-file-name)
    (revert-buffer buffer-file-name t)
   )
  )
;;; =========================================================
;;; 用新编码重新读取文件
;;; ---------------------------------------------------------
(defun suk/revert-buffer-with-coding-system-no-confirm (coding-system)
  "Call `revert-buffer-with-coding-system' with CODING-SYSTEM, but when `revert-buffer' do not need confirm."
  (interactive "Coding system for visited file (default nil): ")
  (let ((coding-system-for-read coding-system))
    (suk/revert-buffer-no-confirm)))
;;; ---------------------------------------------------------
(defun suk/revert-buffer-with-gbk ()
  "Call `revert-buffer-with-coding-system-no-confirm' with gbk.
It is bound to \\[suk/revert-buffer-with-gbk]."
  (interactive)
  (suk/revert-buffer-with-coding-system-no-confirm 'gb18030))
;;; ---------------------------------------------------------
(defun suk/revert-buffer-with-utf8 ()
  "Call `revert-buffer-with-coding-system-no-confirm' with utf-8.
It is bound to \\[suk/revert-buffer-with-utf8]."
  (interactive)
  (suk/revert-buffer-with-coding-system-no-confirm 'utf-8))
;;; =========================================================

;; (bind-keys ("<f5>" . suk/revert-current-buffer)
;;            ("s-r" . suk/revert-current-buffer))

;; --------------------------------------------------------------

(defun no-junk-please-were-unixish ()
  "只用unix类换行格式."
  (let ((coding-str (symbol-name buffer-file-coding-system)))
    (when (string-match "-\\(?:dos\\|mac\\)$" coding-str)
      (set-buffer-file-coding-system 'unix))))

(add-hook 'find-file-hooks 'no-junk-please-were-unixish)


;; If you have a mix of lines that end in ^M and lines that don’t, you can use
;; ‘M-x replace-string RET C-q C-m RET’ to remove the ^M’s. ‘C-q’ quotes the key
;; you press right after it, letting you insert a ^M character. Typing ‘C-m’
;; won’t work – you have to hold down ‘Control’ while hitting ‘q’ followed by
;; ‘m’.

;; ==============================================================
;; buffers
;; --------------------------------------------------------------
(defvar suk/recently-closed-buffers nil
  "A list of recently closed buffers. Each element is (buffer name, file path). The max number to track is controlled by the variable `suk/recently-closed-buffers-max'.")

(defvar suk/recently-closed-buffers-max 40 "The maximum length for `suk/recently-closed-buffers'.")

(defun suk/close-current-buffer ()
  "Close the current buffer.

Similar to `kill-buffer', with the following addition:

• Prompt user to save if the buffer has been modified even if the
buffer is not associated with a file.

• If the buffer is editing a source file in an 'org-mode' file,
prompt the user to save before closing.

• If the buffer is a file, add the path to the list
`suk/recently-closed-buffers'.

• If it is the minibuffer, exit the minibuffer"
  (interactive)
  (let ($emacs-buff-p
        ($org-p (string-match "^*Org Src" (buffer-name))))

    (setq $emacs-buff-p (if (string-match "^*" (buffer-name)) t nil))

    (if (string= major-mode "minibuffer-inactive-mode")
        (minibuffer-keyboard-quit) ; if the buffer is minibuffer
      (progn
        ;; offer to save buffers that are non-empty and modified, even for
        ;; non-file visiting buffer. (because kill-buffer does not offer to save
        ;; buffers that are not associated with files)
        (when (and (buffer-modified-p)
                   (not $emacs-buff-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        (when (and (buffer-modified-p)
                   $org-p)
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (org-edit-src-save)
            (set-buffer-modified-p nil)))

        ;; save to a list of closed buffer
        (when (buffer-file-name)
          (setq suk/recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name)) suk/recently-closed-buffers))
          (when (> (length suk/recently-closed-buffers) suk/recently-closed-buffers-max)
            (setq suk/recently-closed-buffers (butlast suk/recently-closed-buffers 1))))

        ;; close
        (kill-buffer (current-buffer))))))

(defun suk/open-last-closed ()
  "Open the last closed file."
  (interactive)
  (if (> (length suk/recently-closed-buffers) 0)
      (find-file (cdr (pop suk/recently-closed-buffers)))
    (progn (message "No recently close buffer in this session."))))

(defun suk/open-recently-closed ()
  "Open recently closed file.Prompt for a choice."
  (interactive)
  (find-file (ido-completing-read "open:" (mapcar (lambda (f) (cdr f)) suk/recently-closed-buffers))))

(defun suk/list-recently-closed ()
  "List recently closed file."
  (interactive)
  (let (($buf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer $buf)
    (mapc (lambda ($f) (insert (cdr $f) "\n"))
          suk/recently-closed-buffers)))

(global-set-key (kbd "C-x k") 'suk/close-current-buffer)
(global-set-key (kbd "C-S-t") 'suk/open-last-closed) ; control+shift+t

;; new empty buffer
;; --------------------------------------------------------------
(defun suk/new-empty-buffer ()
  "Create a new empty buffer.
New buffer will be named “untitled” or “untitled<2>”, “untitled<3>”, etc.
It returns the buffer (for elisp programing)."
  (interactive)
  (let (($buf (generate-new-buffer "untitled")))
    (switch-to-buffer $buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    $buf
    ))

;; Reload
;; --------------------------------------------------------------
(defun suk/reload-emacs-configuration ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; Open custom file
;; --------------------------------------------------------------
(defun open-custom-file()
  "Open custom.el if exists, otherwise create it."
  (interactive)
  (let ((custom-example
         (expand-file-name "custom-example.el" user-emacs-directory)))
    (unless (file-exists-p custom-file)
      (if (file-exists-p custom-example)
          (copy-file custom-file)
        (error "Unable to find \"%s\"" custom-example)))
    (find-file custom-file)))

;; Create a new scratch buffer
;; --------------------------------------------------------------
(defun suk/create-scratch-buffer ()
  "Create a scratch buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (lisp-interaction-mode))

;; Save a file as utf-8
;; --------------------------------------------------------------
(defun suk/save-buffer-as-utf8 (coding-system)
  "Revert a buffer with `CODING-SYSTEM' and save as UTF-8."
  (interactive "zCoding system for visited file (default nil):")
  (revert-buffer-with-coding-system coding-system)
  (set-buffer-file-coding-system 'utf-8)
  (save-buffer))

;; 显示当前buffer或region或函数的行数和字符数
;; --------------------------------------------------------------
(defun suk/count-brf-lines (&optional is-fun)
  "显示当前buffer或region或函数的行数和字符数."
  (interactive "P")
  (let (min max)
    (if is-fun
        (save-excursion
          (beginning-of-defun) (setq min (point))
          (end-of-defun) (setq max (point))
          (message "当前函数%s内共有%d行, %d个字符" (which-function) (count-lines min max) (- max min)))
      (if mark-active
          (progn
            (setq min (min (point) (mark)))
            (setq max (max (point) (mark))))
        (setq min (point-min))
        (setq max (point-max)))
      (if (or (= 1 (point-min)) mark-active)
          (if mark-active
              (message "当前region内共有%d行, %d个字符" (count-lines min max) (- max min))
            (message "当前buffer内共有%d行, %d个字符" (count-lines min max) (- max min)))
        (let ((nmin min) (nmax max))
          (save-excursion
            (save-restriction
              (widen)
              (setq min (point-min))
              (setq max (point-max))))
          (message "narrow下buffer内共有%d行, %d个字符, 非narrow下buffer内共有%d行, %d个字符"
                   (count-lines nmin nmax) (- nmax nmin) (count-lines min max) (- max min)))))))

;; =========================================================
;; Browse the homepage
(defun suk/browse-homepage ()
  "Browse the Github page of Suk Emacs."
  (interactive)
  (browse-url suk-homepage))


;; ==============================================================
;; 这将从base添加所有第一级dirs并排除exclude-list中的dirs，
;; 而对于include-list中的dirs，它将添加该dir的所有第一级dirs。
;; (add-to-list 'load-path "~/.emacs.d/elpa/company-20170715.1035")
;; (add-to-list 'load-path "~/.local/site-lisp")
;;(suk/add-to-list-with-subdirs "~/.local/site-lisp"
;;                          '(".", "..")
;;                          '())
;; --------------------------------------------------------------
(defun suk/add-to-list-with-subdirs (base exclude-list include-list)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (member f exclude-list)))
        (add-to-list 'load-path name)
        (when (member f include-list)
          (add-to-list-with-subdirs name exclude-list include-list)))))
  (add-to-list 'load-path base))

;; ==============================================================
;; Update the git of emacs configuration
;; --------------------------------------------------------------
(defun suk-update-config ()
  "Update suk Emacs configurations to the latest version."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (if (file-exists-p dir)
        (progn
          (message "Updating Emacs configurations...")
          (cd dir)
          (shell-command "git pull --rebase")
          (message "Update finished. Restart Emacs to complete the process."))
      (message "\"%s\" doesn't exist." dir))))

(declare-function upgrade-packages 'init-package)
(defalias 'suk-update-packages 'upgrade-packages)
(defun suk-update()
  "Update confgiurations and packages."
  (interactive)
  (suk-update-config)
  (suk-update-packages nil))

(declare-function upgrade-packages-and-restart 'init-package)
(defalias 'suk-update-packages-and-restart 'upgrade-packages-and-restart)
(defun suk-update-and-restart ()
  "Update configurations and packages, then restart."
  (interactive)
  (suk-update-config)
  (suk-update-packages-and-restart nil))


;; =========================================================
;; 方便的切换major mode
;; ---------------------------------------------------------
(defvar suk/switch-major-mode-last-mode nil)
;; ---------------------------------------------------------
(defun suk/major-mode-heuristic (symbol)
  (and (fboundp symbol)
       (string-match ".*-mode$" (symbol-name symbol))))
;; ---------------------------------------------------------
(defun suk/switch-major-mode (mode)
  "切换major mode"
  (interactive
   (let ((fn suk/switch-major-mode-last-mode) val)
     (setq val
           (completing-read
            (if fn (format "切换major-mode为(缺省为%s): " fn) "切换major mode为: ")
            obarray 'suk/major-mode-heuristic t nil nil (symbol-name fn)))
     (list (intern val))))
  (let ((last-mode major-mode))
    (funcall mode)
    (setq switch-major-mode-last-mode last-mode)))
;; ---------------------------------------------------------
;; show major mode
(defun suk/get-mode-name ()
  "显示`major-mode'及`mode-name'"
  (interactive)
  (message "major-mode为%s, mode-name为%s" major-mode mode-name))



;; =========================================================
;; 方便快速跳到bookmark
;; ---------------------------------------------------------
(defun suk/ska-point-to-register()
  "Store cursorposition _fast_ in a register. Use ska-jump-to-register to jump back to the stored position."
  (interactive)
  (setq zmacs-region-stays t)
  (point-to-register 8))
;; ---------------------------------------------------------
(defun suk/ska-jump-to-register()
  "Switches between current cursorposition and position that was stored with ska-point-to-register."
  (interactive)
  (setq zmacs-region-stays t)
  (let ((tmp (point-marker)))
        (jump-to-register 8)
        (set-register 8 tmp)))

;; =========================================================
;; toggle-letter-case
;; ---------------------------------------------------------
(defun suk/toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case,
in that cyclic order."
  (interactive)

  (let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'word))
            pos2 (cdr (bounds-of-thing-at-point 'word))))

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char pos1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         (t (put this-command 'state "all lower") )
         )
        )
      )

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region pos1 pos2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))

      (upcase-region pos1 pos2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region pos1 pos2) (put this-command 'state "all lower"))
     )
    )
  )

;; =========================================================
;; 普通用户调用root权限写文件
;; ---------------------------------------------------------
(defun suk/sudo-edit (&optional arg)
  (interactive "P")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:"
                         (ido-read-file-name "Find file(as root): ")))
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))
  )
)
;; ---------------------------------------------------------
(defadvice ido-find-file (after suk/sudo-find-file activate)
      "Find file as root if necessary."
      (unless (and buffer-file-name
                   (file-writable-p buffer-file-name))
        (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
;; ---------------------------------------------------------
(defun suk/sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "Find File for sudo-edit: ")
  (let ((tramp-file-name (concat "/sudo::"
                                 (expand-file-name file-name)
                          )
         )
        )
       (find-file tramp-file-name)
  )
)

;; ---------------------------------------------------------
(defun suk/sudo-save ()
    (interactive)
    (if (not buffer-file-name)
        ; true condition
        (write-file (concat "/sudo:root@localhost:"
                            (ido-read-file-name "File:")
                    )
         )
        ; false condition
        (write-file (concat "/sudo:root@localhost:" buffer-file-name))
    )
)

;; ---------------------------------------------------------
;;emacs sudo编辑远端文件由 jay 发表于 on 六月 20日, 2011我在之前的一篇
;;文章里提到过在Emacs下使用sudo的方法。这个解决了我很多本地编辑的问题。
;;但是我还是抛不开vi，因为一直没有解决服务器上需要sudo才有权限的文件编
;;辑问题。现实中这是一个很普遍的现象，就是在服务器上关闭了root或者其他
;;用户的ssh登陆权限，是通过一般用户登陆以后通过sudo等方式获得权限后才能
;;进行进一步的文件编辑。而如果直接使用sudo，用比如
;;/sudo:user@host:filepath的方式来打开文件，Emacs会报错说这是一个远端文
;;件，不能使用sudo来进行操作。就因为这提示，导致我一直以来对于这样的情
;;况只能乖乖地开个shell跑到服务器上面去用vi编辑，编辑过程中的各种不爽在
;;此不表…… 不过当最终忍受不住这种只能用vi的寂寞后，终于下定决心看一下
;;tramp的手册，结果很好，发现了这么一章内容――Connecting to a remote
;;host using multiple hops，原来tramp是可以通过设置代理的方式来编辑那些
;;无法直接访问到的文件的。代理可以是各种Inline method，也可以是Gateway
;;method。所以通过ssh做跳板再sudo是完全可行的。设置的格式是(host user
;;proxy)，其中proxy可以使用%u和%h来通配输入的用户名和主机名。详细情况感
;;兴趣的童鞋可以细看手册，这儿就只贴出满足我的需求的代码了:
;;
;; 跳板：localhost -> machine1.abc.def.edu -> machine2.abc.def.edu
;(add-to-list 'tramp-default-proxies-alist
;             '(nil "\\`user\\'" "/ssh:%h:")
;)
;
;(add-to-list 'tramp-default-proxies-alist
;'("machine2.abc.def.edu"
;  nil
;  "/ssh:myname@machine1.abc.def.edu:"))
;; 经过这样的设置，就可以直接使用 /sudo:user@host:filepath 来编辑那些远端
;; 需要sudo的文件了。所以，泡杯茶，扔掉vi吧 :)
;; ---------------------------------------------------------
;; User Option: tramp-default-proxies-alist
;; tramp-default-proxies-alist specifies proxy hosts to pass through.
;; This variable is list of triples consisting of (host user proxy).
;;
;; The first match is the proxy host through which passes the file name
;; and the target host matching user@host. host and user are
;; regular expressions or nil, interpreted as a regular expression
;; which always matches.
;;
;; proxy is a literal TRAMP file name whose local name part is ignored,
;; and the method and user name parts are optional.
;;
;; The method must be an inline or gateway method (see Inline methods, see Gateway methods).
;; If proxy is nil, no additional hop is required reaching user@host.
;;
;; For example, to pass through the host ‘bastion.your.domain’ as user ‘bird’
;; to reach remote hosts outside the local domain:
;;
;; (add-to-list 'tramp-default-proxies-alist
;;              '("\\." nil "/ssh:bird@bastion.your.domain:"))
;; (add-to-list 'tramp-default-proxies-alist
;;              '("\\.your\\.domain\\'" nil nil))
;; Note: add-to-list adds elements at the beginning of a list.
;;       Therefore, most relevant rules must come last in the list.
;;
;; Proxy hosts can be cascaded in the alist. If there is another host called ‘jump.your.domain’,
;; which is the only host allowed to connect to ‘bastion.your.domain’, then:
;;
;; (add-to-list 'tramp-default-proxies-alist
;;              '("\\`bastion\\.your\\.domain\\'"
;;                "\\`bird\\'"
;;                "/ssh:jump.your.domain:"))
;; proxy can take patterns %h or %u for host or user respectively.
;;
;; To login as ‘root’ on remote hosts in the domain ‘your.domain’,
;; but login as ‘root’ is disabled for non-local access, then use this alist entry:
;;
;; (add-to-list 'tramp-default-proxies-alist
;;              '("\\.your\\.domain\\'" "\\`root\\'" "/ssh:%h:"))
;; Opening /sudo:randomhost.your.domain: first connects to ‘randomhost.your.domain’
;; via ssh under your account name, and then perform sudo -u root on that host.
;;
;; It is key for the sudo method in the above example to be applied on the host after reaching it and not on the local host.
;;
;; host, user and proxy can also take Lisp forms. These forms when evaluated must return either a string or nil.
;;
;; To generalize (from the previous example):
;; For all hosts, except my local one, first connect via ssh, and then apply sudo -u root:
;;
;; (add-to-list 'tramp-default-proxies-alist
;;              '(nil "\\`root\\'" "/ssh:%h:"))
;; (add-to-list 'tramp-default-proxies-alist
;;              '((regexp-quote (system-name)) nil nil))
;; The above configuration allows TRAMP connection as ‘root’ to remote Ubuntu hosts.
;;
;; tramp-default-proxies-alist is also used for passing through firewalls or proxy servers.
;;
;; For example, the local host ‘proxy.your.domain’ on port 3128 serves as HTTP proxy
;; to the outer world. User has access rights to another proxy server on ‘host.other.domain’.1 Then the configuration is:
;;
;; (add-to-list 'tramp-default-proxies-alist
;;              '("\\`host\\.other\\.domain\\'" nil
;;              "/tunnel:proxy.your.domain#3128:"))
;; Gateway methods in a multiple hop chain can be declared only as the first hop.
;;
;; Passing through hops involves dealing with restricted shells, such as rbash.
;; If TRAMP is made aware, then it would use them for proxies only.
;; ---------------------------------------------------------
;; Inline methods
;;
;; Inline methods use the same login connection to transfer file contents.
;; Inline methods are quick and easy for small files. They depend on
;; the availability of suitable encoding and decoding programs on the remote host.
;; For local source and destination, TRAMP may use built-in equivalents of
;; such programs in Emacs.
;;
;; Inline methods can work in situations where an external transfer program
;; is unavailable. Inline methods also work when transferring files between
;; different user identities on the same host.
;;
;; TRAMP checks the remote host for the availability and usability of
;; mimencode (part of the metamail package) or uuencode. TRAMP uses
;; the first reliable command it finds. TRAMP’s search path can be customized,
;; see Remote programs.
;;
;; In case both mimencode and uuencode are unavailable, TRAMP first transfers
;; a small Perl program to the remote host, and then tries that program
;; for encoding and decoding.
;;
;; To increase transfer speeds for large text files, use compression before encoding.
;; The variable tramp-inline-compress-start-size specifies the file size for
;; such optimization.
;;
;; rsh
;;     rsh is an option for connecting to hosts within local networks
;;     since rsh is not as secure as other methods.
;;
;; ssh
;;     ssh is a more secure option than others to connect to a remote host.
;;
;;     ssh can also take extra parameters as port numbers. For example,
;;     a host on port 42 is specified as host#42 (the real host name, a hash sign, then a port number).
;;     It is the same as passing -p 42 to the ssh command.
;;
;; telnet
;;     Connecting to a remote host with telnet is as insecure as the rsh method.
;;
;; su
;;     Instead of connecting to a remote host, su program allows editing as
;;     another user. The host can be either ‘localhost’ or the host returned
;;     by the function (system-name). See Multi-hops for an exception to this behavior.
;;
;; sudo
;;     Similar to su method, sudo uses sudo. sudo must have sufficient rights to start a shell.
;;
;; sshx
;;     Works like ssh but without the extra authentication prompts.
;;     sshx uses ‘ssh -t -t host -l user /bin/sh’ to open a connection
;;     with a “standard” login shell.
;;
;;     Note that sshx does not bypass authentication questions.
;;     For example, if the host key of the remote host is not known,
;;     sshx will still ask “Are you sure you want to continue connecting?”.
;;     TRAMP cannot handle such questions.
;;     Connections will have to be setup where logins can proceed without such questions.
;;
;;     sshx is useful for Windows users when ssh triggers an error about allocating a pseudo tty.
;;     This happens due to missing shell prompts that confuses TRAMP.
;;
;;     sshx supports the ‘-p’ argument.
;;
;; krlogin
;;     This method is also similar to ssh. It uses the krlogin -x command only for remote host login.
;;
;; ksu
;;     This is another method from the Kerberos suite. It behaves like su.
;;
;; plink
;;     plink method is for Windows users with the PuTTY implementation of SSH.
;;     It uses ‘plink -ssh’ to log in to the remote host.
;;
;;     Check the ‘Share SSH connections if possible’ control for that session.
;;
;;     plink method supports the ‘-P’ argument.
;;
;; plinkx
;;     Another method using PuTTY on Windows with session names instead of host names.
;;     plinkx calls ‘plink -load session -t’. User names and port numbers must be defined in the session.
;;
;;     Check the ‘Share SSH connections if possible’ control for that session.
;; ---------------------------------------------------------
;; Gateway methods
;;
;; Gateway methods are for proxy host declarations (see Multi-hops) so as to pass through
;; firewalls and proxy servers. They are not like the other methods that declare direct
;; connections to a remote host.
;;
;; A gateway method always comes with a port setting. TRAMP targets the port number with
;; the gateway method localhost#random_port from where the firewall or proxy server is accessed.
;;
;; Gateway methods support user name and password declarations for authenticating the corresponding
;; firewall or proxy server. Such authentication can be passed through only if granted access
;; by system administrators.
;;
;; tunnel
;; This method implements an HTTP tunnel via the CONNECT command (conforming to RFC 2616, 2817 specifications).
;; Proxy servers using HTTP version 1.1 or later protocol support this command.
;;
;; For authentication, this protocol uses only Basic Authentication (see RFC 2617).
;; When no port number is specified, this protocol defaults to 8080.
;;
;; socks
;; The socks method connects to SOCKSv5 servers (see RFC 1928) and supports
;; Username/Password Authentication.
;;
;; The default port number for the socks server is 1080, if not specified otherwise.


(defun suk/toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0)))

(fset 'delete-empty-lines (kbd "M-x flush-lines RET ^\s-*$ RET"))


;; =========================================================
;; 段落格式化
;; --------------------------------------------------------------
(defun suk/unfill-paragraph (&optional region)
    "Takes a multi-line paragraph (or REGION) and make it into a single line of text."
    (interactive (progn
                   (barf-if-buffer-read-only)
                   (list t)))
    (let ((fill-column (point-max)))
      (fill-paragraph nil region)))
(bind-key "M-Q" 'my/unfill-paragraph)

;; M-q will fill the paragraph normally, and C-u M-q will unfill it.
;; --------------------------------------------------------------
 (defun suk/fill-or-unfill-paragraph (&optional unfill region)
    "Fill paragraph (or REGION).
  With the prefix argument UNFILL, unfill it instead."
    (interactive (progn
                   (barf-if-buffer-read-only)
                   (list (if current-prefix-arg 'unfill) t)))
    (let ((fill-column (if unfill (point-max) fill-column)))
      (fill-paragraph nil region)))
(bind-key "M-q" 'suk/fill-or-unfill-paragraph)



;; ==============================================================
;; mist
;; --------------------------------------------------------------


;; Recompile elpa directory
;; --------------------------------------------------------------
(defun suk/recompile-elpa ()
  "Recompile packages in elpa directory. Useful if you switch Emacs versions."
  (interactive)
  (byte-recompile-directory package-user-dir nil t))

;; Recompile site-lisp directory
;; --------------------------------------------------------------
(defun suk/recompile-site-lisp ()
  "Recompile packages in site-lisp directory."
  (interactive)
  (byte-recompile-directory
   (concat user-emacs-directory "site-lisp") 0 t))



;; =========================================================
;; Start server
(use-package server
  :ensure nil
  :hook (after-init . server-mode))

;; Emacs可以做为一个server, 然后用emacsclient连接这个server,
;; 无需再打开两个Emacs，windows下还不支持daemon的方式。

;;(server-force-delete)
;; (server-start)

;; History
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package recentf
  :ensure nil
  ;; lazy load recentf
  ;; :hook (find-file . (lambda () (unless recentf-mode
  ;;                            (recentf-mode)
  ;;                            (recentf-track-opened-file))))
  :init
  (add-hook 'after-init-hook #'recentf-mode)
  (setq recentf-max-saved-items 200)
  :config
  (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
  (add-to-list 'recentf-exclude ".cache")
  (add-to-list 'recentf-exclude ".cask")
  (add-to-list 'recentf-exclude ".elfeed")
  (add-to-list 'recentf-exclude "bookmarks")
  (add-to-list 'recentf-exclude "cache")
  (add-to-list 'recentf-exclude "persp-confs")
  (add-to-list 'recentf-exclude "recentf")
  (add-to-list 'recentf-exclude "url")
  (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              history-length 1000
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))

(setq system-time-locale "C")

;; 编码设置 begin
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; 23.2 之后废弃，用buffer-file-coding-system
;(setq default-buffer-file-coding-system 'utf-8)
(setq buffer-file-coding-system 'utf-8)

;; for windows
;; (setq-default pathname-coding-system 'euc-cn)
;; (set-language-environment 'Chinese-GB)
;; 据说设置为UTF-8不会卡顿
(set-language-environment "UTF-8")
;;(setq file-name-coding-system 'euc-cn)
;;(setq file-name-coding-system 'gb18030)
;;(if is-os-windows
;;	(setq file-name-coding-system 'gb18030))

;; 重要提示:写在最后一行的，实际上最优先使用; 最前面一行，反而放到最后才识别。
;; utf-16le-with-signature 相当于 Windows 下的 Unicode 编码，这里也可写成
;; utf-16 (utf-16 实际上还细分为 utf-16le, utf-16be, utf-16le-with-signature等多种)
;; 繁体
(prefer-coding-system 'cp950)
;; 简体
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
;; Unicode
;(prefer-coding-system 'utf-16le-with-signature)
(prefer-coding-system 'utf-16)
;; 新建文件使用utf-8-unix方式
;; 如果不写下面两句，只写
;; (prefer-coding-system 'utf-8)
;; 这一句的话，新建文件以utf-8编码，行末结束符平台相关
(prefer-coding-system 'utf-8-dos)
(prefer-coding-system 'utf-8-unix)
(setq session-save-file-coding-system 'utf-8)
(set-charset-priority 'unicode)

(auto-compression-mode 1)
(setq default-major-mode 'text-mode)
(setq show-paren-style 'parentheses)
;; 当使用 M-x COMMAND 后，过 1 秒钟显示该 COMMAND 绑定的键。
(setq suggest-key-bindings 1)
;;只渲染当前屏幕语法高亮，加快显示速度
(setq font-lock-maximum-decoration t)
;; 书签文件的路径及文件名
(setq bookmark-default-file "~/tmp/emacs/.emacs.bmk")
;; 同步更新书签文件 ;; 或者退出时保存
(setq bookmark-save-flag 1)
;;C-x r m (name)  M-x bookmark-set  设置书签
;;C-x r b (name)  M-x bookmark-jump  跳转到书签
;;C-x r l         M-x bookmark-bmenu-list  书签列表
;;                M-x bookmark-delete  删除书签
;;                M-x bookmark-load  读取存储书签文件
;;备份策略
(setq backup-directory-alist '(("" . "~/tmp/emacs/backup")))
(setq-default make-backup-file t)
(setq make-backup-file t)
(setq make-backup-files t)
(setq version-control t)
(setq kept-old-versions 2)
(setq kept-new-versions 10)
(setq delete-old-versions t)

(setq adaptive-fill-regexp "[ \t]+\\|[ \t]*\\([0-9]+\\.\\|\\*+\\)[ \t]*")
(setq adaptive-fill-first-line-regexp "^\\* *$")

;; 开启行号显示
;; (global-linum-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
;;(display-time-mode 1)

;; 简写模式
(setq-default abbrev-mode t)
(setq save-abbrevs nil)

;; (setq initial-scratch-message nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq kill-whole-line t)                   ; C-k deletes the end of line
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
(setq set-mark-command-repeat-pop t)       ; Repeating C-SPC after popping mark pops it again
;; (setq-default kill-whole-line t)           ; Kill line including '\n'

(setq-default major-mode 'text-mode)

;; 设置 sentence-end 可以识别中文标点。不用在 fill 时在句号后插 入两个空格。
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)
(setq kill-ring-max 200)
;; Save clipboard contents into kill-ring before replace them
(setq save-interprogram-paste-before-kill t)
;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)
;; chmod +x
;; ref. http://th.nao.ac.jp/MEMBER/zenitani/elisp-j.html#chmod
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; delete file if empty
;; ref. http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=delete%20nocontents
;; --------------------------------------------------------------
(add-hook 'after-save-hook 'delete-file-if-no-contents t)
(defun delete-file-if-no-contents ()
  (when (and buffer-file-name (= (point-min) (point-max)))
    (if (y-or-n-p "Delete file and kill buffer? ")
      (let ((filename buffer-file-name))
        (delete-file filename)
        (kill-buffer (current-buffer))
        (message (concat "Deleted " (file-name-nondirectory filename)))
        ))))


;; =========================================================
;; 通过编辑配置文件使其可以调用外部程序，来为其添加功能。
;; 增加命令
;;(defun lxr (names)
;;  (interactive "s查找联系人，请输入条件：")
;;  (call-process-shell-command "lxr" nil t t "-s" names))
;;执行命令
;;首先按功能键，Alt+x，然后输入命令 lxr 。
;;系统提示：“查找联系人，请输入条件："。
;;输入完成后，emacs 会执行命令lxr -s names，并输出执行的结果。
;; =========================================================



(provide 'init-basic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; init-basic.el ends here.
