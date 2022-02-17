;;; init-sudo.el --- sudo .	-*- lexical-binding: t no-byte-compile: t; -*-

;; Copyright (C) 2018 Suk

;; Author: Suk
;; Version: 1.0.0
;; Keywords: .emacs.d

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
;; sudo setting
;;

;;; Code:


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
  (let ((tramp-file-name
		 (concat "/sudo::"
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
;;              '("\\`your\\.domain\\'" nil nil))
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

(provide 'init-sudo)
;;; init-sudo.el ends here
