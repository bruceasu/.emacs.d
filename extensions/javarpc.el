
(require 'cl-lib)
(require 'json)

;; Emacs ➝ RPC
;; {
;;     "type": "req",
;;     "cmd": "command1",
;;     "params": {
;;         "param1": "value1",
;;         "param2": "value2"
;;     },
;;     "callback": "theFunctionName" // 可选
;; }
;;
;; RPC ➝ Emacs
;; 返回结果
;; {
;;     "type": "resp",
;;     "cmd": "command1",
;;     "result": {
;;         "code": "ok",
;;         "message": "",
;;         "data": { }
;;     }
;; }
;; 如果有回调
;; {
;;     "type": "eval",
;;     "cmd": "callback",
;;     "params": {
;;         "callback": "theFunctionName",
;;         "data": { }
;;     }
;; }


(defvar sukrpc--buffer "" "存储未处理的响应数据。")
(defvar sukrpc--process nil "存储RPC服务器进程.")
(defvar sukrpc--socket nil "保存RPC服务器的 socket 连接。")
(defcustom sukrpc-port-file "sukrpc-port.txt" "保存RPC服务器端口文件")
(defvar sukrpc-dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Sukrpc 的目录，优先使用 `load-file-name`，否则使用 `buffer-file-name`。")

(defcustom sukrpc-server-cmd
  (expand-file-name
   (if (eq system-type 'windows-nt) "start-server.bat" "start-server.sh")
   sukrpc-dir)
  "RPC 服务器启动路径，默认为 sukrpc.el 相同目录。用户可覆盖定义。
启动的端口会写入 `sukrp-port-file` 文件。"
  :type 'string
  :group 'sukrpc)


(defvar sukrpc--command-handlers
  '(("callback" . sukrpc--callback-handler)
    ("eval" . sukrpc--eval-handler))
  "存储 Java 命令与 Emacs 处理函数的映射。")

(defvar sukrpc--reserved-commands
  '("eval", "callback")
  "保留的命令，不允许覆盖。")

(defun sukrpc--eval-handler (json-data)
  "处理 eval 命令。"
  (let* ((callback (alist-get 'code (alist-get 'params json-data)))
         (data (alist-get 'data (alist-get 'params json-data))))
    (when (functionp (intern callback))
      (funcall (intern callback) data))
    ))


(defun sukrpc--callback-handler(json-data) 
  (let* ((callback (alist-get 'callback (alist-get 'params json-data)))
         (data (alist-get 'data (alist-get 'params json-data))))
    (when (functionp (intern callback))
      (funcall (intern callback) data))))


(defun deno-bridge-get-free-port ()
  (save-excursion
    (let* ((process-buffer " *temp*")
           (process (make-network-process
                     :name process-buffer
                     :buffer process-buffer
                     :family 'ipv4
                     :server t
                     :host "127.0.0.1"
                     :service t))
           port)
      (setq port (process-contact process))
      (delete-process process)
      (kill-buffer process-buffer)
      (format "%s" (cadr port)))))

(defun sukrpc-register-command (cmd handler)
  "注册 RPC 命令处理函数。

CMD 为命令字符串，HANDLER 为 Emacs 处理函数。

示例：
\(defun my-new-handler (json-data)
  (message \"New command received: %S\" json-data))

\(my-java-register-command \"new-command\" 'my-new-handler)"
  (if (member cmd my-java-reserved-commands)
      (error "命令 %s 是保留命令，不能覆盖" cmd)
    (setq sukrpc--command-handlers
          (cons (cons cmd handler)
                (assq-delete-all cmd sukrpc--command-handlers)))))


(defun sukrpc-call (cmd params &optional callback)
  "发送 JSON 数据到 Java 进程."
  (when (process-live-p sukrpc--socket)
    (let* ((json-string (encode-coding-string
                         (json-encode
                          `((type . "req")
                            (cmd . ,cmd)
                            (params . ,params)
                            (callback . ,callback)))
                         'utf-8))  ;; 显式使用 UTF-8 编码
           (json-length (format "%04d" (string-bytes json-string))) ;; 计算 **字节** 长度
           (message (concat json-length json-string)))
      (process-send-string sukrpc--socket message))))

(defun sukrpc-start-server ()
  "启动 Java 进程，如果它还没运行."
  (interactive)
  (unless (process-live-p sukrpc--process)
    (make-thread
     (lambda ()
       (sukrpc--start-server)
       (run-at-time 2 nil 'sukrpc--connect))
     "sukrpc-server-thread"
     )
    ))

(defun sukrpc-stop-server()
  "彻底杀死进程."
  (interactive)
  ;; 关闭 socket 连接
  (when (process-live-p sukrpc--socket)
    (delete-process sukrpc--socket)
    (setq sukrpc--socket nil))
  (when (process-live-p sukrpc--process)
    (delete-process sukrpc--process)
    (setq sukrpc--process nil)
    ) 
  )

(defun restart-java-server ()
  "重启 RPC 服务器进程。"
  (interactive)
  ;; 如果进程存活，则先杀死
  (sukrpc-stop-server)
  ;; 启动新进程
  (sukrpc-start-server)
  )

(set-process-sentinel
 suk--process
 (lambda (proc event)
   (when (string-match "finished\\|exited\\|killed" event)
     (message "RPC server crashed, restarting...")
     (restart-java-server))))


(defun sukrpc--data-filter (proc data)
  "处理服务器返回的数据，处理粘包和断包。"
  (setq sukrpc--buffer (concat sukrpc--buffer  (decode-coding-string data 'utf-8))) ;; 累积数据
  (sukprc--process-data))

(defun sukrpc--connection-sentinel (proc event)
  "处理 Java 连接断开事件，检查剩余 buffer。"
  (message "RPC 连接事件: %s" event)
  (when (or (string-match "closed" event)
            (string-match "deleted" event))
    (message "RPC 连接丢失，尝试解析剩余数据...")
    (sukrpc--process-data) ;; 处理最后残留数据
    (setq sukrpc--buffer "") ;; 清空 buffer
    (run-at-time 2 nil #'sukrpc--connect)))  ;; 2 秒后重连

(defun sukrpc--process--data ()
  "处理 Java 服务器返回的二进制数据，并解析 JSON。"
  (while (string-match "\\([0-9]+\\)\\(.*\\)" sukrpc--buffer)
    (let* ((length (string-to-number (match-string 1 sukrpc--buffer)))
           (remaining (match-string 2 sukrpc--buffer)))
      (if (>= (length remaining) length)
          (let ((json-str (substring remaining 0 length)))
            (setq sukrpc--buffer (substring remaining length))
            (sukrpc--handle-json json-str)) ;; 解析 JSON
        (return)))))


(defun sukrpc--handle-json (output)
  "处理来自 Java 的 JSON 数据."
  (let* ((json-string (substring sukrpc--buffer 4 json-end))
         (json-data (json-read-from-string json-string))
         (cmd (alist-get 'cmd json-data))
         (handler (alist-get (intern cmd) my-java-command-handlers)))
    (if handler
        (funcall handler json-data)
      (message "Unknown command from RPC: %S" json-data)))
  )


(defun sukrpc--start-server ()
  "启动RPC服务器进程。"
  (setq sukrpc--process
        (start-process "sukrpc-server" "*sukrpc-server*" sukrpc-server-cmd)))

(defun sukrpc--get-port ()
  "读取 Java 进程的动态端口."
  (when (file-exists-p sukrpc-port-file)
    (with-temp-buffer
      (insert-file-contents sukrpc-port-file)
      (string-to-number (buffer-string)))))

(defun sukrpc--connect ()
  "连接到 RPC 服务器."
  (let ((port (sukrpc--get-port)))
    (when port
      (setq sukrpc--socket
            (make-network-process
             :name "rpc-socket"
             :host "127.0.0.1"
             :service port
             :nowait t
             :filter #'sukrpc--data-filter
             :sentinel #'sukrpc--connection-sentinel
             ))
      )))


(sukrpc-start-server)



(provide 'sukrpc)
