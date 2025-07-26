
(require 'cl-lib)
(require 'json)

;; Emacs ➝ Java
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
;; Java ➝ Emacs
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



(defvar my-java-command-handlers
  '(("callback" . my-cmd-callback-handler)
    ("eval" . my-eval-handler))
  "存储 Java 命令与 Emacs 处理函数的映射。")

(defvar my-java-reserved-commands
  '("eval", "callback")
  "保留的命令，不允许覆盖。")

(defun my-eval-handler (json-data)
  "处理 eval 命令。"
  (let* ((callback (alist-get 'code (alist-get 'params json-data)))
         (data (alist-get 'data (alist-get 'params json-data))))
    (when (functionp (intern callback))
      (funcall (intern callback) data))
    )


  (defun my-cmd-callback-handler(json-data) 
    (let* ((callback (alist-get 'callback (alist-get 'params json-data)))
           (data (alist-get 'data (alist-get 'params json-data))))
      (when (functionp (intern callback))
        (funcall (intern callback) data)))
    

    (defun my-java-register-command (cmd handler)
      "注册 Java 命令处理函数。

CMD 为命令字符串，HANDLER 为 Emacs 处理函数。

示例：
\(defun my-new-handler (json-data)
  (message \"New command received: %S\" json-data))

\(my-java-register-command \"new-command\" 'my-new-handler)"
      (if (member cmd my-java-reserved-commands)
          (error "命令 %s 是保留命令，不能覆盖" cmd)
        (setq my-java-command-handlers
              (cons (cons cmd handler)
                    (assq-delete-all cmd my-java-command-handlers)))))


    (defvar my-java-buffer "" "存储未处理的 Java 响应数据。")

    (defun my-java-filter (proc data)
      "处理 Java 服务器返回的数据，处理粘包和断包。"
      (setq my-java-buffer (concat my-java-buffer data)) ;; 累积数据
      (process-java-data))

    (defun my-java-sentinel (proc event)
      "处理 Java 连接断开事件，检查剩余 buffer。"
      (message "Java 连接事件: %s" event)
      (when (or (string-match "closed" event)
                (string-match "deleted" event))
        (message "Java 连接丢失，尝试解析剩余数据...")
        (process-java-data) ;; 处理最后残留数据
        (setq my-java-buffer "") ;; 清空 buffer
        (run-at-time 2 nil #'start-java-process)))  ;; 2 秒后重连

    (defun my-java-filter (proc data)
      "处理 Java 服务器返回的二进制数据，并解析 JSON。"
      (setq my-java-buffer
            (concat my-java-buffer (decode-coding-string data 'utf-8))) ;; 转换二进制为字符串
      (while (string-match "\\([0-9]+\\)\\(.*\\)" my-java-buffer)
        (let* ((length (string-to-number (match-string 1 my-java-buffer)))
               (remaining (match-string 2 my-java-buffer)))
          (if (>= (length remaining) length)
              (let ((json-str (substring remaining 0 length)))
                (setq my-java-buffer (substring remaining length))
                (handle-java-output json-str)) ;; 解析 JSON
            (return)))))

    
    (defun handle-java-output (output)
      "处理来自 Java 的 JSON 数据."
      (let* ((json-string (substring my-java-buffer 4 json-end))
             (json-data (json-read-from-string json-string))
             (cmd (alist-get 'cmd json-data))
             (handler (alist-get (intern cmd) my-java-command-handlers)))
        (if handler
            (funcall handler json-data)
          (message "Unknown command from Java: %S" json-data)))
      )
    

  (defvar my-java-server-jar
    (expand-file-name "server.jar" (file-name-directory load-file-name))
    "Java 服务器 JAR 文件路径，默认为 javarpc.el 相同目录。用户可覆盖定义。")

  (defvar my-java-command "java"
    "Java 执行命令。用户可覆盖定义。")

  (defun my-java-start-server ()
    "启动 Java 服务器进程。"
    (setq my-java-process
          (start-process "java-server" "*java-server*" my-java-command "-jar" my-java-server-jar)))


  (defvar my-java-process nil "存储 Java 进程.")
  (defvar my-java-socket nil "保存 Java 服务器的 socket 连接。")
  (defvar my-java-buffer "" "缓存 Java 返回的数据.")
  (defun read-java-port ()
    "读取 Java 进程的动态端口."
    (when (file-exists-p "emacs-java-port.txt")
      (with-temp-buffer
        (insert-file-contents "emacs-java-port.txt")
        (string-to-number (buffer-string)))))
  
  (defun start-java-process ()
    "启动 Java 进程，如果它还没运行."
    (unless (process-live-p my-java-process)
      (make-thread
       (lambda ()
         (my-java-start-server)
         (run-at-time 2 nil 'connect-to-java))
       "my-java-server-thread"
       )
      ))

  
  (defun connect-to-java ()
    "连接到 Java 服务器."
    (let ((port (read-java-port)))
      (when port
        (setq my-java-socket (open-network-stream
                              "my-java" "*my-java-log*" "localhost" port :type 'binary))
        (setq my-java-socket
              (make-network-process
               :name "java-socket"
               :host "127.0.0.1"
               :service port  ;; Java 监听的端口
               :nowait t
               :filter #'my-java-filter
               :sentinel #'my-java-sentinel
               ))
        )))

  (defun send-to-java (cmd params &optional callback)
    "发送 JSON 数据到 Java 进程."
    (when (process-live-p my-java-socket)
      (let* ((json-string (encode-coding-string
                           (json-encode
                            `((type . "req")
                              (cmd . ,cmd)
                              (params . ,params)
                              (callback . ,callback)))
                           'utf-8))  ;; 显式使用 UTF-8 编码
             (json-length (format "%04d" (string-bytes json-string))) ;; 计算 **字节** 长度
             (message (concat json-length json-string)))
        (process-send-string my-java-socket message))))

  
  (start-java-process)



  (provide 'javarpc)
