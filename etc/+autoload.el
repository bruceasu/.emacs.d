;;; +autoload.el --- some autoload functions
;;;
;;; Commentary:
;;
;; autoload settings
;;
;;; Code:

;; 切换透明
;;;###autoload
(defun suk/toggle-transparency ()
        (interactive)
        (let ((alpha (frame-parameter nil 'alpha)))
          (set-frame-parameter
           nil 'alpha
           (if (eql (cond ((numberp alpha) alpha)
                          ((numberp (cdr alpha)) (cdr alpha))
                          ;; Also handle undocumented (<active> <inactive>) form.
                          ((numberp (cadr alpha)) (cadr alpha)))
                    100)
               '(85 . 85) '(100 . 100)))))



;; ==============================================================
;; Network Proxy
;; --------------------------------------------------------------
;;;###autoload
(defun suk/proxy-http-show ()
  "Show http/https proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is \"%s\"" suk-proxy)
    (message "No proxy")))

(defun suk/proxy-http-enable ()
  "Enable http/https proxy."
  (interactive)
  (setq url-proxy-services `(("http" . ,suk-proxy)
                             ("https" . ,suk-proxy)
                             ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

;;;###autoload
(defun suk/proxy-http-disable ()
  "Disable http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

;;;###autoload
(defun suk/proxy-http-toggle ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
      (proxy-http-disable)
    (proxy-http-enable)))

;;;###autoload
(defun suk/proxy-socks-enable ()
  "Enable Socks proxy."
  (interactive)
  (setq url-gateway-method 'socks)
  (setq socks-noproxy '("localhost"))
  (setq socks-server '("Default server" "127.0.0.1" 1086 5))
  (message "Enable socks proxy."))

;;;###autoload
(defun suk/proxy-socks-disable ()
  "Disable Socks proxy."
  (interactive)
  (setq url-gateway-method 'native)
  (setq socks-noproxy nil)
  (message "Disable socks proxy."))


(provide '+autoload)
;;; +autoload.el ends here
