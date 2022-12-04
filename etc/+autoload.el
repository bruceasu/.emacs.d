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


;; 增加或减少透明
;;;###autoload
(defun sanityinc/adjust-opacity (frame incr)
  "Adjust the background opacity of FRAME by increment INCR."
  (unless (display-graphic-p frame)
    (error "Cannot adjust opacity of this frame"))
  (let* ((oldalpha (or (frame-parameter frame 'alpha) 100))
         ;; The 'alpha frame param became a pair at some point in
         ;; emacs 24.x, e.g. (100 100)
         (oldalpha (if (listp oldalpha) (car oldalpha) oldalpha))
         (newalpha (+ incr oldalpha)))
    (when (and (<= frame-alpha-lower-limit newalpha) (>= 100 newalpha))
      (modify-frame-parameters frame (list (cons 'alpha newalpha))))))

;; 增加透明度
;;;###autoload
(defun suk/less-alpha ()
  (interactive)
  (sanityinc/adjust-opacity nil -2))
;; 减少透明度
;;;###autoload
(defun suk/plus-alpha ()
  (interactive)
  (sanityinc/adjust-opacity nil 2))

(when  (eq system-type 'gnu/linux)
  ;; 调节屏幕亮度
;;;###autoload
  (defun suk/set-backlight (&optional light-value)
    (interactive "s请输入亮度(小数表示的百分比): ")
    (let ((max-backlight (string-to-number (string-trim-right
					    (shell-command-to-string
					     "cat /sys/class/backlight/intel_backlight/max_brightness")))))
      (when (and light-value (floatp (string-to-number light-value)))
	(shell-command
	 (concat "echo "
		 (format "%d" (* max-backlight (string-to-number light-value)))
		 " > /sys/class/backlight/intel_backlight/brightness")))))

  ;; 增加10%屏幕亮度
;;;###autoload
  (defun suk/plus-backlight ()
    (interactive)
    (let* (
	   ;; 最大亮度
	   (max-backlight (string-to-number (string-trim-right
					     (shell-command-to-string "cat /sys/class/backlight/intel_backlight/max_brightness"))))
	   ;; 当前亮度
	   (current-backlight (string-to-number (string-trim-right
						 (shell-command-to-string "cat /sys/class/backlight/intel_backlight/brightness"))))
	   ;; 增加后的亮度
	   (add-backlight (+ current-backlight (* max-backlight 0.1))))
      (if (< add-backlight max-backlight)
	  (progn (shell-command
		  (concat "echo "
			  (format "%d" add-backlight)
			  " > /sys/class/backlight/intel_backlight/brightness"))
		 (message "亮度+10%"))
	(message "亮度MAX!!"))))

  ;; 减少屏幕亮度
;;;###autoload
  (defun suk/less-backlight ()
    (interactive)
    (let* (
	   ;; 最大亮度
	   (max-backlight (string-to-number (string-trim-right
					     (shell-command-to-string "cat /sys/class/backlight/intel_backlight/max_brightness"))))
	   ;; 当前亮度
	   (current-backlight (string-to-number (string-trim-right
						 (shell-command-to-string "cat /sys/class/backlight/intel_backlight/brightness"))))
	   ;; 减少后的亮度
	   (less-backlight (- current-backlight (* max-backlight 0.1))))
      (if (> less-backlight (* max-backlight 0.1) )
	  (progn (shell-command
		  (concat "echo "
			  (format "%d" less-backlight)
			  " > /sys/class/backlight/intel_backlight/brightness"))
		 (message "亮度-10%"))
	(message "亮度Min!!"))))
)


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
;;;###autoload
(defun suk/proxy-http-enable ()
  "Enable http/https proxy."
  (interactive)
  (setq url-proxy-services `(("http" . suk-proxy)
                             ("https" . suk-proxy)
                             ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (suk/proxy-http-show))

;;;###autoload
(defun suk/proxy-http-disable ()
  "Disable http/https proxy."
  (interactive)
  (setq url-proxy-services nil)
  (suk/proxy-http-show))

;;;###autoload
(defun suk/proxy-http-toggle ()
  "Toggle http/https proxy."
  (interactive)
  (if url-proxy-services
      (suk/proxy-http-disable)
    (suk/proxy-http-enable)))

;;;###autoload
(defun suk/proxy-socks-enable ()
  "Enable Socks proxy."
  (interactive)
  (setq url-gateway-method 'socks)
  (setq socks-noproxy '("localhost"))
  (setq socks-server '("Default server" "127.0.0.1" 1080 5))
  (message "Enable socks proxy."))

;;;###autoload
(defun suk/proxy-socks-disable ()
  "Disable Socks proxy."
  (interactive)
  (setq url-gateway-method 'native)
  (setq socks-noproxy nil)
  (message "Disable socks proxy."))

;;;###autoload
(defun now ()
  (interactive)
  ( insert (org-time-stamp)))

(autoload 'calendar "init-calendar" "Config Chinese calendar " t)

(provide '+autoload)
;;; +autoload.el ends here
