;; Your windows specific settings

;; on windows, make pwsh the default shell
;; 2023-09-09
(setq
 explicit-shell-file-name
 (cond
  ((eq system-type 'windows-nt)
   (let ((xlist
          (list
           "~/AppData/Local/Microsoft/WindowsApps/pwsh.exe"
           "C:/Windows/System32/WindowsPowerShell/v1.0/powershell.exe"
           )))
     (seq-some (lambda (x) (if (file-exists-p x) x nil)) xlist)))
  (t nil)))

(run-with-idle-timer
 2
 nil
 #'(lambda()
	 ;; Key Modifiers
	 ;; make PC keyboard's Win key or other to type Super or Hyper
	 ;; (setq w32-pass-lwindow-to-system nil)
	 (setq w32-lwindow-modifier 'super)    ; Left Windows key
	 (setq w32-apps-modifier 'hyper)       ; Menu/App key

	 ;; Optimization
	 (setq w32-get-true-file-attributes nil   ; decrease file IO workload
	       w32-use-native-image-API t         ; use native w32 API
	       w32-pipe-read-delay 0              ; faster IPC
	       w32-pipe-buffer-size 65536)        ; read more at a time (64K, was 4K)
	;;; Windows
    ;; spcial coding settings for Windows
    (unless (memq system-type '(cygwin windows-nt ms-dos))
      (setq selection-coding-system 'utf-8))
	 )


)

(when (display-graphic-p)
  ;; only graphic packages
)
(unless (display-graphic-p)
  ;; only conole packages
)
