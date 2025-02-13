;; Your windows specific settings

;; For example, (w32-register-hot-key [M-tab]) lets you use M-TAB normally in Emacs;

;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key

(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key

(setq w32-pass-apps-to-system nil)
(setq w32-apps-modifier 'hyper) ; Menu key


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
