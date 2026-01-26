(provide 'init-org-hydra)
(with-eval-after-load 'pretty-hydra
  (pretty-hydra-define+ hydra-global-org
    (:color blue :quit-key "q" :title "Org 操作")
    ("Timer"
     (("t" org-timer-start "Start Timer")
      ("s" org-timer-stop "Stop Timer")
      ("r" org-timer-set-timer "Set Timer")    ; 需在 orgmode 下使用
      ("p" org-timer "Print Timer")
      )
     "Clock"
     (("i" (org-clock-in '(4)) "Clock-In")
      ("o" org-clock-out "Clock-Out")
      ("j" org-clock-goto "Clock Goto")
      ("d" org-clock-display "Clock Display")
      ("R" org-clock-report "Clock Report")
      ("c" org-clock-cancel "Clock Cancel")
      ("m" org-clock-show-current "Clock Modeline"))
     "Capture"

     (("C" org-capture "Capture")
      ("l" org-capture-goto-last-stored "Last Capture")

      ("q" nil "Quit"))
     )))

;; 推荐设置快捷键
(global-set-key (kbd "C-c o") 'hydra-global-org/body)
