(require 'rime)

;;; Code:
(setq rime-user-data-dir "~/.config/fcitx/rime")

(setq rime-posframe-properties
      (list :background-color "#333333"
            :foreground-color "#dcdccc"
            :font "asu-cjk-code-14"
            :internal-border-width 10))

(setq default-input-method "rime"
      ;; rime-show-candidate 'popup
      rime-show-candidate 'posframe
      )
(provide 'init-rime)
