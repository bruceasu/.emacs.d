(require 'leader-key)

;; 1. Outline path Jump
(setq org-goto-interface 'outline-path-completion)

(defun my/org-writing-mode ()
  "Show the current Org subtree and enable visual line wrapping."
  (interactive)
  (org-content)
  (visual-line-mode 1)
  (setq-local line-spacing (unless (display-graphic-p) 0.2)))

(keymap-set leader-key-map "o w" #'my/org-writing-mode)
(keymap-set leader-key-map "o j" #'org-goto)
;; C-c C-j → 秒跳技术点
;; 2. org-occur 过滤内容
;; C-c /
;; 3. ripgrep or grep
(provide 'init-org-nav)
