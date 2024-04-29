;;; init-buffers.el --- Initialize buffers configurations. -*- lexical-binding: t -*-

(eval-when-compile
  (require '+const)
  (require '+custom)
  (require '+func)
  (require 'buffer-extension))

(suk-set-key-bindings
 'global-set-key
 (list
  ;;(list (kbd "C-x b l") #'suk/count-brf-lines)

  (list (kbd "C-x x x") #'suk/switch-major-mode)
  (list (kbd "C-x x X") #'suk/get-mode-name)
  (list (kbd "C-x x n") #'suk/new-empty-buffer)
  (list (kbd "C-x x s") #'suk/create-scratch-buffer)
  (list (kbd "C-x x O") #'suk/create-scratch-org)
  (list (kbd "C-x x o") #'suk/new-org-buffer)
  (list (kbd "C-x x m") #'suk/switch-to-minibuffer)
  (list (kbd "C-x x c") #'copy-buffer-file-name-as-kill)
  (list (kbd "C-x x t") #'suk/toggle-margin-right)
  (list (kbd "C-x k")   #'suk/close-current-buffer)
  (list (kbd "C-x C-k")   #'suk/kill-all-buffers-except-current)
  (list (kbd "C-x K")  #'suk/kill-other-window-buffer) ;关闭其他窗口的
  (list (kbd "C-x x u") #'suk/revert-buffer-with-utf8)
  (list (kbd "C-x x g") #'suk/revert-buffer-with-gbk)
  ;;'([C-t]               transpose-chars)
  ;;'([S-f6]              hs-minor-mode)
  ;;'([S-f5]              toggle-truncate-lines)
  (list (kbd "C-S-t") #'suk/open-last-closed)
  (list (kbd "C-x R") #'recentf-open)
  (list (kbd "C-S-<f6>") #'suk/move-buffer-file)
  (list (kbd "C-S-<f2>")  #'suk/rename-file-and-buffer)
  ))

(with-eval-after-load 'hydra
  (defhydra my-hydra-buffers ()
    "
Describe Something: (q to quit)
^Open/Clsoe^         ^Actions^               ^Creation^
-------------------------------------------------------------------
_m_inbuffer          s_w_itch mode           _n_ew
_u_tf-8 realod       _G_et mode              _s_cratch
_g_bk reload         _c_opy name             _o_rg
_l_ast reopen        _M_ove                  _O_rg scratch
_r_ecentf            _t_oggle margin right
_k_ close current    re_n_ame
close _A_ll
_K_ close other
"

    ("n" suk/new-empty-buffer)
    ("s" suk/create-scratch-buffer)
    ("O" suk/create-scratch-org)
    ("o" suk/new-org-buffer)

    ("k" suk/close-current-buffer)
    ("A" suk/kill-all-buffers-except-current)
    ("K" suk/kill-other-window-buffer) ;关闭其他窗口的

    ("w" suk/switch-major-mode)
    ("G" suk/get-mode-name)
    ("c" copy-buffer-file-name-as-kill)
    ("M" suk/move-buffer-file)
    ("n" suk/rename-file-and-buffer)
    ("t" suk/toggle-margin-right)

    ("m" suk/switch-to-minibuffer)
    ("u" suk/revert-buffer-with-utf8)
    ("g" suk/revert-buffer-with-gbk)
    ("l" suk/open-last-closed)
    ("r" recentf-open)


    ("q" nil))
  (global-set-key (kbd "C-c b") 'my-hydra-buffers/body)
  )


(provide 'init-buffers)
;;; init-buffers.el ends here
