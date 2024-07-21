;;; init-buffers.el --- Initialize buffers configurations. -*- lexical-binding: t -*-

(eval-when-compile
  (require 'buffer-extension))

(with-eval-after-load 'hydra
  (defhydra my-hydra-buffers ()
    "
Describe Something: (q to quit)
^Open/Clsoe^         ^Actions^               ^Creation^
-------------------------------------------------------------------
_m_inbuffer          s_w_itch mode           _n_ew
_u_tf-8 realod       _G_et mode              _s_cratch
_g_bk reload         _d_elete file           _o_rg
_l_ast reopen        _M_ove                  _O_rg scratch
_R_eopen             _t_oggle margin right
_r_ecentf            re_n_ame
_k_ close current    _C_opy filename
_K_ close other      _c_ount
close _A_ll
"

    ("n" suk/new-empty-buffer)
    ("s" suk/create-scratch-buffer)
    ("O" suk/create-scratch-org)
    ("o" suk/new-org-buffer)
	("d" suk/delete-this-file)
    ("k" suk/close-current-buffer)
    ("A" suk/kill-all-buffers-except-current)
    ("K" suk/kill-other-window-buffer) ;关闭其他窗口的

    ("w" suk/switch-major-mode)
    ("C" suk/copy-file-name)
    ("c" suk/count-brf-lines)
    ("G" suk/get-mode-name)
    ("M" suk/move-buffer-file)
    ("n" suk/rename-file-and-buffer)
    ("t" suk/toggle-margin-right)

    ("m" suk/switch-to-minibuffer)
    ("u" suk/revert-buffer-with-utf8)
    ("g" suk/revert-buffer-with-gbk)
    ("l" suk/open-last-closed)
    ("r" recentf-open)
    ("R" suk/open-recently-closed)
    ("q" nil))
  ;;(global-set-key (kbd "C-c b") 'my-hydra-buffers/body)
  )


(provide 'init-buffers)
;;; init-buffers.el ends here
