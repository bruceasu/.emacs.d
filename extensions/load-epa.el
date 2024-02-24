(provide 'load-epa)
;easypg.el===file break point===
;; easypg，emacs 自带
(require 'epa-file)
(epa-file-enable)
(setenv "GPG_AGENT_INFO" nil)
;; 总是使用对称加密
(setq epa-file-encrypt-to nil)
;; 允许缓存密码，否则编辑时每次保存都要输入密码
(setq epa-file-cache-passphrase-for-symmetric-encryption t)
;; 允许自动保存
(setq epa-file-inhibit-auto-save nil) 
