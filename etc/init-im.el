;;; init-im -- 输入法
;;; summary:
;; 输入法
;; code:
;; 内置机制
;;(require 'zyoy)
;;(require 'rain)
;;(require 'he)
;;(require 'he-quick)
;; (require 'flypy)

;; pyim
;; 在windows下输入法都是正常的，就是linux比较恶心了。
(when sys/linuxp
(require 'pyim-init))


(provide 'init-im)
