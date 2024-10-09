;;; init-im -- 输入法
;;; summary:
;; 输入法
;; code:
(eval-when-compile
  (require 'init-package)
  )
  
;; 内置机制
;;(require 'zyoy)
;;(require 'rain)
;;(require 'he)
;;(require 'he-quick)
;; (require 'flypy)

;; pyim
;; 在windows下输入法都是正常的，就是linux比较恶心了。
(when sys/linuxp
	;; input method
	;;(require-package 'pyim)
	;;(require-package 'pyim-wbdict) ; someone may use wubi IME, not me
	;;(require-package 'pyim-basedict)

  (require 'pyim-init))


(provide 'init-im)
