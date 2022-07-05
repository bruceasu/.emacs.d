(provide 'init-calendar)
;;; 设置日历 ;;;;;;



;; Emacs Calender
;;
;; Emacs 中有日历，而且可以称之为一个系统，因为其中除了最常用的日历之外，
;; 还有其他的近十种历法，其中有日记、约会提醒、纪念日提示以及节假日提示等
;; 等。其中的历法包括中国的农历、希伯来历、伊斯兰历、法国革命历、中美玛雅
;; 历等等，可以根据经纬度告知你的所在的每天日出日落的时间等等。
;;
;; holiday-fixed m d    固定阳历节日， m 月 d 日
;; holiday-float m w n 浮动阳历节日， m 月的第 n 个星期 w%7
;;
;; ----------------------------------------------
;; .    跳回当前天
;; o    跳到某一个月
;; g d    跳到某年某月某日
;; g c    跳到某年某星期的星期几
;; g C    跳到阴历的某一天
;; p C    显示当前的阴历日期
;; h    显示当前节日
;; i d    加入当前这一天的日程安排
;; i w    加入每周这一天的日程安排
;; i m    加入每月这一天的日程安排
;; i y    加入每年这一天的日程安排
;; i a    加入周年纪念（anniversary），比如生日等
;; d    察看当前日期的diary
;; -----------------------------------------------
;;

(defun animals(birthyear)
  "Calculate the Chinese aninal by year"
  (let ((x (% (- 1997 birthyear) 12)))
    (cond ((or (= x 1) (= x -11))  "鼠")
          ((= x 0)                 "牛")
          ((or (= x 11) (= x -1))  "虎")
          ((or (= x 10) (= x -2))  "兔")
          ((or (= x 9) (= x -3))   "龙")
          ((or (= x 8) (= x -4))   "蛇")
          ((or (= x 7) (= x -5))   "马")
          ((or (= x 6) (= x -6))   "羊")
          ((or (= x 5) (= x -7))   "猴")
          ((or (= x 4) (= x -8))   "鸡")
          ((or (= x 3) (= x -9))   "狗")
          ((or (= x 2) (= x -10))  "猪")
          )
    )
  )

;; 保存日记的文件
;(setq diary-file "~/diary")
(setq diary-mail-addr "bruceasu@163.com")
(add-hook 'diary-hook 'appt-make-list)
;;appointment
(setq appt-issue-message t)



;;除去基督徒的节日、希伯来人的节日和伊斯兰教的节日。
(setq hebrew-holidays nil
      islamic-holidays nil
      solar-holidays nil
      bahai-holidays nil
	  ;; christian-holidays nil
      )

(setq general-holidays '((holiday-fixed 1 1   "元旦")
                         (holiday-fixed 2 14  "情人节")
                         (holiday-fixed 4 1   "愚人节")
                         (holiday-fixed 12 25 "圣诞节")
                         (holiday-fixed 10 1  "国庆节")
                         (holiday-float 5 0 2 "母亲节")   ;5月的第二个星期天
                         (holiday-float 6 0 3 "父亲节")
                         ))

(setq local-holidays '((holiday-chinese 1 15  "元宵节 (正月十五)")
                       (holiday-chinese 5 5   "端午节 (五月初五)")
                       (holiday-chinese 9 9   "重阳节 (九月初九)")
                       (holiday-chinese 8 15  "中秋节 (八月十五)")
                       ;; 生日
                       (holiday-chinese 7 16  "老豆生日 (七月十六)" );阴历生日
                       (holiday-chinese 1 10  "老母生日 (正月初十)" );阴历生日
                       (holiday-chinese 4 4  "老母生日 (四月初四)" );阴历生日
                       (holiday-chinese 10 18 "我嘅生日 (十月十八)" );阴历生日
                       (holiday-chinese 9 1   "細佬生日 (九月初一)") ;阴历生日
                       (holiday-chinese 3 2   "女儿生日 (三月初二)") ;阴历生日
                       (holiday-fixed   4 8   "女儿生日 (4月8日)")   ;阳历生日
                       (holiday-fixed   10 12  "女儿生日 (10月12日)") ;阳历生日
                       (holiday-chinese 9 14   "女儿生日 (九月十四)") ;阴历生日
                       (holiday-chinese 6 21  "老婆生日 (六月廿一)") ;阴历生日
                       ))

(setq chinese-calendar-celestial-stem
      ["甲" "乙" "丙" "丁" "戊" "己" "庚" "辛" "壬" "癸"])
(setq chinese-calendar-terrestrial-branch
      ["子" "丑" "寅" "卯" "辰" "巳" "午" "未" "申" "酉" "戌" "亥"])

(setq mark-diary-entries-in-calendar t
      appt-issue-message nil
      mark-holidays-in-calendar t
      view-calendar-holidays-initially nil)

(setq diary-date-forms '((year "/" month "/" day "[^/0-9]"))
      calendar-date-display-form '(year "/" month "/" day)
      calendar-time-display-form
      '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")")))

(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

(autoload 'chinese-year "cal-china" "Chinese year data" t)

(defun holiday-chinese (cmonth cday string)
  "Chinese calendar holiday, month and day in Chinese calendar (CMONTH, CDAY).

If corresponding MONTH and DAY in gregorian calendar is visible,
the value returned is the list \(((MONTH DAY year) STRING)).
Returns nil if it is not visible in the current calendar window."
  (let* ((m displayed-month)
     (y displayed-year)
     (gdate (calendar-gregorian-from-absolute
         (+ (cadr (assoc cmonth (chinese-year y))) (1- cday))))
     (gm (car gdate))
     (gd (cadr gdate))
     (gy (caddr gdate)))
    (increment-calendar-month m y (- 11 gm))
    (if (> m 9)
        (list (list (list gm gd gy) string)))))

(defun birthday-fixed (month day string)
  "Holiday on MONTH, DAY (Gregorian) called STRING.
If MONTH, DAY is visible, the value returned is the list (((MONTH DAY year)
STRING)).  Returns nil if it is not visible in the current calendar window."
  (let ((m displayed-month)
        (y displayed-year)
        (animal (animals (string-to-number (nth 1 (split-string string "(")))))
        )
    (increment-calendar-month m y (- 11 month))
    (if (> m 9)
      (list (list (list month day y) string animal)))))


;;设置我所在地方的经纬度，calendar里有个功能是日月食的预测，和你的经纬度相联系的。
;; 让emacs能计算日出日落的时间，在 calendar 上用 S 即可看到
(setq calendar-latitude +23.08)
(setq calendar-longitude +113.17)
(setq calendar-location-name "廣州")


;; 设置 calendar 的显示
(setq calendar-remove-frame-by-deleting t)
(setq calendar-week-start-day 1) ; 设置星期一为每周的第一天
(setq mark-diary-entries-in-calendar t) ; 标记calendar上有diary的日期
(setq mark-holidays-in-calendar nil) ; 为了突出有diary的日期，calendar上不标记节日
(setq view-calendar-holidays-initially nil) ; 打开calendar的时候不显示一堆节日


;;(global-set-key (kbd "<f11>") 'calendar)
