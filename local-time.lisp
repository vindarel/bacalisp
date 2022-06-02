;; local-time simple additions that won't be merged upstream:
;; https://github.com/dlowe-net/local-time/pull/106/files

(defun yesterday ()
  "Returns a timestamp representing the day before today."
  (timestamp- (today) 1 :day))

(defun tomorrow ()
  "Returns a timestamp representing the day after today."
  (timestamp+ (today) 1 :day))

(defun leap-year-p (year)
  "Returns T if the given year is a leap year."
  (or (and (zerop (mod year 4))
           (plusp (mod year 100)))
      (zerop (mod year 400))))

(defun weeks-in-year (year)
  "Returns the number of weeks in the given year."
  (if (or (= 4 (timestamp-day-of-week (encode-timestamp 0 0 0 0 1 1 year)))
          (= 4 (timestamp-day-of-week (encode-timestamp 0 0 0 0 31 12 year))))
      53
      52))

;; xxx: merge into timestamp-difference ?
(defun timestamp-days-difference (time-a time-b)
  "Returns the number of days fully elapsed between TIME-A and TIME-B in full.
The second returned value represents the number of seconds remaining in the last day.

(timestamp-difference-days (today) (yesterday))
1
0

(let ((now (now)))
  (timestamp-difference-days now (timestamp- now 25 :hour)))
1
3600
"
  (let ((seconds (timestamp-difference time-a time-b)))
    (floor seconds +seconds-per-day+)))
