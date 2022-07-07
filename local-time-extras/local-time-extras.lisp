
(in-package :local-time)
;; Yeah, we want these functions inside local-time proper,
;; we believe they belong there, but they won't be accepted with a PR :/ (is that bloat??)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extracted from existing functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Extracted from days-in-month
(defun leap-year-p (year)
  "Returns T if the given year is a leap year."
  (or (and (zerop (mod year 4))
           (plusp (mod year 100)))
      (zerop (mod year 400))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Easy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun yesterday ()
  "Returns a timestamp representing the day before today."
  (timestamp- (today) 1 :day))

(defun tomorrow ()
  "Returns a timestamp representing the day after today."
  (timestamp+ (today) 1 :day))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun weeks-in-year (year)
  "Returns the number of weeks in the given year."
  (if (or (= 4 (timestamp-day-of-week (encode-timestamp 0 0 0 0 1 1 year)))
          (= 4 (timestamp-day-of-week (encode-timestamp 0 0 0 0 31 12 year))))
      53
      52))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Export our new functions from local-time
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; not working :/
(export '(:leap-year-p
          :yesterday
          :tomorrow
          :weeks-in-year)
        :local-time)
