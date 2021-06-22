
#|
For the current directory and subdirectories, "graph" the number of files created each day.

(graph-day-occurences (find-files))
min is 20210220
max is 20210225
range is 6 (days maybe).
20210220: x3 events
20210222: x4 events
20210223: x6 events
20210224: x2 events
20210225: x1 events
"
0                       3                        6
˫-----------------------+------------------------˧
█████████████████████████▏
▏
█████████████████████████████████▍
██████████████████████████████████████████████████
████████████████▋
████████▍
"

TODO: compute a proper date range.

|#

(defun find-files ()
  (fof:finder* :recur-predicates (list (complement #'fof/p:hidden?))))

(defun get-creation-dates (files)
  (loop for f in files
     collect (fof:creation-date f)))

(defun date-to-int (d)
  (parse-integer (local-time:format-timestring nil d :format '(:year (:month 2) (:day 2)))))

(defun get-dates-as-integers (files)
  (sort (mapcar #'date-to-int (get-creation-dates files)) #'<))

(defun min-value (ints)
  (loop for val in ints
     minimizing val))

(defun max-value (ints)
  (loop for val in ints
       maximizing val))

(defun group-dates (files)
  (let ((integer-dates (get-dates-as-integers files)))
    (format t "min is ~a~&" (min-value integer-dates))
    (format t "max is ~a~&" (max-value integer-dates))
    (format t "range is ~a (days maybe).~&" (1+ (- (max-value integer-dates)
                                           (min-value integer-dates))))
    (loop for nb in (sort (copy-seq integer-dates) #'<)
       with seen = '()
       unless (member nb seen)
       do (format t "~a: x~a events~&" nb (count nb integer-dates))
         (push nb seen)
       and collect (list nb (count nb integer-dates)))))
;; returns:
;; ((2021220 3) (2021222 4) (2021223 6) (2021224 2) (2021225 1))

(defun fill-in-missing (pairs min max)
  (assert (< (length pairs)
             1000)
          nil
          "You are probably computing a bad range between years. This doesn't work.")
  (loop for val from min to max by 1
     with result = '()
     for pair = (find val pairs :key #'car)
     if (member val pairs :key #'car )
     do (push (cons val (cdr pair)) result)
     else do
     ;; (format t "missing ~a~&" val)
       (push (list val 0) result)
     finally
       ;; (print (reverse result))
       (return (reverse result))))

(defun graph-day-occurences (files)
  (let* ((integer-dates (get-dates-as-integers files))
         (min (min-value integer-dates))
         (max (max-value integer-dates))
         (nb/occurence (group-dates files))
         (nb/occurence-full (fill-in-missing nb/occurence min max)))
    (cl-spark:vspark nb/occurence-full :key #'second)))
