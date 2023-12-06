;; with libraries:
;; ciel-user
;; or
;; ppcre, str, arrow-macros

(defparameter *file-input* "advent2023-12-06.txt")

(defparameter input "Time:      7  15   30
Distance:  9  40  200")

(defun extract-line-numbers (line)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" line)))

(defun parse-time-distances (input)
  (mapcar #'extract-line-numbers (str:lines input)))

(defun find-min/max-time (min/max &key race-time target)
  (loop for stepper = (if (eql min/max :min) #'1+ #'1-)
        for start = (if (eql min/max :min) 1 race-time)
        for holding = start then (funcall stepper holding)
        for speed = holding
        for time-moving = (- race-time holding)
        for distance = (* speed time-moving)
        if (> distance target)
          return holding))

#+(or)
(find-min/max-time :max :race-time 7 :target 9)
;; 5

(defun nb-ways-to-win/attempt1 (race-time target)
  (- (1+ (find-min/max-time :max :race-time race-time :target target))
     (find-min/max-time :min :race-time race-time :target target)))

(defun nb-ways-to-win (race-time target)
  ;; = min and max are symetric.
  (let ((min (find-min/max-time :min :race-time race-time :target target)))
    (1+ (- race-time (* 2 min)))))

;; the real solution is the quadratic equation.

#+(or)
(nb-ways-to-win 30 200)
;; 9

(defun part1 (input)
  (let ((maps (parse-time-distances input)))
    (apply #'* (mapcar #'nb-ways-to-win
                       (first maps)
                       (second maps)))))

#+(or)
(part1 input)
;; 288

#+solve-it
(part1 (str:from-file *file-input*))
;; 1195150 \o/
;;
;; with symetric way, half iterations less:
;; Evaluation took:
;;  0.051 seconds of real time


;; part 2
(defun assemble-number (line)
  (->> line
    (str:split ":")
    (second)
    (str:replace-all " " "")
    (parse-integer)))

(defun parse-time-distances/part2 (input)
  (mapcar #'assemble-number (str:lines input)))

(defun part2 (input)
  (let ((time/distance (parse-time-distances/part2 input)))
    (* (nb-ways-to-win (first time/distance) (second time/distance)))))

#+(or)
(part2 input)
;; 71503

#+solve-it
(part2 (str:from-file *file-input*))
;; 42550411
;; \o/
;; Evaluation took:
;; 0.111 seconds of real time

;; Feedback:
;; > "Basically the quadratic formula."
;; !
