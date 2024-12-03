(uiop:define-package :aoc-2024-02
    (:use :cl
     :ciel  ;; for libraries: str
          ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
          ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
          ;; See below.)
     ))

(in-package :aoc-2024-02)

(defparameter *input* "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defparameter *file-input* "input-day02.txt")

(defun safe-row (row)
  ;; see non-loop function below!
  (when (stringp row)
    (setf row (mapcar #'parse-integer (str:words row))))
  (loop :for rest :on row  ;; "on" advances on the list one by one.
        for cur = (car rest)
        for next = (or (cadr rest)
                       (return t))
        with order = nil
        for current-order = (cond
                              ((< cur next) :ascending)
                              ((> cur next) :descending)
                              (t
                               (return nil)))
        for step = (abs (- next cur))
        if (not (<= 1 step 3))
          do (return nil)
        if (not order)
          do (setf order current-order)
        if (not (equal order current-order))
          do (return nil)
        finally (return t)))

#++
(progn
  (safe-row "7 6 4 2 1")

  (safe-row "1 2 7 8 9")
  ;; NIL

  (safe-row "9 7 6 2 1")
  ;; NIL
  )

(defun part1 (input)
  (loop for line in (str:lines input)
    count (safe-row line)))

#+ciel
(part1 *input*)
;; 2

#+ciel
(format t "solution day2: ~a~&"(part1 (str:from-file *file-input*)))
;; 670
;; o/

(defun cut-row (row i)
  (concatenate 'list
               (subseq row 0 i)
               (subseq row (1+ i))))

(defun maybe-safe-row (row &key (recur t))
  (when (stringp row)
    (setf row (mapcar #'parse-integer (str:words row))))
  (loop :for rest :on row  ;; "on" advances on the list one by one.
        for cur = (car rest)
        for next = (or (cadr rest)
                       (return t))
        for i = 0 then (incf i)
        with order = nil
        for current-order = (cond
                              ((< cur next) :ascending)
                              ((> cur next) :descending)
                              (t
                               ;; try once again.
                               (if recur
                                   (return (maybe-safe-row (cut-row row i) :recur nil))
                                   (return nil))))
        for step = (abs (- next cur))
        if (not (<= 1 step 3))
          do (if recur
                 (return (or
                          (maybe-safe-row (cut-row row (max (1+ i) 0)) :recur nil)
                          (maybe-safe-row (cut-row row (max (1- i) 0)) :recur nil)
                          (maybe-safe-row (cut-row row i) :recur nil)
                          ))
                 (return nil))
        if (not order)
          do (setf order current-order)
        if (not (equal order current-order))
          do
             (if recur
                 (return (or
                          (maybe-safe-row (cut-row row (max 0 (1+ i))) :recur nil)
                          (maybe-safe-row (cut-row row (max 0 (1- i))) :recur nil)
                          (maybe-safe-row (cut-row row i) :recur nil)
                          ))
                 (return nil))
        finally (return t)))

#++
(progn
  ;; still safe
  (assert (maybe-safe-row "7 6 4 2 1"))
  (assert (maybe-safe-row "8 6 4 4 1"))
  ;; T

  (maybe-safe-row "1 3 2 4 5")
  ;; T

  (maybe-safe-row "1 6 5 4")
  )

;; still unsafe
#++
(progn
  (maybe-safe-row "9 7 6 2 1")
  ;; NIL

  (maybe-safe-row "1 2 7 8 9")
  ;; NIL
  )

(defun part2 (input)
  (loop for line in (str:lines input)
    count (maybe-safe-row line)))

#++
(part2 *input*)
;; 4

#++
(part2 (str:from-file *file-input*))
;; 700 o/

#+ciel
(format t "solution day2-part2: ~a~&"(part2 (str:from-file *file-input*)))

;;;
;;; More.
;;;
;;; Reading at others' solutions.
;;;

;; after reading a functional Clojure solution
;; https://www.reddit.com/r/adventofcode/comments/1h4ncyr/2024_day_2_solutions/m03ajiy/
(defun functional-safe-row (row)
  (when (stringp row)
    (setf row (mapcar #'parse-integer (str:words row))))
  (let ((ascendent (apply #'< row))
        (descendent (apply #'> row))
        (correct-deltas (every (^ (delta) (<= (abs delta) 3))
                               (rest (serapeum:deltas row)))))
    (and correct-deltas (or ascendent descendent))))

(defun functional-part-1 (input)
  (->> input
    (str:lines)
    (remove-if-not 'functional-safe-row)
    (length)))

;; usage and test:
#++
(functional-safe-row '(1 2 7 8 9))
;; NIL
