(uiop:define-package :aoc-2024-14
  (:use :cl
   :ciel  ;; for the libraries: cl-str and the ^ lambda shortcut.
   :defclass-std  ;; the one on lisp-maintainers, to have define-print-object/std
   ))

(in-package :aoc-2024-14)

(defparameter *file-input* "input-day14.txt")

(defparameter *input* "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3")

;; (defparameter *dimensions* '(101 103))

(defparameter *dimensions* '(11 7))

;; Quick class definition.
;; Compared to a dict / hash-table, this gives us the accessors.
(defclass-std:class/std robot px py vx vy)

(defclass-std:define-print-object/std robot)  ;; in latest lisp-maintainers/defclass-std

(defun make-robot (px py vx vy)
  ;; It's possible that another defclass shortcut macro out there allows to shorten this.
  ;; But it's rarely needed.
  (make-instance 'robot :px px :py py :vx vx :vy vy))

(defun parse-input (input)
  (loop for line in (str:lines input)
        for nbs = (mapcar #'parse-integer (ppcre:all-matches-as-strings "-?\\d+" line)) ;; goddam - sign lol. Go too fast = debug forever.
        collect (apply #'make-robot nbs)))


(defun move (robot)
  (let ((x (+ (px robot) (vx robot)))
        (y (+ (py robot) (vy robot))))
    (when (minusp x)
      (incf x (first *dimensions*)))
    (when (minusp y)
      (incf y (second *dimensions*)))
    (when (>= x (first *dimensions*))
      (decf x (first *dimensions*)))
    (when (>= y (second *dimensions*))
      (decf y (second *dimensions*)))
    (setf (px robot) x
          (py robot) y))
  robot)

(defparameter *max-seconds* 100)

#++no-cycles!
(defun find-cycle (robot)
  (loop for i from 0
        with start = (list (px robot) (py robot))
        for pos = (list (px robot) (py robot))
        when (> i *max-seconds*)
          return -1
        when (and (not (zerop i)) (equal pos start))
          return i
        do (move robot)
           ;; (log:info "looking at pos ~s" pos)
           (incf i)))

(defun move-for (robot n &key show)
  (dotimes (i n)
    (move robot)
    (when show
      (print robot)
      (show (list robot))))
  robot)

(defun move-all-for (robots n)
  (mapcar (^ (r) (move-for r n)) robots)
  ;; also
  ;; (mapcar (serapeum:partial #'move-for n) robots)
  ;; when move-for takes N first and then ROBOT.
  ;; or alexandria:rcurry to keep the argument N last:
  ;; (mapcar (alexandria:rcurry #'move-for n) robots)
  ;; So partial is nice and we can add it to CIEL. Shall we add rcurry too then?
  robots)

(defun count-quadrants (robots)
  (let ((i (/ (1- (first *dimensions*)) 2))
        (j (/ (1- (second *dimensions*)) 2)))
    ;; (log:info i j)
    (loop for robot in robots
          with q1 = 0
          with q2 = 0
          with q3 = 0
          with q4 = 0
          do (cond
               ((and (< (px robot) i)
                     (< (py robot) j))
                (incf q1))
               ((and (> (px robot) i)
                     (< (py robot) j))
                (incf q2))
               ((and (< (px robot) i)
                     (> (py robot) j))
                (incf q3))
               ((and (> (px robot) i)
                     (> (py robot) j))
                (incf q4))
               (t
                ;;(log:info "no quadrant for ~a" robot)
                ))
          finally
             (print (list q1 q2 q3 q4))
             (return (* q1 q2 q3 q4)))))

(defun grid ()
  ;; dimensions reversed, to benefit from the default array output.
  (make-array (reverse *dimensions*) :initial-element 0))

(defun show (robots &aux (grid (grid)))
  (dolist (robot robots)
    (incf (aref grid
                ;; reversed
                (py robot)
                (px robot))))
  (format t "~&~s" grid))

;; Shows:
#|
#2A((1 0 1 2 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 1 1 0 1 1)
    (1 0 1 0 0 0 0 0 0 0 0)
    (0 0 0 0 0 0 0 0 0 1 0)
    (0 0 0 0 0 0 0 1 0 0 0))
|#

(defun part-1 (input &key (n 100))
  (let ((robots (parse-input input)))
    (move-all-for robots n)
    (show robots)
    (count-quadrants robots)))

#++
(part-1 *input*)

#++
(let ((*dimensions* '(101 103)))
  (part-1 (str:from-file *file-input*)))
;; 218433348 o/
