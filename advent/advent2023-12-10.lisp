
(uiop:define-package :aoc-2023-10
  (:use :cl :ciel))

(in-package :aoc-2023-10)

;;;
;;; I follow the tiles one by one, like if doing it manually.
;;; Not sure this helps me for part 2!
;;;

(defparameter *file-input* "advent2023-12-10.txt")

(defparameter input "-L|F7
7S-7|
L|7||
-L-J|
L|-JF")

(defvar grid nil)

(defun parse-line (line)
  (coerce (ppcre:all-matches-as-strings "." line) 'vector))

(defun parse-input (input)
  ;; still using lists instead of arrays.
  (setf grid (str:lines input)))

(defparameter *start* (list 0 0)) ;; i: inline position, j: vertical line

(defun find-start (grid)
  (setf *start*
    (loop for line in grid
        for j from 0
        for pos = (position #\S line)
        if pos
          return (cons pos j))))

(defun grid-elt (i j)
  (elt (elt grid j) i))

(defun west-elt (point)
  (grid-elt (1- (car point))
            (cdr point)))

(defun west-pos (point)
  (cons (1- (car point))
        (cdr point)))

(defun east-elt (point)
  (grid-elt (1+ (car point))
            (cdr point)))

(defun east-pos (point)
  (cons (1+ (car point))
        (cdr point)))

(defun north-elt (point)
  (grid-elt (car point)
            (1- (cdr point))))

(defun north-pos (point)
  (cons (car point)
        (1- (cdr point))))

(defun south-elt (point)
  (grid-elt (car point)
            (1+ (cdr point))))

(defun south-pos (point)
  (cons (car point)
        (1+ (cdr point))))

(defun first-connected-pipe (start)
  ;; finally unused.
  (cond
    ((equal #\- (west-elt start))
     (west-pos start))
    ((equal #\7 (west-elt start))
     (west-pos start))
    (t
     (error "todo: find other start neighbors"))))

(defparameter pos '(2 . 1)) ;; - not S               ;; current point
(defparameter direction :east)

(defun point-char (point)
  (elt (elt grid (cdr point)) (car point)))

(defun move-pos (point)
  (setf pos
  (case (point-char point)
    (#\-
     (case direction
       (:east (east-pos point))
       (:west (west-pos point))
       (t (error "wrong direction with -, coord: ~a" point))))
    (#\|
     (case direction
       (:north (north-pos point))
       (:south (south-pos point))
       (t (error "wrong direction: | and ~a on ~a" direction point))))
    (#\L
     (case direction
       (:west
        (setf direction :north)
        (north-pos point))
       (:south
        (setf direction :east)
        (east-pos point))
       (t (error "wrong direction: L and ~a on ~a" direction point))))
    (#\J
     (case direction
       (:east
        (setf direction :north)
        (north-pos point))
       (:south
        (setf direction :west)
        (west-pos point))
       (t (error "wrong direction: J and ~a on ~a" direction point))))
    (#\F
     (case direction
       (:west
        (setf direction :south)
        (south-pos point))
       (:north
        (setf direction :east)
        (east-pos point))
       (t (error "wrong direction: F and ~a on ~a" direction point))))
    (#\7
     (case direction
       (:north
        (setf direction :west)
        (west-pos point))
       (:east
        (setf direction :south)
        (south-pos point))
       (t (error "wrong direction: 7 and ~a on ~a" direction point))))
    (t
     (error "should ignore this: ~a on ~s" (point-char point) point)))
  )
  (values pos
          (point-char pos)
          direction))

(defun gomaze ()
  (print pos)
  (loop
    with steps = 1
    until (equal (point-char pos) #\S)
    do
       (multiple-value-bind (p c d) (move-pos pos)
         (incf steps)
         (format t "~a ~a ~a~&" p c d))
    finally (return steps)))

#+(or)
(progn
  (find-start (parse-input (str:from-file *file-input*)))
  (setf direction :east
        ;; take a connected point:
        pos '(40 . 50))
  (gomaze))
;; 14290
;; / 2 => 7145 o/
