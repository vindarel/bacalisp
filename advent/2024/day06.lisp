(defpackage :aoc-2024-06
  (:use :cl
        ;; :defclass-std
   :ciel))

(in-package :aoc-2024-06)

(defparameter *file-input* "input-day06.txt")

(defparameter *input* "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...")

(defparameter *grid* (dict))

(defun cell (char &key guard visited)
  (dict :char char
        :guard guard
        :visited visited))

(defvar *guard* nil)

(defun parse-grid (input)
  (setf *grid* (dict))
  (loop for line in (str:lines input)
        for y from 0  ;; up and down, imagpart
        with grid = (dict)
        do (loop for char across line
                 for x from 0   ;; left to right, realpart
                 for key = (complex x y)
                 for cell = (cell char)
                 if (equal char #\^)
                   do (progn (setf (gethash :guard cell) t)
                             (setf *guard* key))
                 do (setf (gethash key *grid*) cell)))
  *grid*)

(defun is-block (cell)
  ;; accept a NIL, we'll stop the walk in the next iteration.
  (when cell
    (equal (gethash :char cell) #\#)))

(defun next-x (position direction &aux next-x)
  (case direction
    (:up (realpart position))
    (:down (realpart position))
    (:right (1+ (realpart position)))
    (:left (1- (realpart position)))))

(defun next-y (position direction)
  (case direction
    (:up (1- (imagpart position)))
    (:down (1+ (imagpart position)))
    (:right (imagpart position))
    (:left (imagpart position))))

(defun walk (&key (grid *grid*) (input *input*)
               (position *guard*)
               (cell (gethash *guard* *grid*))
               (direction :up)
               (count 0)
             &aux next-cell
               next-position obstacle-coming)

  (unless cell
    (return-from walk count))

  ;; look in the same direction.
  (setf next-position
        (complex (next-x position direction) (next-y position direction)))

  (setf next-cell (gethash next-position grid))

  ;; obstacle?
  (setf obstacle-coming (is-block next-cell))

  ;; then change direction.
  (when obstacle-coming
    (setf direction
          (case direction
            (:up :right)
            (:down :left)
            (:right :down)
            (:left :up))))

  (unless (gethash :visited cell)
    (incf count)
    (setf (gethash :visited cell) t))

  ;; get our next position now.
  (setf next-position
        (complex (next-x position direction) (next-y position direction)))

  (setf next-cell (gethash next-position grid))

  (log:debug "~a:~a " direction next-position)

  (walk :grid grid :input input
        :cell next-cell
        :position next-position
        :direction direction
        :count count))

(defun part-1 (input)
  (walk :grid (parse-grid input)))

#++
(part-1 *input*)

#++
(part-1 (str:from-file *file-input*))
;; 4758 o/

;; part 2: no brute force today. I'll look for smarter solutions.
