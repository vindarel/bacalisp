#|
Game Of Life kata.
|#

(defpackage #:game-of-life
  (:nicknames #:gol)
  (:use #:cl))

(in-package :game-of-life)

(defparameter *world* #2A((0 0 0)
                          (1 1 0)
                          (0 1 0)))
#+nil
(= 1 (aref *world* 2 1))

(defun cell-alive-p (state)
  (= 1 state))

(defun cell (world i j)
  (aref world i j))

(defun (setf cell) (val world i j)
  (setf (aref world i j)
        val))

(defun cell-lives-p (world i j)
  "Rules:

   Any live cell with fewer than two live neighbours dies, as if caused by under-population.
   Any live cell with two or three live neighbours lives on to the next generation.
   Any live cell with more than three live neighbours dies, as if by overcrowding.
   Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction."
  (let ((cell-alive (cell-alive-p (cell world i j)))
        (living-neighbours (living-neighbours world i j)))
    (cond
      ;; under-population.
      ((and cell-alive
            (< living-neighbours 2))
       nil)
      ;; rule 2: lives.
      ((and cell-alive
            (<= 2 living-neighbours 3))
       t)
      ;; overcrowding.
      ((and cell-alive
            (> living-neighbours 3))
       nil)
      ;; reproduction.
      ((and (not cell-alive)
            (= 3 living-neighbours))
       t)
      (t
       nil
       ))))

(defun test-cell-lives-p ()
  (let ((w1 #2A((0 0 1)
                (1 1 0)
                (0 1 0))))
    (assert (cell-lives-p w1 1 0)
            nil "An alive cell keeps living")

    (assert (not (cell-lives-p w1 0 2))
            nil "An alive cell dies.")

    (assert (cell-lives-p w1 2 0)
            nil "A dead cell is regenerated.")

    (assert (not (cell-lives-p w1 0 0))
            nil "Dead cell dies.")

    :OK-test-cell-lives-p))

(defun neighbours (world i j)
  (loop for coord in (list (cons (1- i) j)
                           (cons (1- i) (1- j))
                           (cons (1- i) (1+ j))
                           (cons i (1- j))
                           (cons i (1+ j))
                           (cons (1+ i) j)
                           (cons (1+ i) (1+ j))
                           (cons (1+ i) (1- j)))
     with dimension = (first (array-dimensions world))
     when (not (or (minusp (car coord))
                   (minusp (cdr coord))
                   (>= (car coord) dimension)
                   (>= (cdr coord) dimension)))
     collect (cell world (car coord) (cdr coord))))

(defun test-neighbours ()
  (let ((w1 #2A((0 0 0)
                (0 1 0)
                (0 0 0)))
        (w2 #2A((0 0 0)
                (0 1 0)
                (0 1 0)))
        (w3 #2A((0 0 0)
                (1 1 0)
                (0 1 0))))

    (assert (equal '(0 0 0 0 0 0 0 0)
                   (neighbours w1 1 1)))

    (assert (equal '(0 0 0 0 0 1 0 0)
                   (neighbours w2 1 1)))

    (assert (equal '(0 0 1)
                   (neighbours w1 0 0)))

    (assert (equal '(1 1 1)
                   (neighbours w3 2 0)))


    'OK-test-neighbours))

(defun living-neighbours (world i j)
  "Return the number of living neighbours."
  (let ((neighbours (neighbours world i j)))
    (length (remove-if-not #'cell-alive-p neighbours))))

(defun next-gen (world)
  (let* ((dimension (first (array-dimensions world)))
         (new-world (make-array (list dimension dimension) :initial-element 0)))
    (loop for i below dimension
       do (loop for j below dimension
             when (cell-lives-p world i j)
             do (setf (cell new-world i j)
                      1)))
    new-world))

(defun print-world (world)
  (let ((dimension (first (array-dimensions world))))
    (dotimes (i dimension)
      (dotimes (j dimension)
        (format t " ~a " (cell world i j)))
      (format t "~&")))
  ;; return the world object, so than we can call
  ;; (print-next-gen *)
  ;; repeteadly in the REPL.
  world)

(defun print-next-gen (world)
  (print-world (next-gen world)))

"

Some known forms
================

(defparameter *oscillator* #2A((0 0 0)
                               (1 1 1)
                               (0 0 0)))

(print-next-gen *oscillator*)

 0  1  0
 0  1  0
 0  1  0
#2A((0 1 0) (0 1 0) (0 1 0))

(print-next-gen *)
;; => back to normal.

 0  0  0
 1  1  1
 0  0  0
#2A((0 0 0) (1 1 1) (0 0 0))

A still form:

(defparameter tub #2A((0 1 0)
                      (1 0 1)
                      (0 1 0)))
 0  1  0
 1  0  1
 0  1  0

The toad:

(setf toad #2A((0 0 0 0)
               (0 1 1 1)
               (1 1 1 0)
               (0 0 0 0)))

 0  0  1  0
 1  0  0  1
 1  0  0  1
 0  1  0  0
#2A((0 0 1 0) (1 0 0 1) (1 0 0 1) (0 1 0 0))

and back to normal at the 2nd generation.

"
