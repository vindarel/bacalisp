(defpackage :aoc-2024-10
  (:use :cl
   :ciel  ;; for the libraries: cl-str serapeum (dict)
   ))

(in-package :aoc-2024-10)

(defparameter *file-input* "input-day10.txt")

(defparameter *input* "0123
1234
8765
9876")

(defun parse-input (input)
  (loop for line in (str:lines input)
        with grid = (dict)
        with starts = (list)
        for x from 0
        do (loop for char across line
                 for y from 0
                 for point = (complex x y)
                 for nb = (or
                           (ignore-errors (parse-integer (string char)))
                           -1)
                 if (zerop nb) do (push point starts)
                 do (setf (gethash point grid) nb))
        finally (return (list grid (reverse starts)))))

(defparameter *moves* '(
                        (0 1)
                        (1 0)
                        (-1 0)
                        (0 -1)))

(defun move-to (point move)
  (+ point (complex (first move) (second move))))

(defun valid-point-p (point dimensions)
  (and (not (minusp (realpart point)))
       (< (realpart point) (first dimensions))
       (not (minusp (imagpart point)))
       (< (imagpart point) (second dimensions))))

(defun incremental-step-p (point next grid)
  (= 1  (- (gethash next grid)
           (gethash point grid))))

(defparameter *input-2* "...0...
...1...
...2...
6543456
7.....7
8.....8
9.....9")

(defun next-step (grid point &key dimensions (visited (dict)) path trails
                               is-part-2)
  (unless is-part-2
    (when (gethash point visited)
      (return-from next-step)))
  (setf (gethash point visited) t)
  (push point path)
  (when (= 9 (gethash point grid))
    (log:info point (gethash point grid))
    (push (reverse path) trails)
    (return-from next-step (reverse trails)))
  (log:info point (gethash point grid))
  (loop for move in *moves*
        for next = (move-to point move)
        if (and (valid-point-p next dimensions)
                (incremental-step-p point next grid))
          ;; nconc allows to return a list of lists without many parens nesting.
          nconc (next-step grid next :visited visited :dimensions dimensions :path path
                           :is-part-2 is-part-2)
        ))

(defun find-trails (input &key is-part-2)
  (let* ((grid/starts (parse-input input))
         (grid (first grid/starts))
         (starts (second grid/starts))
         (lines (str:lines input))
         (dimensions (list (length (first lines))
                           (length lines))))
    (loop for start in starts
          for _ = (log:info "starting from ~s" start)
      nconc (next-step grid start :dimensions dimensions :is-part-2 is-part-2))))

(defun count-trailheads (input &key is-part-2)
  (length (find-trails input :is-part-2 is-part-2)))

(defparameter *input-3* "..90..9
...1.98
...2..7
6543456
765.987
876....
987....")
#++
(count-trailheads *INPUT-3*)
;; 4

(defparameter *input-4* "10..9..
2...8..
3...7..
4567654
...8..3
...9..2
.....01")

(defparameter *input-5* "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732")
#++
(count-trailheads *input-5*)
;; 36

#++
(count-trailheads (str:from-file *file-input*))
;; 733 o/

#++
(count-trailheads (str:from-file *file-input*) :is-part-2 t)
;; 1514 o/
