(defpackage :aoc-2024-08
  (:use :cl
   :ciel  ;; for the libraries: cl-str ppcre
   ))

(in-package :aoc-2024-08)

(defparameter *file-input* "input-day08.txt")

(defparameter *simple-input* "..........
..........
..........
....a.....
..........
.....a....
..........
..........
..........
..........")

(defvar *antennas* (dict))

(defparameter *antennas* (dict)
  "char -> list of points")

(defparameter *antinodes* (dict))

(defun cell (char pos)
  (dict :char char
        :pos pos))

(defun parse-input (input &aux (grid (dict)))
  (loop for line in (str:lines input)
        for y from 0
        do (loop for char across line
                 for x from 0
                 for point = (complex x y)
                 for cell = (cell char point)
                 unless (equal #\. char)
                   do (pushnew cell (gethash char grid (list)))))
  (setf *antennas* grid)
  grid)

(defun permutations (char &key (antennas *antennas*) (input *input*) dimensions)
  (let ((antennas (gethash char antennas))
        (max-x (or (first dimensions) (length (first (str:lines input)))))
        (max-y (or (second dimensions) (length (str:lines input)))))
    (alexandria:map-permutations
     (^ (it)
        (let* ((dx (- (realpart (gethash :pos (first it)))
                      (realpart (gethash :pos (second it)))))
               (dy (- (imagpart (gethash :pos (first it)))
                      (imagpart (gethash :pos (second it)))))
               (x (realpart (gethash :pos (first it))))
               (y (imagpart (gethash :pos (first it))))
               (new-x (+ x dx))
               (new-y (+ y dy))
               (point (complex new-x new-y)))
          (when (and (not (minusp new-x))
                     (not (minusp new-y))
                     (< new-x max-x)
                     (< new-y max-y))
            (setf (gethash point *antinodes*)
                      (cell char point)))
          ))
     antennas
     :length 2
    )
    *antinodes*))

(defun count-antinodes (input)
  ;; XXX: don't use globals. Hey, it's easy. Also easy to forget one argument to PERMUTATIONS.
  (setf *antinodes* (dict))
  (setf *antennas* (dict))
  (let* ((antennas (parse-input input))
         (lines (str:lines input))
         (dimensions (list (length (first lines))
                           (length lines))))
    (dohash (k v antennas)
      (permutations k :antennas antennas :dimensions dimensions))
    (hash-table-count *antinodes*)))

(defparameter *input-2* "..........
..........
..........
....a.....
........a.
.....a....
..........
..........
..........
..........")

(defparameter *input* "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............")

#++
(count-antinodes *input*)
;; 14

#++
(count-antinodes (str:from-file *file-input*))
;; 214 o/
