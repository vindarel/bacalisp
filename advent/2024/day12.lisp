(uiop:define-package :aoc-2024-12
  (:use :cl
   :ciel  ;; for the libraries: cl-str serapeum (dict) log4cl (log:info) trivial-do (dohash)
   ))

(in-package :aoc-2024-12)

(defparameter *file-input* "input-day12.txt")

(defparameter *input* "AAAA
BBCD
BBCC
EEEC")

(defvar *grid* (dict) "point -> char")

(defparameter *moves* '((0 1)
                        (1 0)
                        (0 -1)
                        (-1 0)))

(defparameter *regions* (dict) "region id -> list of points")
(defparameter *visited* (dict) "point -> T")
(defparameter *perimeters* (dict) "region id -> integer")
(defparameter *areas* (dict) "region id -> integer")


(defun parse-input (input)
  (loop for line in (str:lines input)
        for y from 0
        with grid = (dict)
        do (loop for char across line
                 for x from 0
                 for point = (list x y)
                 do (setf (gethash point grid) char))
        finally (return (setf *grid* grid))))

(defun dimensions (input)
  (let ((lines (str:lines input)))
    (list (1- (length (first lines)))
          (1- (length lines)))))

(defun move-point (point move)
  (list (+ (x point) (first move))
        (+ (y point) (second move))))

(defun nb-neighbors (point char)
  (let ((neighbors (loop for move in *moves* collect (move-point point move))))
    (loop for neighbor in neighbors
          count (same-region-p neighbor char))))

(defun next-region-id ()
  (hash-table-count *regions*))

(defun x (point)
  (first point))
(defun y (point)
  (second point))

(defun visited (point)
  (gethash point *visited*))

(defun same-region-p (point char)
  (let ((point-char (gethash point *grid*)))
    (when point-char
      (equal char point-char))))

(defun explore (point char region-id)
  (when (visited point)
    (return-from explore))

  (setf (gethash point *visited*) t)
  (push point (gethash region-id *regions*))

  ;; Count perimeter.
  (incf (gethash region-id *perimeters* 0) (- 4 (nb-neighbors point char)))

  ;; Increment the area.
  (incf (gethash region-id *areas* 0))

  ;; (log:info "exploring ~s" point)
  (let ((up (list (x point) (1- (y point))))
        (down (list (x point) (1+ (y point))))
        (left (list (1- (x point)) (y point)))
        (right (list (1+ (x point)) (y point))))
    (when (same-region-p up char)
      (explore up char region-id))
    (when (same-region-p down char)
      (explore down char region-id))
    (when (same-region-p right char)
      (explore right char region-id))
    (when (same-region-p left char)
       (explore left char region-id))))

(defun walk (grid dimensions)
  (loop for x from 0 to (first dimensions)
        do (loop for y from 0 to (second dimensions)
                 for point = (list x y)
                 for char = (gethash point grid)
                 unless (visited point)
                   ;; we are exploring a new region.
                   do (explore point char (next-region-id)))))

(defun part-1 (input)
  (let ((grid (parse-input input))
        (dimensions (dimensions input))
        (total-price 0))

    ;; top-level regions, areas, perimeters… allright.
    (setf *regions* (dict)
          *areas* (dict)
          *visited* (dict)
          *perimeters* (dict))
    (walk grid dimensions)

    (dohash (region-id points *regions*)
      (incf total-price (* (gethash region-id *areas*)
                           (gethash region-id *perimeters*))))
    total-price))

(defparameter *INPUT-2* "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE")

#++
(log:config :debug)

#++
(part-1 (str:from-file *file-input*))
;; 1477924 o/
