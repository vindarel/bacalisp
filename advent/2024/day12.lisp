(uiop:define-package :aoc-2024-12
  (:use :cl
   :ciel  ;; for the libraries: cl-str serapeum (dict) log4cl (log:info) trivial-do (dohash)
   ))

(in-package :aoc-2024-12)

;;
;; Part 1 OK, efficient.
;;
;; Part 2 not efficient and ugly with too many globals (hey, it works and allows easy introspection).
;; Also not efficient because I check too many times that a point is in the points of its region.
;; I should have stored the region ID in the grid cells.
;;
;; Part 2: we parse the grid, collect T when the point has a left/rihgt/…/… border, or NIL,
;; then we count the consecutive Ts.
;;

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

(defparameter *sides* (dict) "id -> integer")

#+to-refactor-with-this
(defun has-border-p (point move char)
  (let ((border-char (gethash (move-point point move) *grid*)))
    (cond
      ((null border-char)
       t)
      ((equal border-char char)
       nil)
      (t
       t))))

;; ;XXX: code repetition.

(defun left-border-p (point char)
  "This point has no neighbor of the same char at the left."
  (let ((left-char (gethash (move-point point '(-1 0)) *grid*)))
    (cond
      ((null left-char)
       t)
      ((equal left-char char)
       nil)
      (t
       t))))

(defun right-border-p (point char)
  "This point has no neighbor of the same char at the left."
  (let ((left-char (gethash (move-point point '(1 0)) *grid*)))
    (cond
      ((null left-char)
       t)
      ((equal left-char char)
       nil)
      (t
       t))))

(defun top-border-p (point char)
  (let ((top-char (gethash (move-point point '(0 -1)) *grid*)))
    (cond
      ((null top-char)
       t)
      ((equal top-char char)
       nil)
      (t
       t))))

(defun bottom-border-p (point char)
  (let ((top-char (gethash (move-point point '(0 1)) *grid*)))
    (cond
      ((null top-char)
       t)
      ((equal top-char char)
       nil)
      (t
       t))))

(defun collect-top-sides (char points grid regions dimensions &key (fn 'top-border-p))
  "Walk the grid from Y to X, collect consecutive Ts if the point is a border (as FN dictates)."
  (declare (ignorable regions))
  (loop for y from 0 to (second dimensions)
        nconc (loop for x from 0 to (first dimensions)
                    for point = (list x y)
                    for point-char = (gethash point grid)
                    if (and (find point points :test #'equal) ;; not efficient :S We should have stored the region id in the grid cells. Well, I have the result now. For next time!
                            (equal char point-char)
                            (funcall fn point char))
                      collect t
                    else
                      collect nil)))

(defun collect-bottom-sides (char points grid regions dimensions &key (fn 'bottom-border-p))
  (collect-top-sides char points grid regions dimensions :fn fn))

(defun count-consecutive-sides (list)
  (loop for elt in list
        with prev
        with res = 0
        if (and (not prev)
                elt)
          do (incf res)
        do (setf prev elt)
        finally (return res)))

(defun collect-left-sides (char points grid regions dimensions &key (fn 'left-border-p))
  "Walk the grid from X to Y, collect Ts."
  (declare (ignorable regions))
  (loop for x from 0 to (second dimensions)
        nconc (loop for y from 0 to (first dimensions)
                    for point = (list x y)
                    for point-char = (gethash point grid)
                    if (and (find point points :test #'equal) ;; not efficient :S
                            (equal char point-char)
                            (funcall fn point char))
                      collect t
                    else
                      collect nil)))

(defun collect-right-sides (char points grid regions dimensions &key (fn 'right-border-p))
  (collect-left-sides char points grid regions dimensions :fn fn))

(defun count-intermediary-sides (grid regions dimensions &aux region-char)
  (setf *sides* (dict))
  (dohash (region-id points regions)
    (setf region-char (gethash (first points) grid))
    (setf (gethash region-id *sides*)
          (list
           (count-consecutive-sides
            (collect-left-sides region-char points grid regions dimensions))
           (count-consecutive-sides
            (collect-right-sides region-char points grid regions dimensions))
           (count-consecutive-sides
            (collect-top-sides region-char points grid regions dimensions))
           (count-consecutive-sides
            (collect-bottom-sides region-char points grid regions dimensions)))))

  *sides*)

(defparameter *all-sides* (dict))

(defun count-all-sides (grid regions dimensions &aux (res (dict)))
  (let ((sides (count-intermediary-sides grid regions dimensions)))
    (dohash (region-id ints sides)
      (setf (gethash region-id res)
            (reduce #'+ ints))))
  (setf *all-sides* res))

(defun part-2 (input)
  (setf *regions* (dict)
        *areas* (dict)
        *visited* (dict)
        )
  (let* ((dimensions (dimensions input))
         (total 0))
    (walk (parse-input input) dimensions)

    ;; I thought about re-drawing the region, finally we use the list of the region' points.
    ;; (check-all-characters *regions*)

    (count-all-sides *grid* *regions* dimensions)

    (dohash (region-id points *regions*)
      (incf total (* (gethash region-id *all-sides*)
                     (gethash region-id *areas*))))
    total))


#++
(part-2 *input*)
;; 80

#++
(part-2 (str:from-file *file-input*))
;; 841934 o/
;; not efficient because of checking the points in lists (among others).

(defparameter *input-3* "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE")

#++
(part-2 *input-3*)
;; 236

(defparameter *input-Os-Xs* "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO")

#++
(assert (= 436 (part-2 *input-Os-Xs*)))


#+not-needed
(defun check-all-characters (regions &aux res seen)
  (dohash (id points regions)
    (let ((char (gethash (first points) *grid*))
          (other-chars (loop for code from 97 to 122
                                  collect (code-char code))))
      (when (find char seen)
        ;; (format t "seen ~s! " char)
        (let ((new (pop other-chars)))
          (loop for point in points
                do (setf (gethash point *grid*) new)))
        )
      (push char res)
      (push char seen)))
  res)

#+devel
(defun all-characters (regions &aux res seen)
  (dohash (id points regions)
    (let ((char (gethash (first points) *grid*)))
      (push char res)
      (push char seen)))
  res)
