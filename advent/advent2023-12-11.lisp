
(uiop:define-package :aoc-2023-11
  (:use :cl :ciel))

(in-package :aoc-2023-11)

(defparameter *file-input* "advent2023-12-11.txt")

(defparameter input "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....")

(defvar grid (list))

(defun parse-input (input)
  (str:lines input))

(defun lines-without-galaxies (lines)
  (loop for line in lines
        for i from 0
        if (every (lambda (char) (char= #\. char)) line)
          collect i))
;; (lines-without-galaxies (parse-input input))

(defun inverse-grid (lines)
  (loop for j from 0 below (length (first lines))
        collect (loop for line in lines
                      collect (elt line j))))

(defun columns-without-galaxies (lines)
  ;; inverse a matrix?
  (lines-without-galaxies (inverse-grid lines)))

(defun expand-grid (lines)
  (let ((expand-lines (lines-without-galaxies lines))
        (expand-cols (columns-without-galaxies lines)))
    (loop for line in lines
          for j from 0
          for newline = (make-string (+ (length line)
                                        (* 1 (length expand-cols)))
                                     :initial-element #\.)
          if (find j expand-lines)
            collect newline
            and
              collect newline
          else
            collect
          (loop for char across line
                for col from 0
                if (find col expand-cols)
                  collect #\. into res
                  and collect #\. into res
                else
                  collect char into res

                finally (return (concatenate 'string res))))))

#+(or)
(showgrid (expand-grid (parse-input input)))

(defun showgrid (lines)
  (str:join #\Newline lines))

(defparameter galaxies (list)
  "List of dicts made of: coordinates (x, y) -> unique number")

(defun collect-galaxies (lines)
  (loop for j from 0
        for line in lines
        with nb = 0
        do (loop for char across line
                 for i from 0
                 if (char= #\# char)
                   do (push (dict :coord (cons i j)
                                  :id nb)
                            galaxies)
                      (incf nb)
                      (log:info nb)))
  galaxies)

;; (defun nb-pairs (galaxies)
;;   ;; mmh… closure or save the nb somewhere or max of values?
;;   ;; 8^2 / 2
;;   (/ (expt (1- (length (hash-table-values galaxies))) 2)
;;      2))

(defun make-pairs (x galaxies)
  (loop for y in galaxies
        ;; collect (list (gethash :id x) (gethash :id y))))
        collect (list x y)))

(defvar pairs nil)

(defun all-pairs (galaxies)
  ;; The trick to get the head, the rest of the list:
  (loop for (x . more) on (reverse galaxies)  ;; we used push.
        ;; nconc: like collect but return a flat list (see CL Cookbook).
        nconc (make-pairs x more)))

(defun coord-i (ht)
  (car (gethash :coord ht)))

(defun coord-j (ht)
  (cdr (gethash :coord ht)))

(defun count-steps(pair)
  ;; easy, it's a coordinate substraction, since we don't have obstacles.
  ;; We are not looking for the path itself. I fear part2…
  (let ((a (first pair))
        (b (second pair)))

    (+ (abs (- (coord-i b)
               (coord-i a)))
       (abs (- (coord-j b)
               (coord-j a))))))

(defun count-all-steps (pairs)
  (reduce #'+ (mapcar #'count-steps pairs)))

(defun part1 (input)
  ;; devel: be sure to reset galaxies ;)
  (-> input
    parse-input
    expand-grid
    collect-galaxies
    all-pairs
    count-all-steps
    ))

#+solve-it
(part1 (str:from-file *file-input*))
;; 9605127 \o/
