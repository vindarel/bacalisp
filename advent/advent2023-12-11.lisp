
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

(defvar *grid* (list))

(defun parse-input (input)
  (setf *grid*
        (str:lines input)))

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
  ;; I actually expand it… even though part 2 clearly makes one think of another strategy.
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
                      (log:debug nb)
                      (incf nb)))
  galaxies)

(defun make-pairs (x galaxies)
  (loop for y in galaxies
        collect (list x y)))

(defparameter pairs nil)

(defun all-pairs (galaxies)
  (setf pairs
  ;; The trick to get the head, the rest of the list:
  (loop for (x . more) on (reverse galaxies)  ;; we used push.
        ;; nconc: like collect but return a flat list (see CL Cookbook).
        nconc (make-pairs x more))))

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

;; part 2

(defparameter *adder* 1000000 "Age of the galaxies.")

(defun collect-list (x y)
  ;; XXX: I'd rather work with ranges!
  (loop for i from (min x y)
          below (max x y)
        collect i))

(defun count-steps-with-expansion (pair &key (grid *grid*) (adder *adder*))
  (let ((a (first pair))
        (b (second pair))
        (expand-lines (lines-without-galaxies grid))
        (expand-cols (columns-without-galaxies grid))
        (steps 0))

    (setf steps
          (+ (abs (- (coord-i b)
                     (coord-i a)))
             (abs (- (coord-j b)
                     (coord-j a)))))

    (log:debug "steps normal: ~a" (count-steps pair))

    (when-let ((list-to-expand (intersection expand-cols
                                             (collect-list (coord-i a) (coord-i b)))))
      (log:debug list-to-expand)
      (log:debug "adding " (* adder (length list-to-expand)))
      (incf steps (* adder (length list-to-expand)))
      (log:debug "removing " (length list-to-expand))
      (decf steps (length list-to-expand))
      )

    (when-let ((cols-to-expand (intersection expand-lines
                                             (collect-list (coord-j a) (coord-j b)))))
      (log:debug expand-cols cols-to-expand)
      (log:debug "and adding" (* adder (length cols-to-expand)))
      (incf steps (* adder (length cols-to-expand)))
      (log:debug "and removing " (length cols-to-expand))
      (decf steps (length cols-to-expand))
      )

    steps))

(defun get-galaxy (id)
  (find id galaxies :from-end t :key (lambda (it) (gethash :id it))))

(defun steps-between-with-expansion (id id2)
  (let ((a (find id galaxies :from-end t :key (lambda (it) (gethash :id it))))
        (b (find id2 galaxies :from-end t :key (lambda (it) (gethash :id it)))))
    (count-steps-with-expansion (list a b))))
;; (steps-between-with-expansion 4 8) ;; which is 5 & 9


(defun count-all-steps-with-expansion (pairs &optional (grid *grid*))
  (reduce #'+ (mapcar #'count-steps-with-expansion pairs)))

(defun part2 (input)
  (-> input
    parse-input
    ;; expand-grid
    collect-galaxies
    all-pairs
    count-all-steps-with-expansion
    ))

;; age 10 => 1030
;; age 100 => OK

#+ciel
(part2 (str:from-file *file-input*))
;; not OK :(
