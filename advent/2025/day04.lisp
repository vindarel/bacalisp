(uiop:define-package :aoc-2025-04
    (:use :cl
     :ciel  ;; for libraries: str, serapeum:dict and more useful ones.
          ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
          ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
          ;; See below.)
          ;; http://ciel-lang.org/
          )
  (:documentation "AOC experience helps! We use complex numbers to represent grid positions, as learned from someone last year, as given as a tip here:

   https://lisp-journey.gitlab.io/blog/practice-for-advent-of-code-in-common-lisp/

"))

(in-package :aoc-2025-04)

(defparameter *input* "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(defparameter *grid* (dict) "devel only")

(defun parse-input (input)
  (loop for line in (str:lines input)
        for j from 0
        with grid = (dict)
        do (loop for char across line
                 for i from 0
                 do (setf (gethash (complex i j) grid)
                          (if (char= char #\.)
                              :space
                              :paper)))
        finally (return grid)))

#++
(setf *grid* (parse-input *input*))

(defun height-locations (position)
  (list
   (+ position (complex 1 0))
   (+ position (complex -1 0))
   (+ position (complex 1 1))
   (+ position (complex 1 -1))
   (+ position (complex -1 1))
   (+ position (complex -1 -1))
   (+ position (complex 0 1))
   (+ position (complex 0 -1))
   ))

(defun less-than-4-adjacent-papers (position &key (grid *grid*))
  (when (equal :space (gethash position grid))
    (return-from less-than-4-adjacent-papers nil))
  (let ((count 0))
    (dolist (pos (height-locations position))
      (when (equal :paper (gethash pos grid))
        (incf count))
      (when (= count 4)
        (return-from less-than-4-adjacent-papers nil)))
    (values t count)))

(defun count-accessible-papers (&optional (grid *grid*))
  (loop for pos being the hash-key of grid
        count (less-than-4-adjacent-papers pos :grid grid)))

(defun part1 (input)
  (count-accessible-papers (parse-input input)))

#++
(part1 *input*)
;; 13

#++
(part1 (str:from-file "day04.txt"))
;; 1424 o/
;; 30ms


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;

;; will it scale??

(defun less-than-4-adjacent-papers/part2 (position)
  "Let's use a top-level *grid* variable,
  and modify it."
  (when (equal :space (gethash position *grid*))
    (return-from less-than-4-adjacent-papers/part2 nil))
  (let ((count 0))
    (dolist (pos (height-locations position))
      (when (equal :paper (gethash pos *grid*))
        (incf count))
      (when (= count 4)
        (return-from less-than-4-adjacent-papers/part2 nil)))

    ;; Move the roll of paper, clear the *grid* position.
    (setf (gethash position *grid*) :space)

    ;; return count as before.
    (values t count)))

(defun count-accessible-papers/part2 (&optional (grid *grid*))
  (loop for pos being the hash-key of grid
        count (less-than-4-adjacent-papers/part2 pos)))

(defun part2 (input)
  (let ((*grid* (parse-input input)))
    (loop for count-step = (count-accessible-papers/part2)
          when (plusp count-step)
            sum count-step into total
          when (zerop count-step)
            return total)))

#+ciel
(print (part2 (str:from-file "day04.txt")))

;; 8727 o/
;; in 170ms
;;
;; thank you creator for not putting too many hedge cases today and let us feel empowered 🤝
