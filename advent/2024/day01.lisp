(uiop:define-package :aoc-2024-01
    (:use :cl
     :ciel  ;; for libraries: str and serapeum:dict and macro ^ (lambda shortcut)
          ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
          ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
          ;; See below.)
     ))

(in-package :aoc-2024-01)

(defparameter *input* "3   4
4   3
2   5
1   3
3   9
3   3")

(defun parse-input (input)
  (loop for line in (str:lines input)
        for words = (str:words line)
        collect (parse-integer (first words)) into col1
        collect (parse-integer (second words)) into col2
        finally (return (list col1 col2))))

#++
(parse-input *input*)
;; ((3 4 2 1 3 3) (4 3 5 3 9 3))

(defun sort-columns (list)
  (list
   (sort (first list) #'<)
   (sort (second list) #'<)))

#++
(sort-columns (parse-input *input*))
;; ((1 2 3 3 3 4) (3 3 3 4 5 9))

(defun distances (list)
  (mapcar (^ (x y) (abs (- x y))) (first list) (second list)))

#++
(distances (sort-columns (parse-input *input*)))
;; (2 1 0 1 2 5)

(defun sum-distances (distances)
  (reduce #'+ distances))

#++
(sum-distances (distances (sort-columns (parse-input *input*))))
;; 11

(defparameter *file-input-1* "input-2024-01.txt")
(defun part1 (input)
  (sum-distances (distances (sort-columns (parse-input input)))))

#++
(part1 (str:from-file *file-input-1*))
;; 1388114


(defun count-occurences (col &aux (counts (dict)))
  ;; Count each nb.
  (dolist (nb col counts)
    (incf (gethash nb counts 0)))

  ;; (loop with counts = (dict)
  ;;       for nb in col
  ;;       do (incf (gethash nb counts 0))
  ;;       finally (return counts))
  )

#++
(count-occurences (second '((1 2 3 3 3 4) (3 3 3 4 5 9))))
 ;; (dict
 ;;  3 3
 ;;  4 1
 ;;  5 1
 ;;  9 1
 ;; )

(defun similarity-score (cols)
  (let ((occurences (count-occurences (second cols))))
    (reduce #'+
            (mapcar (^ (nb)
                       (* nb (gethash nb occurences 0)))
                    (first cols)))))

#++
(similarity-score '((1 2 3 3 3 4) (3 3 3 4 5 9)))
;; 31

(defun part2 (input)
  (similarity-score (sort-columns (parse-input input))))

#++
(part2 (str:from-file *file-input-1*))
;; 23529853

;; o/

;;
;; Running as a script from the terminal:
;;
;; $ ciel day01.lisp
;;
;; and that's it.
;;
;; (fast start-up: runs in 0.03s)

#+ciel
(format t "AOC day 01-part1 solution is: ~a~&" (part1 (str:from-file *file-input-1*)))

#+ciel
(format t "AOC day 01-part2 solution is: ~a~&" (part2 (str:from-file *file-input-1*)))
