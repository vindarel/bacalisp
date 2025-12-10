
(uiop:define-package :aoc-2025-09 (:use :cl
     :ciel  ;; for libraries: str, serapeum:dict and more useful ones.
          ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
          ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
          ;; See below.)
          ;; http://ciel-lang.org/
          )
  (:documentation ""))

(in-package :aoc-2025-09)

(defparameter *input* "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3")

;;;
;;; Sort points by distance from the top-lef corner,
;;; iterate on them and on the reverse order (but not all, half should be enough),
;;; let LOOP maximize the area we can get.
;;;

(defun parse-input (input)
  (mapcar (^ (line)
             (mapcar #'parse-integer (str:split "," line)))
          (str:lines input)))

#++
(defparameter *points* (parse-input *input*))

(defun distance-from-top-left (p)
  (sqrt (+ (expt (- (first p) 0) 2)
           (expt (- (second p) 0) 2))))


(defun sort-by-distance-top-left (points)
  (sort (copy-list points) #'< :key #'distance-from-top-left))

#++
(defparameter *ordering* (sort-by-distance-top-left *points*))

(defun area (p q)
  (let ((xs (sort (list (first p) (first q)) #'<))
        (ys (sort (list (second p) (second q)) #'<)))
    (* (1+ (- (second xs) (first xs)))
       (1+ (- (second ys) (first ys))))))

(defun areas-left-to-right (points)
  (let ((ordering (sort-by-distance-top-left points)))
    (loop for p in ordering
          for i from 0 below (floor (length ordering) 2)
          maximize (loop for q in (reverse ordering)
                         maximize (area p q)))))

(defun part1 (input)
  (areas-left-to-right (parse-input input)))

#++
(part1 *input*)

#+ciel
(part1 (str:from-file "day09.txt"))
;; 4774877510
;; at first try o/
;; in 30ms

;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;
