
(uiop:define-package :aoc-2025-03
    (:use :cl
     :ciel  ;; for libraries: str, serapeum:dict and more useful ones.
          ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
          ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
          ;; See below.)
          ;; http://ciel-lang.org/
          )
  (:documentation ""))

(in-package :aoc-2025-03)

(defparameter *input* "987654321111111
811111111111119
234234234234278
818181911112111")

(defun parse-input (input)
  (str:lines input))

(defun largest-joltage (s)
  (let* ((len (length s))
         (sorted (sort (subseq s 0 (1- len))
                       #'char>=))
         (biggest (elt sorted 0))
         ;; second element
         (index (1+ (position biggest s)))
         (rest (subseq s index))
         (sorted-rest (sort rest #'char>=))
         (second-biggest (elt sorted-rest 0))
         )
    (parse-integer (format nil "~a~a" biggest second-biggest))))

#+testit
(largest-joltage "818181911112111")

(defun part1 (input)
  (reduce #'+ (mapcar #'largest-joltage (parse-input input))))

#+ciel
(format t "~&day03: ~a" (part1 (str:from-file "day03.txt")))
;; 17193 o/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun largest-joltage/part2 (s nth-joltage)
  "Similar, but only find 1 element, return the rest of the string."
  ;; nth-joltage is an index of base 1… substract 1 for 0-based indexing.
  (let* ((len (length s))
         (sorted (sort (subseq s 0 (- len (1- nth-joltage)))
                       #'char>=))
         (biggest (elt sorted 0))
         ;; remaining string.
         (index (1+ (position biggest s)))
         (rest (subseq s index)))
    (values biggest
            rest)))

(defun get-12-joltage (s)
  "Recursion or not recursion? Not this time. I've got the flow!"
  (loop for nth-joltage from 12 downto 1
        with res = nil
        do (multiple-value-bind (nb new-s)
               (largest-joltage/part2 s nth-joltage)
             (setf s new-s)
             (push nb res))
        finally (return
                  (parse-integer
                   (concatenate 'string (reverse res))))))

(defun part2 (input)
  (reduce #'+ (mapcar #'get-12-joltage (parse-input input))))

#+ciel
(format t "~&day03: ~a" (part2 (str:from-file "day03.txt")))
;; 171297349921310 o/
