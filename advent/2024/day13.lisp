(uiop:define-package :aoc-2024-13 (:use :cl
   :ciel  ;; for the libraries: cl-str (with newer str:paragraphs) and cl-arrows (->)
   ))

(in-package :aoc-2024-13)

(defparameter *file-input* "input-day13.txt")

(defparameter *machine* "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400")

(defparameter *input* "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279")

(defun match-line (line)
  (str:match line
    (("Button A: X\\+" a1 ", Y\\+" a2)
     (list (parse-integer a1)
           (parse-integer a2)))
    (("Button B: X\\+" b1 ", Y\\+" b2)
     (list (parse-integer b1)
           (parse-integer b2)))
    (("Prize: X=" c1 ", Y=" c2)
     (list (parse-integer c1)
           (parse-integer c2)))))

(defun parse-machine (input)
  (flatten
   (mapcar #'match-line (str:lines input))))

(defun parse-input (input)
  (mapcar #'parse-machine (str:paragraphs input)))

(defun cramer (a1 a2 b1 b2 c1 c2)
  (let ((x-num (- (* c1 b2)
                  (* b1 c2)))
        (y-num (- (* a1 c2)
                  (* c1 a2)))
        (den (- (* a1 b2)
                (* b1 a2))))
    (list
     (/ x-num den 1.0)
     (/ y-num den 1.0))))

(defun solve-machine (data)
  (apply #'cramer data))

(defun solve-all (coords)
  (mapcar #'solve-machine coords))

(defun is-int (nb)
  (multiple-value-bind (part rest)
      (floor nb)
    (declare (ignore part))
    (zerop rest)))

(defun cost-of-all (results)
  (loop for pair in results
        if (and (is-int (first pair))
                (is-int (second pair)))

          sum (+ (* 3 (first pair))
                 (second pair))))

(defun part-1 (input)
  (-> input
    (parse-input)
    solve-all
    cost-of-all))

#++
(part-1 *input*)
;; 480.0

#++
(part-1 (str:from-file *file-input*))
;; 30973.0 o/
