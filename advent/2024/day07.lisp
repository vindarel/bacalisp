(defpackage :aoc-2024-07
  (:use :cl
   :ciel  ;; for the libraries: cl-str ppcre
   ))

(in-package :aoc-2024-07)

(defparameter *file-input* "input-day07.txt")


(defparameter *input* "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20")

(defparameter *operators-1* '(+ *))
(defparameter *operators-2* '(+ * \|))

(defun parse-input (input)
  (loop for line in (str:lines input)
        for nbs = (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" line))
        for test-value = (first nbs)
        for inputs = (rest nbs)
        collect (list test-value inputs)))

(defparameter *ops*
  (dict '+ (lambda (a b) (+ a b))
        '* (lambda (a b) (* a b))
        '\| (lambda (a b) (parse-integer (str:concat (princ-to-string a) (princ-to-string b))))))

;; let's NOT compute the operators' permutations,
;; but try them all at each place, and exit early.
(defun compute-line (target nbs operators)
  (labels ((rec (i result)
             (cond
               ((> result target)
                (return-from rec 0))
               ((= i (length nbs))
                (return-from rec (if (= result target)
                                     target
                                     0)))
               (t
                (loop for op in operators
                      for res = (rec (1+ i)
                                     (funcall (gethash op *ops*)
                                              result
                                              (nth i nbs)))
                      if (= res target)
                        return res
                      finally (return 0))))))
    (rec 1 (nth 0 nbs))))

#++
(compute-line (first (first *parsed-input*)) (second (first *parsed-input*)) *operators-1*)

(defun part-1 (input)
  (loop for in in (parse-input input)
        sum (compute-line (first in) (second in) *operators-1*)))

#++
(part-1 *input*)
;; 3749

#++
(part-1 (str:from-file *file-input*))
;; 6392012777720
;; 0.035 seconds of real time

(defun part-2 (input)
  (loop for in in (parse-input input)
        sum (compute-line (first in) (second in) *operators-2*)))

#++
(part-2 (str:from-file *file-input*))
;; 61561126043536 o/
;; 1.947 seconds  too high!
