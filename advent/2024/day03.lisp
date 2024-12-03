(uiop:define-package :aoc-2024-03
    (:use :cl
          :uclp  ;; PEG grammars, à la Janet. Not in Quicklisp.
     ;; :ciel  ;; for libraries:
     ;;      ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
     ;;      ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
     ;;      ;; See below.)
          ))

(in-package :aoc-2024-03)


(defparameter *input* "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")

(defparameter *file-input* "input-day03.txt")

(defparameter mul-grammar
  '(grammar
    :mul "mul"
    :nb (between :d 1 3)
    :main (sequence :mul "(" :nb "," :nb ")")))

#++
(find-all MUL-GRAMMAR *input*)

;; But how do we get the matches as text?
;; no built-in way?
(defun matches-as-strings (s)
  (let ((indexes (find-all mul-grammar s)))
    (loop for i in indexes
          for sub = (subseq s i)
          for end = (search ")" sub :test #'equal)
          for match = (subseq sub 0 (1+ end))
          collect match)))

(defun parse-match (s)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" s)))

(defun matches-as-ints (s)
  (mapcar #'parse-match (matches-as-strings s)))

(defun part1 (input)
  (reduce #'+ (mapcar (serapeum:partial #'apply #'*) (matches-as-ints input))))

#++
(part1 *input*)
;; 161

#++
(print (part1 (str:from-file *file-input*)))
;; 190604937 o/
