(uiop:define-package :aoc-2024-03
    (:use :cl
          :uclp  ;; PEG grammars, à la Janet. Not in Quicklisp.
          ;; https://github.com/ravi-delia/uclp

     ;; :ciel  ;; for libraries:
     ;;      ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
     ;;      ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
     ;;      ;; See below.)


          )
  (:import-from :serapeum
   :dict)
  )

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


;;;
;;; Part 2.
;;;

(defparameter *input-2* "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(defparameter mul-grammar/2
  '(grammar
    :nb (between :d 1 3)
    :mul (sequence "mul" "(" :nb "," :nb ")")
    :do "do()"
    :dont "don't()"
    :main (choice :mul :do :dont)))

;; must not match:
#++
(collect-rules (matches-as-objects "mul(536,959$(~#&*[~})"))

(defun make-match (s x y)
  (dict :op s
        :x x
        :y y))

(defun is-do (match)
  (str:containsp "do(" (gethash :op match)))

(defun is-dont (match)
  (str:containsp "don't(" (gethash :op match)))

(defun is-mul (match)
  (str:containsp "mul" (gethash :op match)))

(defun matches-as-objects (s &key (grammar mul-grammar/2))
  "Same as above, but return a list of hash-tables."
  (let ((indexes (find-all grammar s)))
    (loop for i in indexes
          for sub = (subseq s i)
          for end = (search ")" sub :test #'equal)
          for match = (subseq sub 0 (1+ end))
          for tuple = (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" match))
          collect (make-match match (car tuple) (cadr tuple)))))

(defun collect-rules (matches)
  (loop for match in matches
        with collect = t
        do (cond
             ((is-do match)
              (setf collect t))
             ((is-dont match)
              (setf collect nil)))
        if (and  collect (is-mul match))
          collect match))

(defun part2 (input)
  (loop for match in (collect-rules (matches-as-objects input))
        sum (* (gethash :x match) (gethash :y match))))


#++
(print (part2 (str:from-file *file-input*)))
;; 82857512 o/
