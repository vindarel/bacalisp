
(defpackage :aoc-2024-03/parseq
  (:use :cl :parseq))

(in-package :aoc-2024-03/parseq)

;; Adapted from
;; https://github.com/coderfrog256/AdventOfCode/blob/main/2024/lisp/Day3.lisp
;; to learn
;; https://github.com/mrossini-ethz/parseq (PEG)

(defrule int ()
    digit
  ;; the integer rule doesn't work? => only with real integers with input as a LIST (not string).
  ;; parseq accepts sequences as input: strings, lists, vectors. Not only strings.
  ;; same for number
  ;; digit is to match a numeric character, in a string.
  (:function (lambda (arg) (parse-integer (string arg)))))

(defrule max3ints ()
    (rep (1 3) digit)
  (:function (lambda (&rest args)
               (parse-integer
                (str:join "" (mapcar #'string args))))))

;; Get integers and multiply them.
(parseq:defrule mul ()
    (and "mul(" max3ints "," max3ints ")")
  ;; this produces an annoying SBCL warning.
  (:function (lambda (&rest args) (* (nth 1 args) (nth 3 args)))))


(defrule part-1-parser ()
   (* (or mul char))
  ;; matching "char" gets us noise… how to avoid it? We can remove it though.
  ;; Our result is a mixed bug of real results, which are integers but could be
  ;; anything thanks to the return :functions,
  ;; so it's easy to filter out the noise.
  (:function (lambda (&rest args)
             (remove-if #'characterp args))))

(defun part-1 (input)
  (reduce #'+ (parseq 'part-1-parser input)))

#++
(part-1 aoc-2024-03::*input*)
;; 161
;; o/
