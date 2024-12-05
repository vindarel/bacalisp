
(defpackage :aoc-2024-03/parseq
  (:use :cl :parseq))

(in-package :aoc-2024-03/parseq)

;; Adapted from
;; https://github.com/coderfrog256/AdventOfCode/blob/main/2024/lisp/Day3.lisp
;; to learn
;; https://github.com/mrossini-ethz/parseq (PEG)

(defrule int ()
    digit
  ;; the integer rule doesn't work?
  ;; is it necessary to call parse-integer on the result, no number right off?
  (:function (lambda (arg) (parse-integer (string arg)))))

(defrule max3ints ()
    (rep (1 3) digit)
  (:function (lambda (&rest args)
               (parse-integer
                (str:join "" (mapcar #'string args))))))

;; Get integers and multiply them.
(parseq:defrule mul ()
    (and "mul(" max3ints "," max3ints ")")
  ;; this gets an annoying SBCL warning.
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
