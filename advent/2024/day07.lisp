(defpackage :aoc-2024-07
  (:use :cl
        ;; :defclass-std
   :ciel))

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

(defparameter *operators* '(+ *))

(defun parse-input (input)
  (loop for line in (str:lines input)
        for nbs = (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" line))
        for test-value = (first nbs)
        for inputs = (rest nbs)
        collect (list test-value inputs)))

(defun permutations (operators &key length &aux perms)
  ;TODO: augment nb of operators.
  ;;THIS IS NOT WORKING
  ;;and generating all permutations will likely be too expensive.
  (alexandria:map-permutations (^ (it)  (push it perms))
                               operators
                               :length (or length (length operators)))
  perms)

(defun intersperse (nbs operators)
  "operators: a list of the right length that fits in-between nbs, an element of permutations."
  (remove-if #'null
    (loop for i from 0 to (length operators)
        collect (nth i nbs)
        collect (nth i operators))))

(defun compute-line (ops)
  (loop with i = 0
        while (< i (1- (length ops)))
        ;; for l = (log:info i ops)
        for x = (or x (nth i ops))
        for op = (or op (nth (1+ i) ops))
        for y = (or y (nth (+ 2 i) ops))
        ;; for _ = (log:info x op y)
        for res = (funcall op x y)
        do (setf x res
                 op nil
                 y nil)
        do (incf i 2)
        finally (return res)))
#++
(eval-line (intersperse '(10 19) '(*)))

(defun can-be-true (elts &optional (operators *operators*))
  (let* ((test-value (first elts))
         (nbs (second elts))
         (permutations (permutations operators :length (1- (length nbs)))))
    (loop for perm in permutations
          for ops = (print (intersperse nbs perm))
          if (equal test-value
                    (compute-line ops))
            return t)))

;; SHIT the list of operators doesn't always fit :/
