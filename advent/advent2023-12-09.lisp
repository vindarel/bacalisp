
(uiop:define-package :aoc-2023-09
  (:use :cl :ciel))

(in-package :aoc-2023-09)

(defparameter *file-input* "advent2023-12-09.txt")

(defparameter input "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45")

(defun parse-line (s)
  ;; with negative numbers!
  ;; str:words safer?
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "-?\\d+" s)))

(loop for i in seq
      and j = 0 then i
      do (print (cons i j))
      collect (- i j))

(defun seq-differences (seq)
  (loop for (j i) on seq by #'cdr
        if (and i j)
          collect (- i j)))

(defun iter-differences (seq &optional (adding 0))
  (let* ((differences (seq-differences seq))
         (duplicates (remove-duplicates differences)))
    (if (= 1 (length duplicates))
        ;; or check for every #'zerop differences
        (+ adding (first duplicates))
        (+ adding (iter-differences differences (last-elt differences))))))

#+(or)
(+ (last-elt seq)
   (iter-differences seq))

(defun next-elt (seq)
  (+ (last-elt seq)
     (iter-differences seq)))

(defun part1 (input)
  (reduce #'+ (mapcar #'next-elt (mapcar #'parse-line (str:lines input)))))

(defun main ()
  (part1 (str:from-file *file-input*)))

#+ciel
(main)

;; part 2

;; yep I started by modifying the algorithm.

(defun part2 (input)
  (reduce #'+ (mapcar #'next-elt
                      (mapcar #'reverse ;; <-- that's all.
                              (mapcar #'parse-line (str:lines input))))))

#+(or)
(reduce #'+ (mapcar
             (alexandria:compose #'next-elt #'reverse #'parse-line)
             (str:lines input)))
