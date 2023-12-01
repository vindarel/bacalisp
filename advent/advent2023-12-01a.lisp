
;; libraries:
;; CIEL-USER
;; or str, alexandria

(defparameter input "two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen")

(defparameter *file-input-1* "2023-12-01-a.txt")

;;; part 1
(defun collect-integers (input)
 (loop for line in (str:lines input) collect
  (loop for char across line
        for val = (ignore-errors (parse-integer (string char)))
        ;; better: collect the string.
        if val
          collect val)))

(collect-integers input)

(defparameter nb-lists '((1 2) (3 8) (1 2 3 4 5) (7)))

(defun reform-and-sum-pairs (nb-lists)
 (loop for nb-list in nb-lists
       for first = (first nb-list)
       for last = (last-elt nb-list)
       for s = (str:concat (prin1-to-string first) (prin1-to-string last))
       for nb = (parse-integer s)
       sum nb))

#+(or)
 (reform-and-sum-pairs (collect-integers
                      (str:from-file *file-input-1*)))
 ;; 54634

;;; part 2
(defparameter numbers '(("one" 1)
                         ("two" 2)
                         ("three" 3)
                         ("four" 4)
                         ("five" 5)
                         ("six" 6)
                         ("seven" 7)
                         ("eight" 8)
                         ("nine" 9)))

(defun starts-with-number (s)
 (some
  (lambda (nb-list)
    (when (str:starts-with-p (first nb-list) s)
      (second nb-list)))
  numbers))

 #+(or)
 (progn
   (starts-with-number "one1")
   (starts-with-number "heightx")
   (starts-with-number "nope")
   )

(defun find-real-numbers (input)
  ;; test with first line:
  ;; (loop for line in (list (first (str:lines input)))
  (loop for line in (str:lines input)
       collect
       (loop for s = line then (subseq s 1)
             until (str:blankp s)
             for val = (let ((maybe-int (ignore-errors (parse-integer (subseq s 0 1)))))
                         (if maybe-int
                             maybe-int
                             (starts-with-number s)))
             if val
               collect val
             )))

 #+(or)
 (find-real-numbers input)
 ;; ((2 1 9) (8 2 3) (1 2 3) (2 1 3 4) (4 9 8 7 2) (1 8 2 3 4) (7 6))

 #+(or)
 (reform-and-sum-pairs (find-real-numbers input))
 ;; 221

 #+(or)
 (reform-and-sum-pairs (find-real-numbers (str:from-file "2023-12-01-a.txt")))
  ;; 53855
