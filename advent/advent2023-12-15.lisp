
(uiop:define-package :aoc-2023-15
  (:use :cl :ciel))

(in-package :aoc-2023-15)

(defparameter *file-input* "advent2023-12-15.txt")

(defparameter input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,
ot=7")

(defun holiday-HASH (s)
  (loop for char across s
        with current-value = 0
        for code = (char-code char)
        do
           (incf current-value code)
           (setf current-value (* current-value 17))
           (setf current-value (rem current-value 256))
        finally (return current-value)))

#+(or)
(holiday-HASH "HASH")
;; 52

(defun parse-input (input)
  (str:split "," (ppcre:regex-replace-all "\\n" input "")))

(defun part1 (input)
  (reduce #'+ (mapcar #'holiday-HASH (parse-input input))))

#(+or)
(part1 (str:from-file *file-input*))
;; 495972
