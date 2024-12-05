(defpackage :aoc-2024-04
  (:use :cl
   :ciel))

;;;
;;;
;;; Wrong approach to do part 2.
;;;
;;; better: map all points in a grid represented by a hash-table.
;;; - keys: a tuple (i j) such as a complex number
;;; - values: the character
;;;

(in-package :aoc-2024-04)

(defparameter *FILE-INPUT* "input-day04.txt")

(defparameter *input* "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX")

(defparameter *simple-input*
"..X...
.SAMX.
.A..A.
XMAS.S
.X....")

;; Inverted matrix
#|
"...X.
.SAMX
XA.A.
.M.S.
.XA..
...S."
|#


(defun count-xmas (line)
  "Count the number of XMAS in this line, and the reversed line."
  (+ (ppcre:count-matches "xmas" (str:downcase line))
     (ppcre:count-matches "xmas" (str:downcase
                                  (reverse line)))))

#++
(assert (equal 3 (count-xmas "xmasxmasxmasxxx")))


(defun count-matching-lines (lines)
  (loop for line in lines
        sum (count-xmas line)))

(defun reform-lines (lists)
  (loop for list in lists
        collect (str:concat list)))

(defun invert-matrix (objects)
  ;; adapted from ansi-term.
  "Transform a list of rows to a list of columns.

  (invert-matrix '((a b c) (1 2 3)))
  ;; => ((A 1) (B 2) (C 3))
  "
  (loop :with content-length = (length (first objects))
        :with grid = (loop :repeat content-length :collect (make-list (length objects)))
        :for obj :in objects
        :for i := 0 :then (incf i)
        :do (loop :for j :from 0 :to (1- content-length)
                  :for row := (elt grid j)
                  :do  (setf (elt row i) (elt obj j)))
        :finally (return (reform-lines grid))))

(defun diag (lines)
  (loop for line in lines
        for i from 0
        nconc
        (loop for char across line
              for j from 0
              if (equal i j)
                collect char)))

#++
(diag (str:lines *INPUT*))
;; (#\. #\S #\. #\S #\.)

;; Now do the same on (rest lines) to get the shorter diagonals,
;; then do the same on the inverse matrix.

(defun all-diags (lines &optional res)
  (let ((diag (diag lines)))
    (if diag
        (all-diags (rest lines) (cons diag res))
        (reform-lines (reverse res)))))

(defun all-diags-as-strings (lines)
  (reform-lines (all-diags lines)))
;; (".S.S." ".AA." ".M." "XX" ".")

(defun cut-off-column (lines)
  (loop for line in lines
        for i from 0
        collect (subseq line 1)))

(defun all-right-diags (lines &optional res)
  (let ((diag (diag lines)))
    (if diag
        (all-right-diags (cut-off-column lines) (cons diag res))
        (reform-lines (reverse res)))))


(defun part-1 (input)
  (let* ((lines (str:lines (str:trim input)))
         (inverted (invert-matrix lines)))
    (+ (count-matching-lines lines)
       ;; diagonals
       (count-matching-lines (all-right-diags (cut-off-column lines)))
       (count-matching-lines (all-right-diags lines))

       ;; inverted matrix
       (count-matching-lines inverted)
       ;; diagonals of inverted matrix
       (count-matching-lines (all-right-diags (cut-off-column inverted)))
       (count-matching-lines (all-right-diags inverted))
       )))

#++
(part-1 *simple-input*)
;; 18

#+ciel
(format t "~&day04 part1: ~a~&" (part-1 (str:from-file *file-input*)))

#+ciel
(part-1 (str:from-file *file-input*))
;; 2547 :(
;; also got 2561.
;; Real answer is 2560 :(


(defparameter *test-input* "S..S..S
.A.A.A.
..MMM..
SAMXMAS
..MMM..
.A.A.A.
S..S..S")

(defparameter *test-input-2* "A.A.A
.MMM.
AMXMA
.MMM.
A.A.A")
