
(uiop:define-package :aoc-2025-06
    (:use :cl
     :ciel  ;; for libraries: str, serapeum:dict and more useful ones.
          ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
          ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
          ;; See below.)
          ;; http://ciel-lang.org/
          )
  (:documentation ""))

(in-package :aoc-2025-06)

(defparameter *input* "123 328  51 64
 45 64  387 23
  6 98  215 314
*   +   *   +")

(defun parse-input (input)
  (let* ((lines (str:lines input))
         (number-lines (butlast lines))
         (ops (str:words (last-elt lines))))
    (loop with xys = (make-array (list (length (str:words (first lines)))
                                       (1- (length lines))))

          for line in number-lines
          for j from 0
          do
             (loop for word in (str:words line)
                   ;; I mixed up the indices but that's how we wwant them ahahah.
                   for i from 0
                   do (setf (aref xys i j) (parse-integer word)))
          finally (return (list xys ops)))))

#++
(defparameter *array* (first (parse-input *input*)))

(defun compute-problems (worksheet/ops)
  (loop with worksheet = (first worksheet/ops)
        for i from 0 below (array-dimension worksheet 0)
        for op in (second worksheet/ops)
        sum (reduce (str:string-case op
                      ("*" '*)
                      ("+" '+)
                      ("-" '-)
                      (t (error "unknown operation: ~a" op)))
                    (loop for j from 0 below (array-dimension worksheet 1)
                          collect (aref worksheet i j))
                    )))

(defun part1 (input)
  (compute-problems (parse-input input)))

#++
(part1 *input*)

#+ciel
(part1 (str:from-file "day06.txt"))
;; 6635273135233 o/
;; 7ms

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Look, it's a mess, I'm not re-using everything from part1,
;; but I don't fight the flow :p (nor array indices lol).


;; I prefer to manipulate strings than multi-dimensionnal arrays, so
;; we re-create an input string, turned 90° (but not sure which
;; direction ahah).

(defun transpose-grid (s)
  (let ((lines (str:lines s)))
    (loop for line in lines
          with array = (make-array (list (1+ (length (first lines)))
                                         (length lines)))
          for j from 0
          do
             (loop for char across line
                   for i from 0
                   do (setf (aref array i j) char))
          finally (return array))))

#+testit
(transpose-grid
 "a b c
d e f")

#++
(defparameter *grid* "123 328  51 64
 45 64  387 23
  6 98  215 314")

#+testit
(transpose-grid *grid*)

(defun array-to-string (a)
  (with-output-to-string (s)
    (loop for i from 0 below (array-dimension a 0)
          do (loop for j from 0 below (array-dimension a 1)
                   do (princ (aref a i j) s)
                   finally (princ #\Newline s)))))

#+testit
(array-to-string
 (transpose-grid *grid*))

;; This gives us:
#|
"1
24
356

369
248
8

 32
581
175

623
431
004"

not the form I was betting on, but that's easy to handle.

FORTUNATELY there was no / or - operators, where operands order must be preserved.
|#

(defun %parse-intermediate-input (s)
  (loop for line in (str:lines s)
        with groups = '()
        with group = '()
        if (str:blankp line)
          do  (push (reverse group) groups)
              (setf group '())
        else
          do (push (parse-integer line) group)
        finally (push group groups)
                (return (reverse groups))))

#+testit
(%parse-intermediate-input (array-to-string (transpose-grid *grid*)))

(defun compute-sets (sets ops)
  (loop for set in sets
        for op in ops
        sum (reduce (str:string-case op
                      ("*" '*)
                      ("+" '+)
                      ("-" '-)
                      (t (error "unknown operation: ~a" op)))
                    set)))


(defun get-sheet (input)
  (str:unlines (butlast (str:lines input))))

(defun part2 (input)
  (compute-sets (%parse-intermediate-input
                 (array-to-string (transpose-grid (get-sheet input))))
                (second (parse-input input))))

#++
(part2 *input*)

#+ciel
(part2 (str:from-file "day06.txt"))
;; 12542543681221 o/
;; 15ms
