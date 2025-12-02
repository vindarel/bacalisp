(uiop:define-package :aoc-2025-01
    (:use :cl
     :ciel  ;; for libraries: str and serapeum:dict and macro ^ (lambda shortcut)
          ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
          ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
          ;; See below.)
     ))

(in-package :aoc-2025-01)

(defparameter *input* "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")

(defparameter dial-min 0)  ;; bad, use *earmuffs*
(defparameter dial-max (1+ 99)
  "Account for 0.")

(defun parse-input (input)
  (loop for line in (str:lines input)
        collect (list
                 (subseq line 0 1)
                 (parse-integer (subseq line 1)))))

(defun rotate (start direction count)
  "direction: L or R."
  ;; Rotating by 501 equals to rotating by 1.
  (let* ((count (nth-value 1 (floor count dial-max)))
         (op (str:string-case direction
               ("L" #'-)
               ("R" #'+)
               (t (error "unknown direction: ~s" direction))))
         (tmp (funcall op start count)))
    (cond
      ((minusp tmp)
       (+ dial-max tmp))
      ((>= tmp dial-max)
       (- tmp dial-max))
      (t
       tmp))))

#+testit
(and
 (= 0 (rotate 99 "L" 99))
 (= 55 (rotate 95 "R" 60))
 (= 96 (rotate 95 "R" 5001))
 )

(defun part1 (input &key (start 50))
  (loop for (direction count) in (parse-input input)
        with nb-of-zeros = 0
        for tmp = (rotate start direction count)
        when (zerop tmp)
          do (incf nb-of-zeros)
        do (setf start tmp)
        finally (return nb-of-zeros)))

#+ciel
(part1 (str:from-file "day01.txt"))
;; 964

;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;

(defun rotate2 (start direction count)
  "This time, we also return the number of times we pass in front the 0 tick.

  Return 2 values."
  ;; Rotating by 501 equals to rotating by 1.
  ;; And in part 2, we must count how many times we pass by 0

  ;; bind = metabang-bind, included into CIEL.
  (bind (((:values passing-by-zero count) (floor count dial-max))
         (op (str:string-case direction
               ;; the clever ones multiplied by -1 or +1 :p
               ("L" #'-)
               ("R" #'+)
               (t (error "unknown direction: ~s" direction))))
         (tmp (funcall op start count)))

    (when (and (minusp tmp)
               (not (zerop start)))
      (incf passing-by-zero))

    (cond
      ((minusp tmp)
       (values
        (+ dial-max tmp)
        passing-by-zero))
      ((>= tmp dial-max)
       (values
        (- tmp dial-max)
        (1+ passing-by-zero)))
      (t
       (when (zerop tmp)
         (incf passing-by-zero))
       (values tmp
               passing-by-zero)))))

#+testit
(and
 (multiple-value-bind (position passing-by-zero)
     (rotate2 50 "L" 55)
   (= 0 position)
   (= 1 passing-by-zero))
 )

(defun part2 (input &key (start 50))
  (loop for (direction count) in (parse-input input)
        with nb-of-zeros = 0
        do (multiple-value-bind (tmp passing-by-zero)
               (rotate2 start direction count)
             (incf nb-of-zeros (or passing-by-zero 0))
             (setf start tmp))
        finally (return nb-of-zeros)))

#+ciel
(format t "day part2: ~a~&" (part2 (str:from-file "day01.txt")))
;; 5872
;; o/
