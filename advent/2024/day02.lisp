(uiop:define-package :aoc-2024-02
    (:use :cl
     :ciel  ;; for libraries: str
          ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
          ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
          ;; See below.)
     ))

(in-package :aoc-2024-02)

(defparameter *input* "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9")

(defparameter *file-input* "input-day02.txt")

(defun safe-row (row)
  (when (stringp row)
    (setf row (mapcar #'parse-integer (str:words row))))
  (loop :for rest :on row  ;; "on" advances on the list one by one.
        for cur = (car rest)
        for next = (or (cadr rest)
                       (return t))
        with order = nil
        for current-order = (cond
                              ((< cur next) :ascending)
                              ((> cur next) :descending)
                              (t
                               (return nil)))
        for step = (abs (- next cur))
        if (not (<= 1 step 3))
          do (return nil)
        if (not order)
          do (setf order current-order)
        if (not (equal order current-order))
          do (return nil)
        finally (return t)))

#++
(progn
  (safe-row "7 6 4 2 1")
  ;; ((7 6) (6 4) (4 2) (2 1) (1 NIL))

  (safe-row "1 2 7 8 9")
  ;; NIL

  (safe-row "9 7 6 2 1")
  ;; NIL
  )

(defun part1 (input)
  (loop for line in (str:lines input)
    count (safe-row line)))

#+ciel
(part1 *input*)
;; 2

#+ciel
(part1 (str:from-file *file-input*))
;; 670
;; o/
