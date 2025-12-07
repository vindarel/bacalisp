
(uiop:define-package :aoc-2025-07
    (:use :cl
     :ciel  ;; for libraries: str, serapeum:dict and more useful ones.
          ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
          ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
          ;; See below.)
          ;; http://ciel-lang.org/
          )
  (:documentation ""))

(in-package :aoc-2025-07)

(defparameter *input* ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

(defparameter *grid* (dict) "devel only")

(defparameter *splitters* (dict)
  "coordinate -> dict: visited ?")

(defvar *start* nil "start position")

(defun parse-input (input)
  "Grid as a hash-table, coordinates as complex numbers. This usually works well."
  (loop for line in (str:lines input)
        with grid = (dict)
        ;; with splitters = (dict)
        for j from 0
        do (loop for char across line
                 for i from 0
                 for key = (complex i j)
                 do (setf (gethash key grid) char)
                    (when (equal #\S char)
                      (setf *start* key))
                    (when (equal #\^ char)
                      (setf (gethash key *splitters*)
                            (dict :visited-left nil
                                  :visited-right nil))))
        finally (return grid)))

#++
(setf *grid* (parse-input *input*))

(defun next (coord)
  (+ coord (complex 0 1)))

(defun splitter-p (coord)
  (equal (gethash coord *grid*) #\^))

(defun reaches-splitter (coord)
  (splitter-p (next coord)))

(defun divide-right-next (coord)
  (+ (complex 1 1) coord))

(defun divide-left-next (coord)
  (+ (complex -1 1) coord))



(defun rdescend (&key (grid *grid*) (coord *start*))
  (cond
    ((null (gethash coord grid))
     t)
    ((reaches-splitter coord)
     (let ((right (divide-right-next coord)))
       (unless (gethash :visited-right
                        (gethash (next coord) *splitters*))

         (setf (gethash :visited-right (gethash (next coord) *splitters*))
               t)
         ;; I tried a LOOP and I setf the new coord to right here… it doesn't work.
         ;; It fits fairly well to a reursive solution, I don't actually see how to do it with a loop.
         (rdescend :coord right)))

     (let ((left (divide-left-next coord)))
       (unless (gethash :visited-left
                        (gethash (next coord) *splitters*))

         (setf (gethash :visited-left (gethash (next coord) *splitters*))
               t)
         (rdescend :coord left)))
     )
    (t
     (rdescend :coord (next coord)))))

(defun try (&optional (input *input*) &aux (count-visited-splitters 0))
  ;; yeah, top-level params not very clean from a recursive and purely functional perspective.
  ;; They are handy tough!
  (let* ((*splitters* (dict))
         (*grid* (parse-input input)))
    (rdescend)

    (do-hash-table (k v *splitters*)
      (declare (ignore k))
      (when (and (gethash :visited-left v)
                 (gethash :visited-right v))
        (incf count-visited-splitters))))
  count-visited-splitters)

(defun part1 (input)
  (try input))

#+ciel
(part1 (str:from-file "day07.txt"))
;; 1537 o/
;; not even a ms
