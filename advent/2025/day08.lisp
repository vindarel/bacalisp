
;; (ql:quickload "fset")
;; to use proper SETs, and more.

(uiop:define-package :aoc-2025-08
    (:use :cl
     :ciel  ;; for libraries: str, serapeum:dict and more useful ones.
          ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
          ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
          ;; See below.)
          ;; http://ciel-lang.org/
          )
  (:documentation ""))

(in-package :aoc-2025-08)

(defparameter *input* "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

(defstruct point
  x
  y
  z
  circuit-id)

(defun parse-point (s)
  (let* ((xyz (mapcar #'parse-integer (str:split "," s))))
    (make-point :x (first xyz) :y (second xyz) :z (third xyz))))

(defun parse-input (input)
  (mapcar #'parse-point (str:lines input)))

#++
(defparameter *points* (parse-input *input*))

(defun distance (p q)
  (sqrt (+ (expt (- (point-x p) (point-x q)) 2)
           (expt (- (point-y p) (point-y q)) 2)
           (expt (- (point-z p) (point-z q)) 2))))

(defparameter *distances* (dict)
  "set of points -> float")

;;;;;;;;;;;;;;;;;;
;; trying FSet…
;; I want a SET… I'm pulling more of FSet then.

(defun compute-distances (points)
  (setf *distances* (fset:empty-map))
  (loop for p in points
        do (loop for q in points
                 for key = (fset:set p q)
                 unless (or (equal p q)
                            (fset:lookup key *distances*))
                   do (fset:includef *distances* key (distance p q))))
  *distances*)

(defun sort-distances (distances)
  (fset:sort (fset:convert 'fset:seq distances) #'< :key #'cdr))

(defun take-n (sorted-distances &optional (n 10))
  (serapeum:take n (fset:convert 'list sorted-distances)))

;; Ugh, I'm doing too many conversions fset -> lisp.


;;;;;;
;; Without FSet

(defun distances (points)
  (setf *distances* (dict))
  (loop for p in points
        do (loop for q in points
                 for key = (sort (list p q) #'< :key #'point-x) ;; our unique set
                 unless (or (equal p q)
                            (gethash key *distances*))
                   do (setf (gethash key *distances*) (distance p q))))
  *distances*)

(defun sort-distances (&optional (distances *distances*))
  (sort (hash-table-alist distances) #'< :key #'cdr))


(defparameter *clusters* (dict)
  "id -> points")

(defun group-pairs (alists)
  ;; *clusters* keeps track of our points with a given ID.
  ;;
  ;; I'd prefer to move stuf into sets,
  ;; but I don't know enough FSet.
  (setf *clusters* (dict))
  (let ((id 0))
    (loop for alist in alists
          for points = (car alist)
          for a = (first points)
          for b = (second points)
          for distance = (cdr alist)
          for a-id = (point-circuit-id (first points))
          for b-id = (point-circuit-id (second points))
          for total = (length *points*)
          do (cond
               ((and a-id (not b-id))
                (log:info "a -> b" a-id a b)
                (setf (point-circuit-id b) a-id)
                (pushnew b (gethash a-id *clusters*))
                )
               ((and b-id (not a-id))
                (log:info "b -> a" b-id b a)
                (setf (point-circuit-id a) b-id)
                (pushnew a (gethash b-id *clusters*)))
               ((and (not a-id) (not b-id))
                (incf id)
                (log:info "new circuit" id a b)
                (setf (point-circuit-id a) id
                      (point-circuit-id b) id)
                (push a (gethash id *clusters*))
                (push b (gethash id *clusters*))
                )
               ((equal a-id b-id)
                (log:info " == nothing" a b)
                t)
               ((not (equal a-id b-id))
                (log:info "move ~a points from ~a to ~a" (length (gethash b-id *clusters*))
                          b-id a-id)
                (dolist (p (gethash b-id *clusters*))
                  (setf (point-circuit-id p) a-id)
                  (pushnew p (gethash a-id *clusters*))
                  )
                (remhash b-id *clusters*)
                )

               (t
                (log:error "no logic with " a b)
                (error "oopsy logic")))

             ;; part 2
             (when (and (equal 1 (length (hash-table-keys *clusters*)))
                        (equal total (length (gethash (first (hash-table-keys *clusters*))
                                                      *clusters*))))
               (log:warn "the end with " points)
               (return-from group-pairs (reduce #'* points :key #'point-x)))
          )
    *clusters*))

(defun part1 (&optional (input *input* input-p))
  (group-pairs (take-n (sort-distances (distances (parse-input input)))
                       (if input-p 1000 10)
                       ))
  (reduce #'* (serapeum:take 3
                             (sort (loop for v being the hash-value of *clusters*
                                         collect (length v))
                                   #'>)))
  )

#+ciel
(log:config :warn)

#+ciel
(part1 (str:from-file "day08.txt"))

;; 79560
;; o/
;; 500ms :/

;;;;;;;;;;;;;;;;;;;;;
;; part 2

;; todo:

(defun part2 (input)
  ;; ugh global vars, sorry.
  (setf *points* (parse-input input))
  (group-pairs (sort-distances (distances (parse-input input)))))

#++
(part2 *input*)
;; 25272
;; OK

#+ciel
(part2 (str:from-file "day08.txt"))
;; 31182420
;; o/
