(uiop:define-package :aoc-2024-15
  (:use :cl
   :ciel  ;; for the libraries: cl-str, ppcre, alexandria (hash-table-values) and the ^ lambda shortcut.
   :defclass-std  ;; the one on lisp-maintainers, to have define-print-object/std
        )
  (:documentation "In this puzzle I used CLOS, to use the newly
   augmented DEFCLASS-STD shortcut. Using CLOS doesn't give much
   benefits for AOC or at least this puzzle IMO. We surely can have
   terser code by using a DICT or a simple data structure.

   Using methods to compute the gps coordinates is elegant though.

   Part 2 looks great^^ I'll see when I do it."))

(in-package :aoc-2024-15)

(defparameter *file-input* "input-day15.txt")

(defparameter *input* "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<")

(defparameter *larger-input* "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^")

(defvar *map* (dict))

(defvar *moves* (list))

(defvar *dimensions* (list))

(defun parse-input (input)
  (let ((pars (str:paragraphs input)))
    (setf *map* (parse-map (first pars))
          *moves* (parse-moves (second pars)))))

(defclass/std cell ()
  ((point)))

(defclass/std wall (cell) ())

(defclass/std empty (cell) ())

(defclass/std box (cell) ())

(defclass/std robot (cell) ())

(defvar *robot* nil "the robot position (point)")

(defun make-cell (x y)
  (make-instance 'cell :point (complex x y)))

(defmethod initialize-instance :after ((robot robot) &key)
  (with-slots (point) robot
    (setf *robot* point)))

(define-print-object/std cell)

(defun parse-map (s)
  (setf *map* (dict))
  (let* ((lines (str:lines s))
         (dimensions (list (length (first lines))
                           (length lines))))
    (setf *dimensions* dimensions)
    (loop for line in lines
          for y from 0
          do (loop for char across line
                   for x from 0
                   for point = (complex x y)
                   for type = (case char
                                (#\# 'wall)
                                (#\. 'empty )
                                (#\O 'box)
                                (#\@ 'robot)
                                (t (error "unknown cell type: ~a" char)))
                   do (setf (gethash point *map*) (make-instance type :point point))))
    (values *map* *robot*)))

(defun parse-moves (s)
  "S is the block of lines representing moves. Make it one line before parsing."
  (setf *moves* (loop :for char :across (ppcre:regex-replace-all "\\n" s "") :collect char)))

(defun robot ()
  (gethash *robot* *map*))

(defun (setf robot) (point)
  (setf *robot* point)
  (setf (gethash point *map*) (make-instance 'robot :point point)))

(defun in-bounds (x y)
  (and (< -1 x (first *dimensions*))
       (< -1 y (second *dimensions*))))

;; (defmethod in-map ((point list))
;;   (in-bounds (first point) (second point)))

(defmethod in-map ((x integer))
  "The case when the imagpart is 0 as in (complex 3 0) this complex turns into an integer."
  (in-bounds x 0))

(defmethod in-map ((p complex))
  (in-bounds (realpart p) (imagpart p)))

(defmethod in-map ((c cell))
  (in-map (point c)))

(defgeneric movable (cell direction)
  (:documentation "Is this cell object movable to this direction? Walls are NOT movable, then it depends if we are on a border.")
  (:method (cell direction)
    (when cell
      (in-map cell))))

(defmethod movable ((wall wall) direction)
  nil)

#++
(mapcar (^ (p) (movable p '<)) (hash-table-values *map*))

(defparameter *directions* (dict #\< -1
                                 #\> 1
                                 #\^ (complex 0 -1)
                                 #\v (complex 0 1)))

(defmethod next ((cell cell) direction)
  (gethash
   (+ (point cell) (gethash direction *directions*))
   *map*))

(defmethod collect-movable ((cell cell) direction &aux res)
  (push cell res)
  (loop for next = (next cell direction)
        unless next
          return res
        if (typep next 'wall)
          return nil
        if (typep next 'empty)
          return res
        if (and (in-map next)
                (movable cell direction))
          do (push next res)
        do (setf cell next)))

(defmethod move ((cell cell) direction)
  "This cell is supposed to be movable, since it was collected before."
  (let ((point (+ (point cell)
                  (gethash direction *directions*))))

    ;; Check bounds.
    (when (>= (realpart point) (first *dimensions*))
      (setf point (complex (1- (realpart point))
                           (imagpart point))))
    (when (minusp (realpart point))
      (setf point (complex (1+ (realpart point)) 0)))
    (when (>= (imagpart point) (second *dimensions*))
      (setf point (complex (realpart point)
                           (1- (imagpart point)))))
    (when (minusp (imagpart point))
      (setf point (complex (realpart point) 0)))

    ;; Change the map.
    (setf (point cell) point
          (gethash point *map*) cell)

    (when (typep cell 'robot)
      ;; (log:info "change *robot* pos")
      (setf *robot* point)
      (setf (gethash point *map*)
            (make-instance 'robot :point point)))))

(defmethod move :before ((cell robot) direction)
  (declare (ignore direction))
  ;; (log:info "the robot gives place to a block")
  (setf (gethash (point cell) *map*)
        (make-instance 'empty :point (point cell))))

(defmethod move :after ((cell robot) direction)
  (declare (ignore direction))
  ;; (log:info "new robot position: ~s" (point cell))
  (setf *robot* (point cell)))

(defun move-all (cells direction)
  (mapcar (^ (cell) (move cell direction)) cells))

(defun process-moves (moves)
  (loop for direction in moves
        for i from 0 to (length moves)
        for tomove = (collect-movable (robot) direction)
        do
           ;(log:info "step ~a, direction: ~a moving: x~a: ~a" i
            ;         direction (length tomove) tomove)
           (move-all tomove direction)))

(defun show (&optional (stream t))
  (loop for y below (second *dimensions*)
        do
           (loop for x below (first *dimensions*)
                 do (format stream "~a"
                            (typecase (gethash (complex x y) *map*)
                              (wall "#")
                              (robot "@")
                              (empty ".")
                              (box "O")
                              (t "X"))))
        (format stream "~&")))

(defmethod gps-coordinate ((point complex))
  (+ (* 100 (imagpart point))
     (realpart point)))

(defmethod gps-coordinate ((cell box))
  (gps-coordinate (point cell)))

(defmethod gps-coordinate ((cell cell))
  0)

(defun sum-coordinates (cells)
  (reduce #'+ (mapcar #'gps-coordinate cells)))

(defun part-1 (input)
  (parse-input input)
  (process-moves *moves*)
  (show)
  (sum-coordinates (hash-table-values *map*)))

#++
(part-1 *input*)
;; 2028

#++
(progn
  (part-1 *input*)
  (assert (equal "########
#....OO#
##.....#
#.....O#
#.#O@..#
#...O..#
#...O..#
########
"
                 (with-output-to-string (s)
                   (show s)))))

#++
(progn
  (part-1 *larger-input*)
  (assert (equal "##########
#.O.O.OOO#
#........#
#OO......#
#OO@.....#
#O#.....O#
#O.....OO#
#O.....OO#
#OO....OO#
##########
"
                 (with-output-to-string (s)
                   (show s)))))

#++
(part-1 (str:from-file *file-input*))
;; 1526018 o/
