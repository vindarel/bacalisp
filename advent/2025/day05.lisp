(uiop:define-package :aoc-2025-05
    (:use :cl
     :ciel  ;; for libraries: str, serapeum:dict and more useful ones.
          ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
          ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
          ;; See below.)
          ;; http://ciel-lang.org/
          )
  (:documentation "ranges -> writing some utilities to create and merge ranges. That's the right approach I believe :)"))

(in-package :aoc-2025-05)

(defparameter *input* "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

(defstruct range
  start
  end)
;; => gives the make-range constructor and copy-range functions.

(defun mk-range (start end)
  (make-range :start start :end end))

(defun make-exclusive-range (start end)
  (make-range :start (1+ start) :end (1- end)))

(defun to-range (s)
  (let ((bounds (str:split "-" s)))
    (make-range :start (parse-integer (first bounds))
                :end (parse-integer (second bounds)))))

(defun parse-input (input)
  (let ((parts (str:paragraphs input)))
    (list (mapcar #'to-range (str:lines (first parts)))
          (mapcar #'parse-integer (str:words (second parts))))))

(defun in-range-p (n range)
  (and (<= (range-start range) n)
       (>= (range-end range) n)))

(defun valid-ingredient-p (id ranges)
  (loop for range in ranges
        thereis (in-range-p id range)))

(defun count-valid-ingredients (ids ranges)
  (loop for id in ids
        count (valid-ingredient-p id ranges)))

(defun part1 (input)
  (let ((ranges/ids (parse-input input)))
    (count-valid-ingredients (second ranges/ids) (first ranges/ids))))

#++
(part1 *input*)
;; 1 us

#+ciel
(part1 (str:from-file "day05.txt"))
;; 896 o/
;; 11ms

;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;

(defun sort-ranges (ranges)
  (sort (copy-list ranges) #'<= :key #'range-start))

#++
(defparameter *ranges* (first (parse-input *input*)) "devel only")

(defun mega-ranges (ranges)
  (let ((current (copy-range (first ranges)))
        (superranges nil))
    (loop for range in (rest ranges)
          do (cond
               ((in-range-p (range-start range) current)
                 ;; extend
                (when (not (in-range-p (range-end range) current))
                  (setf (range-end current) (range-end range))))
               (t
                ;; create new super range
                ;; and save previous super range.
                (push current superranges)
                (setf current (copy-range range))))
          finally (push current superranges))
    (reverse superranges)))

#++
(defparameter *megaranges* (mega-ranges (sort-ranges *ranges*)) "devel only")

(defun range-length (range)
  (1+
   (- (range-end range)
      (range-start range))))

(defun sum-ranges-length (ranges)
  (reduce #'+ (mapcar #'range-length ranges)))

(defun part2 (input)
  (sum-ranges-length (mega-ranges (sort-ranges (first (parse-input input))))))

#++
(part2 *input*)

#+ciel
(part2 (str:from-file "day05.txt"))
;; 346240317247002
;; at first try o/
;; in 11ms
;; thank you so much for not puting difficult hedge cases!
