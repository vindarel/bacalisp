
(uiop:define-package :aoc-2023-13
  (:use :cl :ciel))

(in-package :aoc-2023-11)

(defparameter *file-input* "advent2023-12-13.txt")

;; That's 2 patterns:
(defparameter input "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#")

(defun input-patterns (input)
  (ppcre:split "\\n\\n" input))

(defun parse-line (line)
  "Represent the line with 1s and 0s"
  ;; do bitwise?
  (loop for char across (reverse line)
        for multiplier = 1 then (setf multiplier (* 10 multiplier))
        sum (* multiplier (if (equal #\# char) 1 0))))
#+(or)
(parse-line "#..#.")
;; 10010

;; Same logic for horizontal or vertical reflections.
(defun inverse-grid (lines)
  (loop for j from 0 below (length (first lines))
        collect (loop for line in lines
                      collect (elt line j) into res
                      finally (return (concatenate 'string res)))))

(defun inverse-pattern (pattern)
  (str:join #\Newline (inverse-grid (str:lines pattern))))

(defun parse-pattern (pattern)
  (mapcar #'parse-line (str:lines pattern)))

(defun is-symetric (mark &key (hashes *hashes*))
  (when (< 0 mark (length hashes))
    (loop for i from 0
          until (or (zerop (- mark i))
                    (= (length hashes) (+ mark i)))
          always (equal (elt hashes (- (1- mark) i))
                        (elt hashes (+ mark i))))))

(defun find-symetry (pattern)
  (let ((hashes (parse-pattern pattern)))
    (loop for hash in hashes
          and prev = 0 then hash
          for i from 0
          if (and (= prev hash)
                  (is-symetric i :hashes hashes))
            return i)))

(defun count-simple (nb)
  (or nb 0))

(defun count*100 (nb)
  (if nb (* 100 nb) 0))

#+(or)
(-> input
  input-patterns
  first
  inverse-pattern
  find-symetry
  count-simple
  )

(defun summarize-pattern (pattern)
  (let ((int (-> pattern
               find-symetry
               count*100)))
    (case int
      (0 (-> pattern
           inverse-pattern
           find-symetry
           count-simple))
      (t
       int))))

(defun summarize-notes (input)
  (reduce #'+ (mapcar #'summarize-pattern (input-patterns input))))

#+ciel
(summarize-notes (str:from-file *file-input*))
;; 3725 \o/
;; So there is only one reflection line per pattern. They could have been sniky.
