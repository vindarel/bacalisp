;; with libraries:
;; ciel
;; or
;; str, access, serapeum


(defparameter *file-input* "advent2023-12-08.txt")

(defparameter input "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)")

(defun parse-line (s)
  (ppcre:all-matches-as-strings "[A-Z]+" s))

(defun parse-input (input)
  (let* ((lines (str:lines input))
         (map (dict :instructions (first lines))))
    (loop for line in (subseq lines 2)
          for elts = (parse-line line)
          if (str:ends-with-p "A" (first elts))
            do (print (first elts))
          do (setf (gethash (first elts) map)
                   (cons (second elts)
                         (third elts))))
    map))

(defun solved-part1 (key)
  (equal "ZZZ" key))

(defun solved-part2 (key)
  (str:ends-with? "Z" key))

(defun walk (map &key (start "AAA") (solved-p #'solved-part1))
  (loop with instructions = (coerce (gethash :instructions map) 'list)
        ;; the trick: set the pointer of the last element of the list
        ;; to the list itself.
        with infinite-directions = (setf (cdr (last instructions)) instructions)
        with key = start
        for direction in infinite-directions
        for i from 0
        if (funcall solved-p key)
          return i
        else
          do (setf key (if (equal #\L direction)
                           (car (gethash key map))
                           (cdr (gethash key map))))
        ))

#+(or)
(walk (parse-input input))

#+solve-it
(walk (parse-input (str:from-file *file-input*)))

;; part 2

;; "JVA"
;; "XLA"
;; "DNA"
;; "AAA"
;; "SHA"
;; "DLA"
