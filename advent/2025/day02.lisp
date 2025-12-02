
(uiop:define-package :aoc-2025-02
    (:use :cl
     :ciel  ;; for libraries: str and serapeum:dict and macro ^ (lambda shortcut)
          ;; CIEL also makes it easy to run a .lisp file as a script from the terminal
          ;; (with simpler CLI args, and all CIEL libraries baked in, so fast start-up times.
          ;; See below.)
     ))

(in-package :aoc-2025-02)

(defparameter *input* "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

(defun parse-input (input)
  "Return: list of dash-separated ranges (strings)."
  (str:split "," input))

(--> simple-invalid-id (string) boolean)
(defun simple-invalid-id (start &aux len-start)
  (declare (inline simple-invalid-id))
  (check-type start string)
  (setf len-start (length start))
  (when (evenp len-start)
    (let ((a (subseq start 0 (/ len-start 2)))
          (b (subseq start (/ len-start 2))))
      (equal a b))))

(defvar *part2* nil
  "when T, use another predicate in find-next-invalids")

(defun find-next-invalids (start end)
  "Try higher numbers… nothing more clever?"
  (loop for x :from (1+ (parse-integer start))
          :to (1- (parse-integer end))
        :for s = (princ-to-string x)
        :when (if (not *part2*)
                  (simple-invalid-id s)
                  (repeated-sequences s))
          :collect s))


;; (--> find-invalid-ids (string) (or null cons))
(defun find-invalid-ids (range &aux start end len-end (res (list)))
  "range: string-string"
  (check-type range string)
  (let ((ab (str:split "-" range)))
    (setf start (first ab))
    (setf end (second ab))
    (setf len-end (length end)))

  (when (simple-invalid-id start)
      ;; simple case like "11"
      (push start res))

  (when (simple-invalid-id end)
      (push end res))

  (when-let ((next (find-next-invalids start end)))
    (push next res))

  (flatten res))


#+testit
(setf fiveam:*debug-on-error* t)

#+testit
(setf fiveam:*run-test-when-defined* t)

#+testit
(fiveam:def-test test-find-invalid-ids ()
  (fiveam:is (equal 2
                    (length (find-invalid-ids "11-22"))))

  (fiveam:is (equal (list "99") (find-next-invalids "95" "115")))
  (fiveam:is (equal (list "99") (find-invalid-ids "95-115")))

  (fiveam:is (null (find-invalid-ids "1698522-1698528")))

  (fiveam:is (equal (list "1010") (find-invalid-ids "998-1012")))

  (fiveam:is (equal (list "446446") (find-invalid-ids "446443-446449")))
  (fiveam:is (equal (list "38593859") (find-invalid-ids "38593856-38593862")))

  (dolist (range (list "2121212118-2121212124" "824824821-824824827"))
    (fiveam:is (null (find-invalid-ids range))))
  )

(defun addup-string (&optional a b)
  (when (stringp a)
    (setf a (parse-integer a)))
  (cond
    ((and a b)
     (+ a (parse-integer b)))
    (a
     a)
    (t
     0)))

(defun part1 (input)
  (reduce #'addup-string (flatten
                          (mapcar #'find-invalid-ids (parse-input input)))))

;; 1227775554

#+ciel
(part1 (str:from-file "day02.txt"))
;; 35367539282 o/

;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;

;; I don't like today :(

(defun only-sequences-of (subs s)
  (null (remove-if #'str:blankp (str:split subs s))))

(defun repeated-sequences (s)
  (loop for i from 1 to (floor (length s) 2)
        for subs = (subseq s 0 i)
        when (only-sequences-of subs s)
          collect subs))

#+testit
(fiveam:def-test test-part-2-predicate ()
  (fiveam:is (equal (list "10")
                    (repeated-sequences "101010")))

  (fiveam:is (equal (list "99" "111") (find-next-invalids "95" "115")))

  (fiveam:is (equal (list "990" "1010") (find-next-invalids "998" "1012")))

  (fiveam:is (equal (list "824824824") (find-next-invalids "824824821" "824824827")))
  )

(defun part2 (input)
  (let ((*part2* t))
    (reduce #'addup-string (flatten
                            (mapcar #'find-invalid-ids (parse-input input))))))


#+ciel
(part2 (str:from-file *input*))

#+ciel
(part2 (str:from-file "day02.txt"))
;; 45814076230 o/
;; but
;; in ±20s. Stupid CPU power to the air lol.
