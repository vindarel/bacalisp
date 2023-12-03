;; with ciel-user
;; or
;; with quicklisp libraries:
;; str, alexandria, serapeum's dict

(defparameter *file-input* "2023-12-03.txt")

(defparameter *input-lines* (str:lines (str:from-file *file-input*)))
#+(or)
(defparameter *input-lines* (str:lines input)))


(defparameter input "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..")

(defun symbol-p (i j)
  (when (or (< i 0)
            (< j 0)
            (<= (length *input-lines*) j)
            (<= (length (first *input-lines*)) i))
    (return-from symbol-p nil))
  (let* ((line (elt *input-lines* j))
         (char (elt line i)))
    (and (not (digit-char-p char))
         (not (equal #\. char)))))

(defun number-p (i j)
  (when (or (< i 0)
            (< j 0)
            (<= (length *input-lines*) j)
            (<= (length (first *input-lines*)) i))
    (return-from number-p nil))
  (let* ((line (elt *input-lines* j))
         (char (elt line i)))
    (digit-char-p char)))

(defun position-char (i j)
  (let* ((line (elt *input-lines* j))
         (char (elt line i)))
    char))

(defun keep-number-p (i j &key (recurse t))
    (or
     ;; right: is a number AND is connected.
     (and
      (number-p (1+ i) j)
      (when recurse
        (keep-number-p (1+ i) j)))
     ;; a symbol on its left.
     (symbol-p (1- i) j)
     ;; a symbol on the left-up corner
     (symbol-p (1- i) (1- j))
     ;; on top
     (symbol-p i (1- j))
     ;; top right
     (symbol-p (1+ i) (1- j))
     ;; right: symbol
     (symbol-p (1+ i) j)
     ;; bottom right
     (symbol-p (1+ i) (1+ j))
     ;; bottom
     (symbol-p i (1+ j))
     ;; bottom left
     (symbol-p (1- i) (1+ j))
     ;; left: is a number AND is connected
     (and
      (number-p (1- i) j)
      (keep-number-p (1- i) j :recurse nil))
     ))


(defun collect-valid-digits (input)
  ;; this collects also periods and symbols.
  (flatten
  (loop for line in (str:lines input)
        for j = 0 then (incf j)
        collect
           (loop for char across line
                 for i = 0 then (incf i)
                 if  (keep-number-p i j)
                   collect char))))

(defun sum-numbers-from-chars (chars)
  "Read digits, transform to numbers, sum."
  (loop for char in chars
      for digit = (digit-char-p char)
      with multiplier = 1 ;; then 10
      with nb = 0
      with total = 0
      if (digit-char-p char)
        do (setf multiplier 10)
           (setf nb (* nb multiplier))
           (incf nb digit)
      else
        do (setf multiplier 1)
           (incf total nb)
           (setf nb 0)
       finally (return total))
)

#+(or)
(sum-numbers-from-chars (collect-valid-digits input))
;; 4361

#+(or)
(sum-numbers-from-chars (collect-valid-digits (str:from-file *file-input*)))
