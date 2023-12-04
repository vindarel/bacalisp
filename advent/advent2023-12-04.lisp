
(defparameter input " Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defparameter *file-input* "04.txt")

(defun parse-line (line)
  "Return a list: card id, winning numbers, our hand numbers."
  (let* ((card (first (str:split ":" line)))
         (id (parse-integer (str:replace-all "Card " "" card)))
         (winning-s (first (str:split "|" (second (str:split ":" line)))))
         (hand-s (second (str:split "|" (second (str:split ":" line))))))
    (list
     id
     (parse-numbers winning-s)
     (parse-numbers hand-s))))

(defun parse-numbers (s)
  (loop for char across (str:concat " " s " ")  ;; my algo needs whitespaces.
        for digit = (digit-char-p char)
        with res
        with multiplier = 1
        with nb = 0
        if digit
          do (setf nb (* nb multiplier))
             (incf nb digit)
             (setf multiplier 10)
        else
          do
          (setf multiplier 1)
          (unless (zerop nb) (push nb res))
          (setf nb 0)
        finally (return (reverse res))))
#+(or)
(parse-numbers " 41 48 ")

(defun hand-matches (hand)
  ;; also
  ;; (count-if (lambda (nb) (find nb (second hand))) (third hand))
  (loop for nb in (third hand)
        count (find nb (second hand))))

(defun game-worth (hand)
  (let ((matches (hand-matches hand)))
    (if (plusp matches)
        (expt 2 (1- matches))
        0)))
#+(or)
(progn
  (game-worth (parse-line (first (str:lines input))))
  (game-worth (parse-line (fifth (str:lines input)))))

(defun games-total (input)
  (apply #'+ (mapcar #'game-worth (mapcar #'parse-line (str:lines input)))))

(defun solve ()
  (games-total (str:from-file *file-input*)))

;; part 2

(defun hand-id (hand)
  (first hand))

(defun next-n-cards (id n)
  (loop for i from (1+ id) repeat n
        collect i))

(defun inc-copies (ids copies)
  (loop for id in ids
        for qty = (or (gethash id copies) 0)
        do (setf (gethash id copies)
                 (1+ qty))))

(defun populate-copies (input)
  (loop for hand in (mapcar #'parse-line (str:lines input))
      for matches = (hand-matches hand)
      for id = (hand-id hand)
      for new-cards = (next-n-cards id matches)
      with copies = (dict)
      do
         (inc-copies new-cards copies)
         (dotimes (n (or (gethash id copies) 0))
           (inc-copies new-cards copies))
        finally (return copies)))

(defun part2 (input)
  (+
   (apply #'+ (hash-table-values (populate-copies input)))
   (length (str:lines input))))

#+(or)
(part2 (str:from-file *file-input*))
