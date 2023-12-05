
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
  ;; just because I want to reuse this parsing loop,
  ;; otherwise use mapcar parse-integer on str:words
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

;; feedback:
;; (@ccQpein on Twitter)
#+(or)
(progn
  (ppcre:all-matches "\\d+" "1 2 3 49 98 71") ;; (0 1 2 3 4 5 6 8 9 11 12 14)
  (ppcre:all-matches-as-strings "\\d+" "1 2 3 49 98 71") ;; ("1" "2" "3" "49" "98" "71")
  ;; so:
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" "1 2 3 49 98 71")) ;; (1 2 3 49 98 71)
  )
;; oh-my-god^^

(defun hand-matches (hand)
  ;; also
  ;; (count-if (lambda (nb) (find nb (second hand))) (third hand))
  ;;
  ;; feedback:
  ;; length intersection
  ;; https://github.com/bo-tato/advent-of-code-2023/commit/d62eda2f2cdc38523efc39428cf4bceb2caf55da
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
        ;; feedback:
        ;; don't forget gethash's third argument: default value if not found.
        for qty = (gethash id copies 0) ;; instead of my original (or (gethash …) 0)
        do (setf (gethash id copies)
                 (1+ qty))))

(defun populate-copies (input)
  (loop for hand in (mapcar #'parse-line (str:lines input))
      for matches = (hand-matches hand)
      for id = (hand-id hand)
      for new-cards = (next-n-cards id matches)
      with copies = (dict)
      ;; feedback: an array instead of a hash-table:
      ;; (make-array num-cards :initial-element 1)
      do
         (inc-copies new-cards copies)
         (dotimes (n (gethash id copies 0))
           (inc-copies new-cards copies))
        finally (return copies)))

(defun part2 (input)
  (+
   ;; feedback:
   ;; (lispm) avoid apply because of CALL-ARGUMENTS-LIMIT
   ;; (a big number nonetheless)
   (apply #'+ (hash-table-values (populate-copies input)))
   (length (str:lines input))))

#+(or)
(part2 (str:from-file *file-input*))
