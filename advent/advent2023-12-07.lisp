;; with libraries:
;; ciel
;; or
;; str, access, serapeum

(uiop:define-package :aoc-2023-07
  (:use :cl :ciel))

(in-package :aoc-2023-07)

(defparameter *file-input* "advent2023-12-07.txt")

(defparameter input "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483")

(defun parse-input (input)
  (mapcar #'str:words (str:lines input)))
;; (("32T3K" "765") ("T55J5" "684") ("KK677" "28") ("KTJJT" "220") ("QQQJA" "483"))

;; hand types:
;; 1 high-card
;; 2 one pair
;; 3 two pairs
;; 4 three of a kind (no pair)
;; 5 full house (three and pair)
;; 6 four of a kind
;; 7 five of a kind
(defun hand-strength (hand &key (slot :hand))
  (let ((uniq (remove-duplicates (access hand slot))))
    (case (length uniq)
      (1
       (setf (access hand :type-name) :five
             (access hand :type) 7))
      (2
       (let ((disposition (sort (loop for char across uniq
                                      collect (count char (access hand slot))) #'<)))
         (cond
           ((equal (list 1 3) disposition)
            (setf (access hand :type-name) :three-of-a-kind
                  (access hand :type) 4))
           ((equal (list 2 3) disposition)
            (setf (access hand :type-name) :full-house
                  (access hand :type) 5))
           ((equal (list 1 4) disposition)
            (setf (access hand :type-name) :four
                  (access hand :type) 6))
           (t (error "hand-strength issue with ~a uniq cards" uniq)))))
      (3
       (let ((disposition (sort (loop for char across uniq
                                      collect (count char (access hand slot))) #'<)))
         (cond
           ((equal (list 1 1 3) disposition)
            (setf (access hand :type-name) :three-of-a-kind
                  (access hand :type) 4))
           ((equal (list 1 2 2) disposition)
            (setf (access hand :type-name) :two-pairs
                  (access hand :type) 3))
           (t (error "hand-strength issue with ~a uniq cards" uniq)))))
      (4
       (setf (access hand :type-name) :pair
             (access hand :type) 2))
      (5
       (setf (access hand :type-name) :highest-card
             (access hand :type) 1))
      (t (error "hand-strength unknown combinaison: ~a uniq cards" (length uniq))))
    hand))


#+(or)
(progn
  (hand-strength (first (parse-hands input)))
  (hand-strength (dict :hand "3TTTT" :type 1))
  (hand-strength (dict :hand "TTTTT" :type 1))
  ;; TT33T
  (equal :full-house
         (access
          (hand-strength (dict :hand "TT33T" :type 1))
          :type-name))
  (equal :full-house
         (access
          (hand-strength (dict :hand "QQQJJ" :type 1))
          :type-name))
  )

(defun make-hand (pair)
  (dict :hand (first pair)
        :bid (parse-integer (second pair))
        :type 1
        :type-name :highest-card
        ))
#+(or)
;; example:
(dict
  :HAND "55585"
  :BID 18
  :TYPE 6
  :TYPE-NAME :FOUR
 )

(defun parse-hands (input)
  (mapcar (lambda (pair)
            (hand-strength (make-hand pair))) ;; hand-strength returns a hand.
          (parse-input input)))

(defparameter *heads* '(#\T #\J #\Q #\K #\A)
  "Heads order.")

(defun head< (h h2)
  (< (position h *heads*) (position h2 *heads*)))
#+(or)
(progn
  (head< #\T #\K)
  (head< #\K #\T)
)

(defun compare-hands (it that &key (test #'card<))
  (if (not (= (access it :type)
              (access that :type)))
      (< (access it :type) (access that :type))
      ;; Compare cards.
      (loop for i from 0 upto (1- (length (access it :hand)))
            for c = (elt (access it :hand) i)
            for c2 = (elt (access that :hand) i)
            for c-lower = (funcall test c c2)
            for foo = (log:debug i c c2 (card< c c2))
            if (not (equal c c2))
              return c-lower
            )))

(defun sort-hands (hands &key (test #'compare-hands))
  (stable-sort (copy-seq hands) test))

(defun total-winnings (sorted-hands)
  (loop for hand in sorted-hands
        for rank from 1
        sum (* rank (access hand :bid))))

#+devel
(defun reset (&optional (input input))
  (setf hands (parse-hands input)))

;; change log config:
;; (log:config :info)

#+solve-it
(total-winnings (sort-hands (parse-hands (str:from-file *file-input*))))

;; part 2

(defun card<-joker-lower (c c2)
  "Compare two cards (characters), digits or heads,
  J is always the lowest value."
  (cond
    ((equal #\J c)
     t)
    ((equal #\J c2)
     nil)
    (t
     (card< c c2))))
;; (CARD<-JOKER-FIRST #\J #\Q)
;; T

(defun pairs<-joker-first (c c2)
  "This time work on a cons cell with card character, occurences."
  (cond
    ;; let's put the J in front:
    ((equal #\J (car c))
     nil)
    ((equal #\J (car c2))
     t)
    (t
     ;; Sort on number of occurences.
     (< (cdr c) (cdr c2)))))

(defun hand-repartition (s)
  "Return a list of pairs: character, nb of occurences,
  but with the joker always first."
  (let ((uniq (remove-duplicates s)))
    (reverse
     (sort (loop for char across uniq
                 collect (cons char (count char s)))
           #'pairs<-joker-first))))
#+(or)
(hand-repartition "T55J5")
;; ((#\J . 1) (#\5 . 3) (#\T . 1))
;; (hand-repartition "KK677")

(defun hand-strength/part2 (hand)
  (let* ((new-hand (access hand :hand))
         (repartition (hand-repartition new-hand))
         (second-best (or (car (second repartition))
                          ;; When we have only Js !
                          #\A)))
    ;; Jokers?
    (when (equal #\J (car (first repartition)))
      (log:debug "joker! second best card ~a" (second repartition))
      (setf new-hand
            (str:replace-all "J" second-best new-hand)))

    ;; Always record new-hand slot for part2.
    (setf (access hand :new-hand)
          (str:replace-all "J" second-best new-hand))

    ;; Normal strength lookup, but on our new field:
    (hand-strength hand :slot :new-hand)
    ))

(defun parse-hands/part2 (input)
  (mapcar (lambda (pair)
            (hand-strength/part2 (make-hand pair)))
          (parse-input input)))

(defun compare-hands/part2 (it that)
  (compare-hands it that :test #'card<-joker-lower))

(defun sort-hands/part2 (hands)
  (stable-sort (copy-seq hands) #'compare-hands/part2))

#+(or)
(total-winnings (sort-hands/part2 (parse-hands/part2 input)))

#+solve-it
(total-winnings (sort-hands/part2 (parse-hands/part2 (str:from-file *file-input*))))
;; 251224870 \o/
