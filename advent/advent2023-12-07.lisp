;; with libraries:
;; ciel-user
;; or
;; ppcre, str, arrow-macros

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
(defun hand-strength (hand)
  (let ((uniq (remove-duplicates (access hand :hand))))
    (case (length uniq)
      (1
       (setf (access hand :type-name) :five
             (access hand :type) 7))
      (2
       (let ((disposition (sort (loop for char across uniq
                                      collect (count char (access hand :hand))) #'<)))
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
                                      collect (count char (access hand :hand))) #'<)))
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
      (t (error "hand-strength unknown combinaison")))
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
        :type 1  ;; high card: minimum.
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

(defun card< (c c2)
  "Compare two cards (characters), digits or heads."
  (let ((c-digit (digit-char-p c))
        (c2-digit (digit-char-p c2)))
    (cond
      ((and c-digit c2-digit)
       (< c-digit c2-digit))
      ((and c-digit (not c2-digit))
       t)
      ((and (not c-digit) c2-digit)
       nil)
      (t
       (head< c c2)))))

(defun compare-hands (it that)
  (if (not (= (access it :type)
              (access that :type)))
      (< (access it :type) (access that :type))
      ;; Compare cards.
      (loop for i from 0 upto (1- (length (access it :hand)))
            for c = (elt (access it :hand) i)
            for c2 = (elt (access that :hand) i)
            for c-lower = (card< c c2)
            for foo = (log:debug i c c2 (card< c c2))
            if (not (equal c c2))
              return c-lower
            )))

(defun sort-hands (hands)
  (stable-sort (copy-seq hands) #'compare-hands))

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
