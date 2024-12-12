(uiop:define-package :aoc-2024-11
  (:use :cl
   :ciel  ;; for the libraries: cl-str serapeum (dict)
   )
  (:import-from :fare-memoization
                :define-memo-function))

(in-package :aoc-2024-11)

;;;
;;; simple recursive algorithm,
;;; but I struggled to find the right way to cache results.
;;; I finally use a third-party library for memoization and it helped.
;;; Computing result takes ms.
;;;

(defparameter *file-input* "input-day11.txt")

(defparameter *input* "125 17")

(defparameter *memo* (dict) "nb -> final result")

(defun parse-input (input)
  (mapcar #'parse-integer (str:words input)))

(defun rules (nb)
  (cond
    ((zerop nb)
     1)
    ((evenp (length (princ-to-string nb)))
     (let* ((s (princ-to-string nb))
            (l (length s))
            (end (/ l 2)))
       (list (parse-integer (subseq s 0 end))
             (parse-integer (subseq s end)))))
    (t
     (* 2024 nb))))

(defvar *stones* 0)

(defparameter *nb-blinks* 1)
(defparameter *nb-blinks* 6)
(defparameter *nb-blinks* 25)

;; USAGE: I thougth setf *memo* to a new dict would clear up the cache,
;; but only changing the function signature does?
(define-memo-function (blink-on :table (dict)) (nb &key (blink *nb-blinks*))
  (when (= blink 0)
    (return-from blink-on 1))

  (let ((res (uiop:ensure-list (rules nb))))
    ;; (log:info "blink ~a: ~a -> ~a" blink nb res)
    (+ (blink-on (first res) :blink (1- blink))
       (if (second res)
           (blink-on (second res) :blink (1- blink))
           0))
    ))

(defun all-blinks (input &key (blink *nb-blinks*))
  (reduce #'+ (mapcar #'blink-on (parse-input input))))

(defun all-blinks/reset (input)
  (setf *memo* (dict))
  (all-blinks input))

(defun part-1 (input)
  (all-blinks/reset input))

#++
(part-1 (str:from-file *file-input*))
;; 203228 o/
;; 0.002564 seconds

(defparameter *nb-blinks* 75)
;; part 2
#++
(time (all-blinks/reset (str:from-file *file-input*)))
;; 240884656550923 o/
;; 0.135626 seconds of total run time (0.135626 user, 0.000000 system)
