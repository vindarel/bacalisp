
(uiop:define-package :aoc-2023-19
  (:use :cl :ciel))

;; needs str::match, unmerged as of <2024-01-27>

(in-package :aoc-2023-19)

(defparameter *file-input* "advent2023-12-19.txt")

(defparameter input "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}")

(defparameter workflows (dict "A" :accept
                        "R" :reject)
  "name -> rules (lambdas)")

(defparameter ratings (list)
  "List of objects.")

(defun workflow-start ()
  (gethash "in" workflows))

;; defstruct is shorter but its accessor being rating-s
;; it's more difficult to generate code using them, such as
;; (slot-value (symbolicate "X"))
;; in short, how to generate rating-s as a symbol?
;;
;; But structures are not cool to work with dynamically, they ask
;; confirmation to change their defition each time, objects are not
;; updated, our state is not updated.
(defclass rating ()
  ;; defclass/std for shorter class definition?
  ((x :initarg :x :initform nil)
   (m :initarg :m :initform nil)
   (a :initarg :a :initform nil)
   (s :initarg :s :initform nil)))

(defmethod print-object ((obj rating) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-slots (x m a s) obj
      (format stream "x:~a, m:~a, a:~a, s:~a" x m a s))))

(defun make-rating (&rest keys)
  (apply #'make-instance 'rating keys))

(defun parse-ratings (input)
  (->> input
    (ppcre:split "\\n\\n")
    second
    str:lines
    (mapcar #'parse-rating)))
#+(or)
(parse-ratings input)

(defun parse-rating (line)
  ;; it's always x m a s in order…
  ;; XMAS :)
  (let* ((xmas (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" line))))
    (make-rating :x (first xmas)
                 :m (second xmas)
                 :a (third xmas)
                 :s (fourth xmas))))
#+(or)
(parse-rating "{x=787,m=2655,a=1222,s=2876}")
;; #S(RATING :X 787 :M 2655 :A 1222 :S 2876)

(defun parse-workflows (input)
  (->> input
    (ppcre:split "\\n\\n")
    first
    str:lines
    (mapcar #'parse-workflow)))

(defun parse-workflow (line)
  (multiple-value-bind (name body)
      (str::match line
        ((name "{" body "}")
         (values name body))
        (t
         (error "unknown workflow format: ~a" line)))
    (let ((parts (str:split "," body)))
      (setf (gethash name workflows)
            (mapcar #'parse-part parts)))))
#+(or)
(parse-workflow "in{s<1351:px,qqz}")

(defun parse-part (s)
  "Create a rule, or straight redirect, or Accept or Reject."
  (cond
    ((equal s "A")
     ;; use conditions?
     :accept)
    ((equal s "R")
     :reject)
    ((str:containsp ":" s)
     (str::match s
       ((category "<" value ":" target)
        ;; générer des lambdas: rabbit hole ?
        (lambda (rating)
          ;; (when (< [rating's value] [our value] …))
          (when (< (slot-value rating (alexandria:symbolicate (str:upcase category)))
                   (parse-integer value))
            target)))
       ;; copy&paste:
       ((category ">" value ":" target)
        (lambda (rating)
          (when (> (slot-value rating (alexandria:symbolicate (str:upcase category)))
                   (parse-integer value))
            target)))
       (t
        (error "unknown part format: ~a" s))))
    ((not (str:containsp ":" s))
     ;; target
     s)
    (t
     (error "undecided part: ~a" s))))
#+(or)
 (parse-part "s<1351:px") ; => creates a lambda.
;; (funcall * (make-rating :s 3))

(defun apply-rule (rule rating)
  (if (functionp rule)
      (funcall rule rating)
      rule))

(defun apply-rules (rules rating)
  (loop for rule in rules
        for state = (apply-rule rule rating)
        do (cond
             ((equal "A" state)
              :accept)
             ((equal "R" state)
              :reject)
             ((stringp state)
              (return-from apply-rules state))
             ((null state)
              ;; continue loop, apply next rule.
              t)
             (t
              ;; :reject and :accept
              (return-from apply-rules state)))))

#+(or)
(apply-rules (workflow-start) (first ratings))
;; "qqz"


(defun chain-workflows (rules rating)
  (let ((state (apply-rules rules rating)))
    (cond
      ((stringp state)
       (chain-workflows (gethash state workflows) rating))
      ((equal :reject state)
       nil)
      ((equal :accept state)
       t)
      ((null state)
       (error "what is ~a" state)))))

(defun sum-rating (rating)
  (+ (slot-value rating 'x)
     (slot-value rating 'm)
     (slot-value rating 'a)
     (slot-value rating 's)))

;; with class introspection:
#+(or)
(mapcar (lambda (slot)
          (slot-value (first ratings) (sb-mop:slot-definition-name slot)))
        (sb-mop:class-direct-slots (class-of (first ratings))))
;; (787 2655 1222 2876)
;;
;; use closer-mop

(defun run-rating (rating)
  (let ((state (chain-workflows (workflow-start) rating)))
    (if state
        (sum-rating rating)
        0)))

(defun do-all-ratings (ratings)
  (reduce #'+ (mapcar #'run-rating ratings)))

(defun part1 (input)
  (let ((_ (parse-workflows input))  ;; it's stored in a global
        (ratings (parse-ratings input)))
    (declare (ignore _))
    (do-all-ratings ratings)))

#+(or)
(part1 input)
;; 19114

#+(or)
(part1 (str:from-file *file-input*))
;; 375719 nope :(
