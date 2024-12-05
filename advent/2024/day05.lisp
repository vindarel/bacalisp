(defpackage :aoc-2024-05
  (:use :cl
   :ciel))

(in-package :aoc-2024-05)

(defparameter *file-input* "input-day05.txt")

(defparameter *input* "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defparameter *rules* "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13")

(defparameter *pages* "75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(defun parse-rules (input)
  (mapcar (^ (list)
             (mapcar #'parse-integer list))
          (mapcar (serapeum:partial #'ppcre:all-matches-as-strings "\\d+") (str:lines input))))

(defun parse-pages (input)
  (mapcar (^ (list)
             (mapcar #'parse-integer list))
          (mapcar (serapeum:partial #'ppcre:all-matches-as-strings "\\d+") (str:lines input))))

(defun line-ordered-p (line rules)
  (when (stringp line)
    (setf line (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" line))))
  (loop for (a b) in rules
        for pos-a = (or (position a line) )
        for pos-b = (or (position b line) )
        if (and (and pos-a pos-b)
                (not (< pos-a pos-b)))
          return nil
        finally (return line)))

(defun collect-ordered (pages rules)
  (loop for line in pages
        when (line-ordered-p line rules)
          collect line))

(defun part-1 (input)
  ;; ONGOING: str:paragraphs it's just ppcre:split "\\d+" line
  (let* ((pages (parse-pages (second (str::paragraphs input))))
         (rules (parse-rules (first (str::paragraphs input))))
         (lines (collect-ordered pages rules)))
    (loop for line in lines
          for length = (length line)
          sum (elt line (/ (1- length) 2)))))

#+ciel
(part-1 (str:from-file *file-input*))
;; 5452 o/

(defun parse-line (s)
  (mapcar #'parse-integer (ppcre:all-matches-as-strings "\\d+" s)))

(defun sort-line (line &optional (rules *rules*))
  (when (stringp line)
    (setf line (parse-line line)))
  (sort line (^ (a b)
                (search (format nil "~a|~a" a b) rules :test #'equal))))

(defun collect-ordered/2 (pages rules)
  "Return two values: ordered, non-ordered."
  (loop for line in pages
        if (line-ordered-p line rules)
          collect line into ordered
        else
          collect line into non-ordered
        finally
        (return (values ordered non-ordered))))

(defun part-2 (input)
  ;; ONGOING: str:paragraphs it's just ppcre:split "\\d+" line
  (let* ((rules-text (first (str::paragraphs input)))
         (rules (parse-rules rules-text))
         (pages (parse-pages (second (str::paragraphs input)))))
    (multiple-value-bind (ordered non-ordered)
        (collect-ordered/2 pages rules)
      (declare (ignore ordered))
      (loop for toorder in non-ordered
            for line = (sort-line toorder rules-text)
            for length = (length line)
            sum (elt line (/ (1- length) 2))))))

#+ciel
(part-2 (str:from-file *file-input*))
;; 4598 o/
