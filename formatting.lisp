;; (in-package :str)

;; See cl-ansi-term with more stuff.

(defun as-title (str &key (filler #\=) (stream t))
  "Print with an underline of the same length.
  The underline character is '=' by default. Change with `filler' (string or character)."
  (unless (characterp filler)
    (if (= 1 (length filler))
        (setf filler (coerce filler 'character))
        (error "The filler '~a' should be a character or a string of one character." filler)))
  (format stream "~a~&~a" str (make-string (length str)
                                           :initial-element filler)))

#|
(as-title "Hello")

Hello
=====

|#

(defun as-subtitle (str &key (filler #\-) (stream t))
  (as-title str :filler filler :stream stream))

(defun fill-line (length &key (filler #\-) (stream t))
  (unless (characterp filler)
    (if (= 1 (length filler))
        (setf filler (coerce filler 'character))
        (error "The filler '~a' should be a character or a string of one character." filler)))
  (format stream "~a" (make-string length :initial-element filler)))


(defun as-boxed (str &key (stream t) (filler #\-) (min-width 80))
  ;TODO: wrap long lines. Bobbin? Code for cl-ansi-term?
  (let* ((str-length (max (length str)
                          min-width))
         (top-bottom-line (format nil "+~a~a~a+~&"
                                  filler
                                  (fill-line str-length :stream nil :filler filler)
                                  filler))
         (min-width (max min-width
                         (length str))))
    (format stream "~a" top-bottom-line)
    (format stream "| ~a~a |~&" str (fill-line (- min-width (length str))
                                               :filler " "
                                               :stream nil))
    (format stream "~a" top-bottom-line)))

#|
+----------------------------------------------------------------------------------+
| Hello                                                                            |
+----------------------------------------------------------------------------------+
|#

(defun as-section (str &key (stream t) (filler #\#))
  (format stream "~a~&" filler)
  ;TODO: wrap long strings.
  (format stream "~a ~a~&" filler str)
  (format stream "~a~&" filler))

#|

#
# Hello
#

|#
