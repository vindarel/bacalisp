(defpackage testtable
  (:use #:cl
        #:weblocks-ui/form
        #:weblocks/html)
  (:import-from #:weblocks/widget
                #:render
                #:update
                #:defwidget)
  (:import-from #:weblocks/actions
                #:make-js-action)
  (:import-from #:weblocks/app
                #:defapp)
  (:import-from #:weblocks-navigation-widget
                #:defroutes)
  (:documentation "Reproducing an update bug with a table (see Weblocks issues).")

(in-package :testtable)

(defapp testtable :prefix "/")

(weblocks/debug:on)

(defparameter *quantity* 0 "Simulate a book's quantity in the DB.")

;; (defwidget book-widget (weblocks-ui:ui-widget)
(defwidget book-widget ()
  ())

(defun add-book (book-widget)
  "Add one copy to the default place."
  (incf *quantity*)
  (update book-widget))

(defmethod render ((widget book-widget))
  (with-html
    (:td "Title: On Lisp")
    (:td "quantity:" *quantity*)
    (:td (with-html-form (:POST (lambda (&key &allow-other-keys)
                                  (add-book widget)))
           (:input :type "submit"
                   :value "+ 1")))))

(defwidget book-list-widget ()
  ())

(defun interstr (string &rest rest)
  (let ((strings (cons string rest)))
    (loop for string in strings
       do (if (str:contains? "~" string)
              (print :interpo!)))))

(defmacro istr (&body body)
  (when (str:contains? "~" (first body))
    (print :yes))
  `(print ,@body))

(istr "rst" "val is ~{val}")

(defmethod render ((widget book-list-widget))
  (with-html
    (:table
     ;; (:tbody
      (dolist (elt (list (make-instance 'book-widget)
                         (make-instance 'book-widget)))
         do (with-html
              (:tr (render elt)))))))

(defmethod weblocks/session:init ((app testtable))
  (declare (ignorable app))
  (make-instance 'book-list-widget))

(defun start ()
  (weblocks/server:start :port 8910))
