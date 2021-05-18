
#+(or)
(ql:quickload '("hunchenissr" "markup" "log4cl"))

;; elisp side:
;; (add-to-list 'load-path "~/quicklisp/dists/quicklisp/software/markup-20201220-git/")
;; (require 'lisp-markup)

(defpackage :issr-test
  (:use :cl
        :markup)
  (:import-from #:hunchenissr
                define-easy-handler
                *id*
                *ws-port*
                start
                stop))

(in-package #:issr-test)

(markup:enable-reader)

(defparameter server
  (start (make-instance 'hunchentoot:easy-acceptor
                        :port 8080
                        :document-root "resources/")
         :ws-port 4433))

(defparameter *products* (list))

(defclass product ()
  ((title :initform ""
          :initarg :title
          :accessor title
          :type string)))

(defmethod print-object ((obj product) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a" (slot-value obj 'title))))

(defun create-products ()
  (loop for title in '("foo" "bar" "baz")
     collect (make-instance 'product :title title)))

(setf *products* (create-products))

(markup:deftag base-template (children &key title)
  ;; naming it simply "base" fails ?!
  <html>
    <head>
     <title>,(progn title)</title>
     <script src="/issr.js"></script>
     <script noupdate="t">
     ,(format nil "setup(~a,~a)" *id* *ws-port*)
     </script>
    </head>
    <body>
      ,@(progn children)
    </body>
  </html>)

#+(or)
;; use C-c C-p to eval and print the generated HTML in a buffer.
(markup:write-html
     <base-template title="Hello" >
        <h1>hello world!</h1>
     </base-template>)

(define-easy-handler (todo :uri "/products")
    (;; GET parameter names go here
     ;; They come from the input and button names.
     add-new-task
     new-task)
  (let
      ((add-new-task-p (and add-new-task
                            new-task
                            (string= add-new-task "add")
                            (not (str:emptyp
                                        ;XXX: str:blankp
                                  (str:trim new-task))))))
    (log:info add-new-task-p)
    (if add-new-task-p
      (print (setf *products* (append *products*
                                      (list (make-instance 'product :title new-task)))))
      ;;TODO: it's currently failing, I can't create a new task.
      ;; The client sends the GET parameters correctly.
      (log:info add-new-task new-task))
    (setf *products* (append *products*
                             (list (make-instance 'product :title "hacky"))))
    (format t "~& --- Products: ~s~&" *products*)

    (write-html
     <base-template title="My ISSR test" >
     <h1>Products List</h1>
     <ul>
     ,@(loop for product in (reverse *products*)
          collect
            <li>
            ,(progn (title product))
            </li>)
     </ul>

     <input name="new-task"
           placeholder="New task"/>
    <button action="add-new-task"
            value="add"
            onclick="rr(this)">
      Add
      </button>

      </base-template>)))
