(defpackage :server
     (:use :cl)
     (:export))

(in-package :server)

(defvar *my-acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4000
                                     :document-root #p"public/"))

(defun run()
  (format t "~aStarting my test app~&")
  (hunchentoot:start *my-acceptor*))

;; (run)
