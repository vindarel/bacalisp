(eval-when (:compile-toplevel :load-toplevel :execute)
    (ql:quickload "iup"))

(defpackage :iup-quicklisp
  (:use :cl))

(in-package :iup-quicklisp)

(defparameter *quicklisp-dist* nil)

(defparameter *quicklisp-systems* '())

(defun quicklisp-systems ()
  (or (and *quicklisp-systems*
           (subseq *quicklisp-systems* 0 200))
      (let ((dist (or *quicklisp-dist*
                      (setf *quicklisp-dist* (ql-dist:find-dist "quicklisp")))))
        (setf *quicklisp-systems* (ql:provided-systems dist)))))

(defun message (message)
  (iup:message "Callback Example" message))

(defun iuplist ()
  ;; list all quicklisp systems...
  (iup:with-iup ()
    (let* ((ok-button (iup:button :title "Install"
                                  :expand :yes
                                  :tip "Install this system"
                                  :action (lambda (handle)
                                            (declare (ignorable handle))
                                            (message "Apply?")
                                            iup:+default+)))
           (frame (iup:frame
                   (iup:scroll-box
                    (iup:vbox
                     (loop for list in (list
                                        (iup:list :value 3 :tip "List 3" :editbox :yes))
                        do (loop for system in (quicklisp-systems)
                              for i from 1
                              do (setf (iup:attribute list i)
                                       (format nil " ~A" (ql-dist:name system))))
                        collect list)))
                   :title "Quicklisp systems"))
           (dialog (iup:dialog frame :menu "menu" :title "Quicklisp systems")))
      (iup:map dialog)
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (iuplist)
#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (iuplist))
