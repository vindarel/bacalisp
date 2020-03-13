(in-package #:asdf-user)

(defsystem :pisi
  :author "vindarel <vindarel@mailz.org>"
  :maintainer "vindarel <vindarel@mailz.org>"
  :license ""
  :version "0"
  :homepage ""
  :bug-tracker ""
  :source-control (:git "")
  :description ""
  :depends-on (:cl-ppcre
               :cl-ppcre-unicode
               :str
               :pythonic-string-reader)
  :components ((:file "pisi"))

  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op :pisi.test))))
