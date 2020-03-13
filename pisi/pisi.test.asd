(in-package #:asdf-user)

(defsystem :pisi.test
  :author "vindarel <vindarel@mailz.org>"
  :maintainer "vindarel <vindarel@mailz.org>"
  :license ""
  :description "Test suite."
  :depends-on (:rove)
  ;; :defsystem-depends-on (:prove-asdf)
  :components ()

  :perform (test-op (op system)
             (funcall (read-from-string "rove:run") ;TODO:
                      (system-relative-pathname :pisi.test "test/"))))
