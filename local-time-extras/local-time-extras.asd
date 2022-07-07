

(defsystem "local-time-extras"
  :version "0.1"
  :license "BSD"
  :author ""
  :description "More functions for time manipulation that we add in the local-time package."
  :depends-on (:local-time)
  ;; :in-order-to ((test-op (test-op "local-time/test")))
  :components ((:file "local-time-extras")))
