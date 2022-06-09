
(unless (ql:quickload '("hunchentoot" "trivial-backtrace" "BORDEAUX-THREADS"))
  (uiop:quit 1))


(defvar *my-acceptor* nil)

(handler-bind ((error (lambda (c)
                        (format *error-output* "~&An error occured: ~a~&" c)
                        (format *error-output* "~&Backtrace: ~&")
                        (trivial-backtrace:print-backtrace c))))

  (progn

    (uiop:format! t "-------- create hunchentoot acceptor… ~&")
    (setf *my-acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4000
                                     :document-root #p"public/"))

    (uiop:format! t "-------- start app on port 4000… --------------~&")
    (hunchentoot:start *my-acceptor*))
    (uiop:format! t "-------- … and done. Now we have the REPL. --------------~&")


    (uiop:format! t "-------- put the server thread on the foreground~&")
    ;; otherwise the Lisp quits instantly.
    ;; systemd might even tell you there was a
    ;; "compilation unit aborted ; caught 1 fatal ERROR condition
    (bt:join-thread (find-if (lambda (th)
                               (search "hunchentoot" (bt:thread-name th)))
                             (bt:all-threads)))

  )
