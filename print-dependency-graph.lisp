
;; https://gist.github.com/svetlyak40wt/a8ed639bf8fe07caed1531611bcf932d
(defun print-dependency-graph (system-name &key (level 0))
  (loop repeat level
        do (format t "  "))
  (format t "~A~%" system-name)
  (typecase system-name
    ((or string symbol)
     (let ((system (asdf/system:find-system system-name)))
       (loop for dep in (asdf/system:system-depends-on system)
              do (print-dependency-graph dep :level (1+ level)))))))
