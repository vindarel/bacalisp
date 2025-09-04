
;; thanks https://github.com/werelax/consomail/blob/master/consomail.lisp

(defun enable-echo ()
  (uiop:run-program '("/bin/stty" "echo") :input :interactive :output :interactive))

(defun disable-echo ()
  (uiop:run-program '("/bin/stty" "-echo") :input :interactive :output :interactive))
  ;; or:
  ;; (sb-ext:run-program "/bin/stty" '("-echo") :input t :output t))


(defun prompt (msg)
  (format t msg)
  (force-output)
  (read-line))

(defun pass-prompt ()
  (format t "password: ")
  (force-output)
  (disable-echo)
  (unwind-protect
      (read-line)
    (enable-echo)))

(eval-when (:execute)
  (let ((pass (pass-prompt)))
    (format t "~&I still know the pass was ~s~&" pass)))
