
(defun tty-p ()
  "Return T if we run on a terminal.
  This must fail on Slime (on Emacs' default shell prompt) and succeed on a Lisp in a terminal window."
  (let ((test (with-output-to-string (s)
                (uiop:run-program "echo [ -n \"$TERM\" ]" :output s))))
    (not (str:containsp "dumb" test))))
