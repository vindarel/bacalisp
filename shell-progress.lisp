#!/usr/bin/sbcl --script

(loop for percent in '(20 30 70 80) do
     (format t "~a~a[~a]"
             (make-string percent :initial-element #\>)
             (make-string (- 80 percent) :initial-element #\ )
             percent)
     (write-char #\return)
     (force-output)
     (sleep 1))
(write-char #\newline)

(loop for percent upto 80 do
     (format t "=-"
             )
     (write-char #\backspace)
     (force-output)
     (sleep 0.05))
(write-char #\newline)
