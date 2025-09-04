
;; thanks https://github.com/werelax/consomail/blob/master/consomail.lisp

(defun disable-echo ()
  (sb-ext:run-program "/bin/stty" '("-echo") :input t :output t))

(defun enable-echo ()
  (sb-ext:run-program "/bin/stty" '("echo") :input t :output t))


(defun prompt (msg)
  (format t msg)
  (force-output)
  (read-line))


(defun pass-prompt ()
  (format t "password: ")
  (force-output)
  (disable-echo)
  (let ((pwd (read-line)))
    (enable-echo)
    pwd))

(let ((pass (pass-prompt)))
  (format t "~&I still know the pass was ~s~&" pass))
