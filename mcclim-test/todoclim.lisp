
;;
;; http://turtleware.eu/posts/You-dont-need-a-backend-to-use-CLIM.html
;;

#+(or)
(ql:quickload '("mcclim" "alexandria" "hunchentoot" "local-time"))

(defpackage #:eu.turtleware.dimwit
           (:use #:clim-lisp)
           (:local-nicknames (#:m #:clim)
                             (#:e #:clim-extensions)
                             (#:b #:clim-backend)
                             (#:a #:alexandria)
                             (#:t #:local-time)
                             (#:h #:hunchentoot)))

(in-package #:eu.turtleware.dimwit)

(defun simple-repl (ct &optional (prompt (m:command-table-name ct))
                    &aux (* nil))
  (labels ((list-commands ()
             (format t "Commands in ~a:~%" ct)
             (m:map-over-command-table-commands #'print ct))
           (assert-command (command)
             (when (eq command 'help)
               (list-commands)
               (return-from assert-command `(climi::com-null-command)))
             (let ((name (car command)))
               (unless (symbolp name)
                 (error "~a does not designate a command." name))
               (unless (m:command-accessible-in-command-table-p name ct)
                 (error "Command ~a not found." name))
               command)))
    (loop
      (format t "~&~a> " prompt)
      (handler-case
          (let* ((cmd (assert-command (read)))
                 (results (multiple-value-list (eval cmd))))
            (format t "~&---~%~{~a~%~}" results)
            (setf * (first results)))
        (m:frame-exit ()
          (format t "~&---~%Good Bye!~%")
          (return))
        (serious-condition (c)
          (format t "~&---~%ERROR:~%~a" c))))))

;; Command table
(m:define-command-table default-command-table)

(m:define-command (com-get-time :command-table default-command-table :name t)
    ()
  (t:format-rfc1123-timestring t (t:now)))

(m:define-command (com-echo :command-table default-command-table :name t)
    ((string 'string))
  (format t "~a" string))

(m:define-command (com-eval :command-table default-command-table :name t)
    ((expression 'm:expression))
  (eval expression))

;; TODO app

(defclass task ()
  ((state
    :type (member :todo :done)
    :initarg :state
    :accessor state)
   (title
    :type string
    :initarg :title
    :accessor title)))

(defclass textual-view-with-colors (m:textual-view) ())
(defconstant +textual-view-with-colors+
  (make-instance 'textual-view-with-colors))

(m:define-presentation-method m:present
    (object (type task) stream (view m:textual-view) &key)
  (format stream "[~s | ~a]" (state object) (title object)))

(m:define-presentation-method m:present
    (object (type task) stream (view textual-view-with-colors) &key)
  (m:with-drawing-options (stream :ink (ecase (state object)
                                         (:done m:+dark-green+)
                                         (:todo m:+dark-red+)))
    (format stream "~a " (state object)))
  (format stream "| ~a" (title object)))

(m:define-application-frame todo-list ()
  ((tasks :initarg :tasks :accessor tasks))
  (:panes (int :interactor
               :default-view +textual-view-with-colors+
               :text-margins '(:left 5 :top 3))
          (app :application
               :default-view +textual-view-with-colors+
               :text-margins '(:left 10 :top 5)
               :display-function (lambda (frame stream)
                                   (declare (ignore frame stream))
                                   (show-tasks))))
  (:layouts (default (m:vertically () app int)))
  (:command-table (todo-list :inherit-from (default-command-table)))
  (:default-initargs :tasks nil))

(define-todo-list-command (add-task :name t)
    ((title 'string))
  (push (make-instance 'task :title title :state :todo)
        (tasks m:*application-frame*)))

(define-todo-list-command (delete-task :name t)
    ((task 'task))
  (let ((frame m:*application-frame*))
    (setf (tasks frame) (remove task (tasks frame)))))

(define-todo-list-command (change-state :name t)
    ((task 'task))
  (setf (state task)
        (ecase (state task)
          (:done :todo)
          (:todo :done))))

(define-todo-list-command (show-tasks :name t)
    ()
  (m:format-textual-list
   (tasks m:*application-frame*)
   (lambda (object stream)
     (m:present object 'task :stream stream))
   :separator #\newline))

(m:define-presentation-to-command-translator click-change-state
    (task change-state todo-list)
    (object)
  (list object))

;;
;; Use the app:
;;
(defun command-loop (frame)
  (let ((m:*application-frame* frame)
        (ct (m:frame-command-table frame)))
    (simple-repl ct)))

(defparameter *todo-list* (m:find-application-frame 'todo-list))

(defun start ()
  (command-loop *todo-list*))

;;
;; And also: a frame manager to have several todo-apps at once.
;;
