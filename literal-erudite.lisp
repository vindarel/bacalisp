#| The latest Erudite release brings cool new features.
|#

;; @section Literate programming (in Lisp) with Erudite

;; A comment.
(print :hello
       ;; This comment is inline.
       :bar)

;; @section Section two

(print
 :really :cool!)

;; @ignore

(this should be ignored) (and it is)
;; this too? yes, this too.

;; @end ignore

#|
@section Evaluating code

With the latest Erudite, we can evaluate code. Note that it's a work
in progress.

Write code between the "eval" and "end eval" directives (which I can't
write with the "@" nor use inside "verbatim" here).

The code snippet must (currently) be inside the comments too.

 The result of
@code
(1 + 1)
@end code

is…

@eval
(+ 1 1)
@end eval

<br>

You might need to create a Swank server first with

@code
(swank:create-server :dont-close t)
@end code

and tell Erudite its port

@code
(setf erudite::*swank-port* 4005)
@end code
 |#

;; @section Rendering to markdown

(erudite:erudite #p"literal.md" "literal-erudite.lisp" :output-type :markdown)

;; @section Live rendering

#|
 @ignore

 It's all more fun when it's automatic. Isn't it?

 Livedown didn't work, it rendered a blank page after an Erudite update.

 Impatient-mode… works! But it easily breaks.

 Now that's cool :)

@end ignore
|#

(ql:quickload :cl-inotify)

;; Automatically run the Erudite rendering when the file is changed:
(bt:make-thread
 (lambda ()
   (cl-inotify:with-inotify (inotify t ("literal-erudite.lisp" :close-write))
     (cl-inotify:do-events (event inotify :blocking-p t)
       (format t "~a~&" event)
       (erudite:erudite #p"literal.md" "literal-erudite.lisp" :output-type :markdown))))
 :name "inotify-erudite")
