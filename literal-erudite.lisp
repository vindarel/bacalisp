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

;; @ignore
;; @section Evaluating code

;; With the latest Erudite, we can evaluate code.
;; I don't know how to make it work yet. This breaks the whole rendering process:

;; @eval
;; The result of 1 + 1 is…
(+ 1 1)
;; @end eval
;; @end ignore

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
