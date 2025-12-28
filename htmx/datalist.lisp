
(in-package :ciel-user)

"doc: it works well, but: the datalist can't be styled, so it looks ridiculous,
 Firefox only shows 6 items, which we can't style (no custom HTML: no bold, no image etc).
 It's a poor man select2 / typeahead."

(eval-when (:execute)  ;; enough?
  (ql:quickload "find-port"))

(enable-pythonic-string-syntax)

(defparameter *server* nil)

(defparameter *index.html* """"
              <html>
              <head>
                <script src="https://cdn.jsdelivr.net/npm/htmx.org@2.0.8/dist/htmx.min.js"></script>
              </head>

              <body>

              <input
  list="wordList"
  name="wordInput"
  hx-get="/typeahead"
  hx-target="#wordList"
  hx-trigger="keyup changed delay:500ms"
/>
<datalist id="wordList"></datalist>

         </body>
"""")

(defun render-datalist (&optional (n 50))
  (with-output-to-string (s)
    (format s """"
            <datalist id="wordList">
            """")
    (loop for i below n
          collect (format s """"
                          <option value="~R"></option>
                          """"
                          i))
    (format s "</datalist")))

(easy-routes:defroute index ("/") ()
  *index.html*)

(easy-routes:defroute typeahead ("/typeahead") ()
  (format t "==> this is the search input: ~s ~&" (hunchentoot:get-parameter "wordInput"))
  ;; so here we can search and filter the DB,
  ;; even though in this example we return the same datalist options and the browser
  ;; does the filtering.
  (render-datalist))


(defun start-app (&key port)
  (unless port
    (setf port (find-port:find-port)))
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor :port port))
  (hunchentoot:start *server*)
  (format! t "started Hunchentoot on port ~a" port))

(defun stop-app ()
  (hunchentoot:stop *server*))

#+ciel
(start-app)
