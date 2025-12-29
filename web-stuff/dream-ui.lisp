
(defpackage :dream-ui
  (:use :cl
   :ciel))

(in-package :dream-ui)

"
Can we have a better server-rendered typeahead/search than datalist?

datalist can't be styled.
Firefox only shows 6 items, the select box looks ridiculous.

Is that better?

Yes!!

This works with HTMX.
"

(eval-when (:execute)  ;; enough?
  (ql:quickload "find-port"))

(enable-pythonic-string-syntax)

(defparameter *server* nil)

(defparameter *index.html* (str:from-file "dream-ui.html"))

(defun serve-static-assets ()
  "Let Hunchentoot serve static assets under the current directory.

  Then reference static assets with no prefix.

  Used to load dream-ui.js"
  (push (hunchentoot:create-folder-dispatcher-and-handler
          "/"
          "")
        hunchentoot:*dispatch-table*))

(easy-routes:defroute index ("/") ()
  *index.html*)

(defun gen-data (&optional (n 50))
  (loop for i below n
        collect (format nil "~R" i)))

(defun render-datalist (q &optional (n 50))
  "Render HTML that fits in dream-ui's Bulma dropdown style."
  (when (str:blankp q)
      (setf n 10))
  (with-output-to-string (s)
    (loop for option in (gen-data n)
          with i = 0
          when (str:containsp q option)
            do
               (unless (zerop i)
                 (format s """"
                        <hr class="dropdown-divider" />
                        """") )
                (format s """"
                          <button class=dropdown-item tabindex="~a"
                            value="~a" role="menuitem">
                            ~a
                        </button>
                          """"
                          i
                          option
                          option)
                (incf i))
    ))

(easy-routes:defroute typeahead ("/combobox") ()
  ;; XXX: not called??
  (format t "==> this is the search input: ~s ~&" (hunchentoot:get-parameter "fruit"))
  ;; so here we can search and filter the DB,
  ;; even though in this example we return the same datalist options and the browser
  ;; does the filtering.
  (render-datalist (hunchentoot:get-parameter "fruit")))

(defun start-app (&key port)
  (unless port
    (setf port (find-port:find-port)))
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor :port port))
  (hunchentoot:start *server*)
  (serve-static-assets)
  (format! t "started Hunchentoot on port ~a" port))

(defun stop-app ()
  (hunchentoot:stop *server*))

#+ciel
(start-app)
