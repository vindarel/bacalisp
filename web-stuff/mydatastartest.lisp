
(defpackage :mydatastar
  (:use :cl
   :datastar-cl)
  (:documentation "you'll need:
   - hunchentoot
   - pythonic-string-reader
   - log4cl
   - str
   (or just CIEL)

   and datastar-cl https://github.com/fsmunoz/datastar-cl?tab=readme-ov-file"))


(in-package :mydatastar)

(pythonic-string-reader:enable-pythonic-string-syntax)


(defparameter *port* 7000
  "Port for Hunchentoot HTTP server")

(defvar *server* nil)

;;; Server Management

(defun start-app (&key (port *port*))
  "Start Hunchentoot server.

   Hunchentoot is designed for long-lived connections (push mode),
   but pull mode is available for experimentation/testing."
  (setf *server* (make-instance 'hunchentoot:easy-acceptor :port port))
  (hunchentoot:start *server*)
  (format t "~%Hunchentoot server started on port ~a~%" port))

(defun stop-app ()
  (when *server*
    (hunchentoot:stop *server*)
    (setf *server* nil)))

;; templates

(defun index ()
  "return: string"
  """"
  <head>
    <script type="module" src="https://cdn.jsdelivr.net/gh/starfederation/datastar@1.0.0-RC.7/bundles/datastar.js"></script>

    <!-- Bulma -->
    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bulma@1.0.4/css/bulma.min.css">

  </head>

  <body>

    <div class="container">
     <div> hello ! </div>

      <input
        type="text"
        placeholder="Search..."
        data-bind:search
        data-on:input__debounce.500ms="@get('/search')"
      />

      <div id="results"></div>
    </div>

  </body>

  """")

;; State: we need to keep all state right?
(defparameter *searches* (list))


;; routes

(hunchentoot:define-easy-handler (root :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (index))

(defun results (searches)
  "We must keep all state here, I guess? Instead of pushing and adding one single element to a list of divs."
  ;; xxx: use templates or the markup library.
  (with-output-to-string (s)
    (princ """"<div id="results">"""" s)
    (loop for search in searches
          do
             (format s
                     """"
                     <div>
                     you searched for: ~a
                     </div>
                     """"
                     search))
    (princ "</div>" s)))

(hunchentoot:define-easy-handler (details-handler :uri "/search") ()
  "Process signals, send HTML fragments."
  (let ((signals (datastar-cl:read-signals hunchentoot:*request*)))
    (datastar-cl:with-sse (gen hunchentoot:*request*)
      (log:info signals)
      (when signals
        (let ((q (gethash "search" signals)))
          (when (and q (str:non-blank-string-p q))
            (pushnew q *searches*)
            (datastar-cl:patch-elements gen
                                       (results *searches*)
                                        )))))))
