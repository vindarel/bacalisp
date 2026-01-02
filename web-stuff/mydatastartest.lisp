
(defpackage :mydatastar
  (:use :cl
   :datastar-cl)
  (:documentation "you'll need:
   - hunchentoot
   - pythonic-string-reader
   - log4cl
   (or just CIEL)

   and datastar-cl https://github.com/fsmunoz/datastar-cl?tab=readme-ov-file"))


(in-package :mydatastar)


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

(pythonic-string-reader:enable-pythonic-string-syntax)

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
        data-on:input__debounce.200ms="@get('/search')"
      />

      <div id="results"></div>
    </div>

  </body>

  """")


;; routes

(hunchentoot:define-easy-handler (index :uri "/") ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (index))


(hunchentoot:define-easy-handler (details-handler :uri "/search") ()
  "Process signals, send HTML fragments."
  (let ((signals (datastar-cl:read-signals hunchentoot:*request*)))
    (datastar-cl:with-sse-response (gen hunchentoot:*request*)
      (log:info signals)
      (when signals
        (let ((q (gethash "search" signals)))
          (when q
            (datastar-cl:patch-elements gen
                                        (format nil
                                                """"
                                                <div id="results">
                                                you are searching for: ~a
                                                </div>
                                                """"
                                                q
                                                ))))))))
