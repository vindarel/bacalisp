
;;;
;;; Use dream-ui combobox with Datastar signals: it works o/
;;;
;;; aka: ajax-based autocoplete/select2 with no client-side javascript.
;;;

(eval-when (:execute)  ;; enough?
  (ql:quickload '("find-port"
                  "markup"
                  "hunchentoot"
                  "datastar-cl/hunchentoot"
                  "easy-routes"
                  "log4cl"
                  "str"
                  )
                ))

(defpackage :datastar-dream-ui
  (:use :cl))

(in-package :datastar-dream-ui)

(defvar *server* nil)

(defparameter *index.html* (str:from-file "dream-ui.html")
  "currently, need to recompile this after each HTML edit…")

(markup:enable-reader)

(markup:deftag template (children &key title)
  <html>
    <head>
     <title>,(progn title)</title>
    </head>
    <body>
      ,@(progn children)
    </body>
  </html>)

(defun testmarkup ()
  (markup:write-html
     <template title="Hello" >
        <h1>hello world!</h1>
     </template>))

(defun serve-static-assets ()
  "Let Hunchentoot serve static assets under the current directory.

  Then reference static assets with no prefix.

  Used to load dream-ui.js"
  (push (hunchentoot:create-folder-dispatcher-and-handler
          "/"
          "")
        hunchentoot:*dispatch-table*))

;; user-facing UI:
(easy-routes:defroute index ("/datastar-dream-ui/") ()
  *index.html*)

(defun gen-data (&optional (n 50))
  (loop for i below n
        collect (format nil "~R" i)))

(defun render-datalist (q &optional (n 50))
  "Return HTML (string): a button by matching result.

  Each button displays a small image.

  The HTML fits in dream-ui's Bulma dropdown style."
  (when (str:blankp q)
    (setf q "one"))
  (with-output-to-string (s)

    ;; markup: we can't write unclosed html tags.
    ;; no:

    ;; (markup:write-html-to-stream
    ;;  <div id="datastar-search-results" class="dropdown-content"> </div>
    ;;    s)

    (format s "<div id=\"datastar-search-results\" class=\"dropdown-content\">")

     (loop for option in (gen-data n)
          for i = 0 then (incf i)
          when (str:containsp q option)
            collect (markup:write-html-to-stream
                <button class="dropdown-item"
                        tabindex=i
                        value=option
                        role="menuitem">
                  <table>
                    <tr>
                      <td>
                        <img src="https://lispcookbook.github.io/cl-cookbook/orly-cover.png"
                             style="max-width: 50px"/>
                      </td>
                      <td style="padding-left: 5px">
                        ,(progn option)
                      </td>
                    </tr>
                  </table>
                </button>
                s))

    (format s "~&</div>")

   ))


;; The route in the html combobox
(easy-routes:defroute datastar-dream-ui ("/search") ()

  ;; We don't have a classic URL parameter anymore (unlike with HTMX),
  ;; datastar sends search?datastar={"search":"one"}
  ;; (format t "==> this is the search input: ~s ~&" (hunchentoot:get-parameter "q"))

  ;; so here we can search and filter the DB,
  ;; even though in this example we return the same datalist options and the browser
  ;; does the filtering.
  (let* ((signals (datastar-cl:read-signals hunchentoot:*request*))
         (q (gethash "search" signals)))
    (log:info signals)
    ;; (unless signals
      ;; (format t "--- no datastar signals found.~&"))
    (when (and signals
               (str:non-blank-string-p q))
      (let ((html (render-datalist q)))
        (log:info html)
        (datastar-cl:with-sse (resp hunchentoot:*request*)
          (datastar-cl:patch-elements resp html))))))

(defun start-app (&key port)
  (unless port
    (setf port (find-port:find-port)))
  (setf *server* (make-instance 'easy-routes:easy-routes-acceptor :port port))
  (hunchentoot:start *server*)
  (serve-static-assets)
  (uiop:format! t "started Hunchentoot on port ~a" port))

(defun stop-app ()
  (hunchentoot:stop *server*))
