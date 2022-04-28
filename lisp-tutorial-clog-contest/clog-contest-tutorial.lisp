;;@ignore
;;
;; This is a literate blog post.
;; Comments and sources are inlined and are rendered to a markdown file.
;; You can read it in your editor and evaluate code snippets one by one.
;;
;; Render to a file with Erudite:
;;
;; (erudite:erudite #p"clog-contest.md" "clog-contest-tutorial.lisp" :output-type :markdown)
;;
;; Below is the YAML frontmatter for my Hugo blog.
;; @end ignore
;;
;; ---
;; title: "Writing an interactive web app in Common Lisp: Hunchentoot then CLOG"
;; date: 2022-04-28T15:23:14+02:00
;; tags: ["tutorial", "web"]
;; draft: false
;; ---

#|

We want a web app to display a list of data and have an input field to interactively filter it.

We'll start with a simple, regular web app built with
Hunchentoot. We'll have a search input to filter our data, and we'll see that to be more interactive, typically to filter out the results as the user types, we'll need more than basic HTTP requests. We'll need some JavaScript. But we'll reach this level of interactivity with CLOG (and no JavaScript).

> DISCLAIMER: this post is my entry for the @link{https://www.reddit.com/r/lisp/comments/tpr0bu/common_lisp_50_tutorial_contest_3_winners/}{CLOG contest}!

|#

;;
;; Let's install our first libraries: Hunchentoot for the web server, Djula for the HTML templates, str for a string utility.
#+(or)
(ql:quickload '("hunchentoot" "djula" "str"))

;; We create a package for our experiments, and we "enter" it. I use UIOP's `define-package` because it throws less warnings than `defpackage` when we add and remove symbols. It also has more features (`:reexport`) that I don't use here.
(uiop:define-package :clog-contest
    (:use :cl))

(in-package :clog-contest)

;; OK. We define a route. It takes one GET parameter, for demo purposes.
(hunchentoot:define-easy-handler (root-route :uri "/") (name)
  (format nil "Hey~@[ ~A~]!" name))

;; Start the server:
(defvar *server* (make-instance 'hunchentoot:easy-acceptor :port 6789))
(hunchentoot:start *server*)

;; and access http://localhost:6789/

;; Now let's create our products. We quickly define a class containing an ID, a title and a price.

(defclass product ()
  ((id :initarg :id :accessor product-id :type integer
       :documentation "Unique ID")
   (title :initarg :title :accessor product-title :type string)
   (price :initarg :price :accessor product-price :type integer)))

(defvar *product-id* 1
  "Stupid counter to increment our unique product ID.
  Normally this is given by a DB.")

(defparameter *products* '() "A list of products.")

;; We are going to create random testing products, so let's have a couple helpers to create random titles and prices.
(defun random-price ()
  "Return an integer between 1 and 10.000 (price is expressed in cents)."
  (1+ (random 9999)))

(defparameter *title-part-1* (list "pretty" "little" "awesome" "white" "blue"))
(defparameter *title-part-2* (list "book" "car" "laptop" "travel" "screwdiver"))
(defun random-title ()
  (let ((index (random (length *title-part-1*)))
        (index-2 (random (length *title-part-2*))))
    (format nil "~a ~a" (elt *title-part-1* index) (elt *title-part-2* index-2))))
;; try it out:
#+(or)
(random-title)
;; We get titles like "white book", "little car", etc.

;; Now, for testing purposes, we create a 100 dummy product instances:
(defun gen-test-products (&optional (nb 100))
  (dotimes (i nb)
    (push (make-instance 'product
                         :id (incf *product-id*)
                         :title (random-title)
                         :price (random-price))
          *products*)))

(defun reset-test-products ()
  (setf *products* nil))

#|
Try it and we get:

@code
*products*
(#<PRODUCT {1005B29363}> #<PRODUCT {1005B29113}> #<PRODUCT {1005B28EC3}>
 #<PRODUCT {1005B28C73}> #<PRODUCT {1005B28A23}> #<PRODUCT {1005B287D3}>
 …)
@end code

Implement the `print-object` method if you want nice-looking product literals. See the Cookbook.

|#

;; <br>
;; Now let's display the products in the browser. We will redefine our route.
;; We make sure to extract the view logic in functions.

(defun print-product (it &optional (stream nil))
  "Print a product title and price on STREAM (return a new string by default)."
  (format stream "~a - ~f~&"
          (str:fit 20 (product-title it))  ;; the fit function was merged recently.
          (/ (product-price it) 100)))

(defun print-products (products)
  "Return a list of products as a string (dummy, for tests purposes)."
  (with-output-to-string (s)
    (format s "Products:~&")
    (dolist (it products)
      (print-product it s))))
#|
@code
CL-USER> (print-products (subseq *products* 0 10))

"Products:
pretty car           -   22.26
awesome travel       -   13.87
little screwdiver    -   35.6
white laptop         -   6.08
little book          -   27.57
white laptop         -   42.63
blue travel          -   93.8
blue car             -   29.99
pretty car           -   38.95
little screwdiver    -   46.99
@end code

|#

;; We tell our route to display the list of products like this:

(hunchentoot:define-easy-handler (root-route :uri "/") ()
  (print-products *products*))

;; We see something, but it's stupid to return text to the browser. We need templates.

;; We'll use Djula templates. And we'll steal some ready-to-use pretty HTML :)
;;
;; I'll use @link{https://bulma.io/}{Bulma} CSS because it's simple, modern (flexbox) and just because. I don't know all CSS frameworks out there.

;; I'll do my shopping in this showcase of Bulma templates: https://bulmatemplates.github.io/bulma-templates/ IIRC I took the "Modal Cards" one and simplified it a bit.

;; Our final result is:

;; ![](/blog/products.png)

;; Let's create a `templates/` directory and create:
;;
;; - base.html
;;
;; - products.html, that inherits the base.
;;
;; The base template loads Bulma from a CDN, creates a navbar, defines a "content" block that our other templates will override, and a footer.
;;
;; Our products template "extends" base.html and creates the "content" block.
;; There we loop over a list of products given by our Hunchentoot root and display them.

;; But before that happens, we need to install and configure Djula to find and compile our templates.

;; We tell Djula to look for templates in the templates/ directory.
(djula:add-template-directory "templates/")

#|
Note that normally, I do that relatively to an .asd file, that we didn't create yet:

@code
(djula:add-template-directory
 (asdf:system-relative-pathname "myproject" "src/templates/"))
@end code

If you have an issue with the path on the Lisp REPL, on SLIME you can do ,cd (a comma command) to change the current working directory.

|#

;; Now we define our templates:
(defparameter +base.html+ (djula:compile-template* "base.html"))
(defparameter +products.html+ (djula:compile-template* "products.html"))

;; As a result, you can see they are compiled templates:
;; @code
;; CLOG-CONTEST> +products.html+
;; #<DJULA::COMPILED-TEMPLATE /home/vince/bacasable/bacalisp/lisp-tutorial-clog-contest/templates/products.html {2073D30B}>
;; @end code

;; OK, our route needs to return a template and give data to it.
;; Our route returns `djula:render-template*`.
(hunchentoot:define-easy-handler (root-route :uri "/") ()
  (djula:render-template* +products.html+ nil
                          :products *products*))

;; Nice!
;; We display all products. We will accept a search query to filter them. Pagination is for later.
;;
;; We have an input field that defines an HTML form,
;; that calls the search endpoint.
;; We need:
;;
;; - to define the /search route
;;
;; - to write a dummy function to search in our products list.

(defun search-products (query &optional (products *products*))
  "Search for QUERY in the products' title.
  This would be a DB call."
  (loop for product in products
     when (str:containsp (str:downcase query) (str:downcase (product-title product)))
     collect product))

;; Try it:
#+(or)
(search-products *products* "awesome")

(hunchentoot:define-easy-handler (search-route :uri "/search") (q)
  (let* ((products (search-products *products* q)))
    (djula:render-template* +products.html+ nil
                            :title (format nil "My products - ~a" q)
                            :query q
                            :products products
                            :no-results-p (zerop (length products)))))

;; Try it, it works :)
;; I agree, the search algorithm is simplistic. What about multiple words, accents, typos, non-exact searches (stemming)…?
;;
;; Your search query is seen in the URL parameters:
;; http://localhost:6789/search?q=travel
;; That is usually a good thing. In modern single-page applications, you can loose this, or you have to handle the URL construction yourself.
;;
;; The search required a page reload. If your app is fast, it might not be an issue.
;; However, if we wanted the search to be more interactive, for example showing results as we type, we would need to use JavaScript. Enters CLOG.
;;
;;
;; @section{CLOG}
;;
;; Can we make our app interactive with @link{https://github.com/rabbibotton/clog/}{CLOG}?
;;
;; Well, we can, and what's even cooler is that the development
;; process is itself very interactive.  CLOG sends changes to the page
;; through websockets as you add or edit functionalities. As such we
;; can see changes in real time. For example, change a colour:
;; @code
;; (setf (background-color *body*) :red)
;; @end code
;; and BAM, it's red.
;;
;;
;; Let's create another package for this new app. I'll "use" functions and macros provided by the :clog package, as well as our previously defined :clog-contest ones (duh… we didn't :export any yet).

(uiop:define-package :clog-contest-with-clog
    (:use :cl :clog
          :clog-contest))

(in-package :clog-contest-with-clog)

;;
;; The very first steps you can do to grasp CLOG's interactive fun is to make changes to a browser window while on the CLOG REPL.
;; @code
;; (ql:quickload "clog")
;; CL-USER> (in-package clog-user)
;; CLOG-USER> (clog-repl)
;; CLOG-USER> (setf (background-color *body*) "red")
;; CLOG-USER> (create-div *body* :content "Hello World!")
;; @end code
;; And voilà. A browser window was opened for you.
;;
;; You will also find many demos here: https://github.com/rabbibotton/clog/tree/main/tutorial
;; You can run them with `(clog:run-tutorial 1)` (by their number id).
;;
;; For the following, I invite you to have a look at CLOG's common elements: https://rabbibotton.github.io/clog/clog-manual.html#toc-8-common-clog-elements
;;
;; Typically, to create a `div` on a DOM element, we use `create-div`.
;;

#|
The first thing we want to start our CLOG app is the `initialize` function.
 @code
(on-new-window-handler &key (host 0.0.0.0) (port 8080) (server hunchentoot)
 (extended-routing nil) (long-poll-first nil) (boot-file /boot.html)
 (boot-function nil) (static-boot-html nil) (static-boot-js nil)
 (static-root (merge-pathnames ./static-files/ (system-source-directory clog))))

Inititalize CLOG on a socket using HOST and PORT to serve BOOT-FILE
as the default route to establish web-socket connections and static
files located at STATIC-ROOT. […]
@end code
|#
;;
;; It calls our `add-products` functions with a `body` (CLOG object) as argument.
(defun start-tutorial ()
  "Start tutorial."
  (initialize 'add-products)
  (open-browser))

;; OK so what do we want to do? We want to create a search input field, and below we display our products. When the user types something, we want to *immediately* start filter the products, and re-display them.

#|

A first version where we only display products would be this:

@code
(defun add-products (body)
  (let* ((result-div (create-div body :content "")))
    (display-products result-div (subseq clog-contest::*products* 0 10))))
@end code

And the `display-products` function is below:

|#

(defun display-products (body products)
  "Display these products in the page.
  Create a div per product, with a string to present the product.
  We don't create nice-looking Bulma product cards here."
  (dolist (it products)
      (create-div body :content
                  (format nil "~a - ~a"
                          (clog-contest::product-id it)
                          (clog-contest::print-product it)))))

;; Now we want to handle the interactivity. The event to watch is the key up event. In CLOG, we have `set-on-key-up` method. It takes: a CLOG object (the DOM object it watches for events) and a handler function. This function takes two arguments: the CLOG object and the event.
;;
;; In our add-products function below, we create the search input and we listen the key-up event:

(defun add-products (body)
  "Create the search input and a div to contain the products.
  Bind the key-up event of the input field to our filter function."
  (let* ((form (create-form body))
         (input (create-form-element form :input :name "query"
                                     :label
				     (create-label form :content "Filter product: ")))
         (result-div (create-div body :content "" )))

    (set-on-key-up input
                   (lambda (obj event)
                     (format t ":key-up, value: ~a~&" (value obj)) ; logging
                     (setf (text result-div) "") ; this is how we erase the current content.
                     (handle-filter-product result-div obj event)))

    (display-products result-div clog-contest::*products*)))

;; Below, to find out what is typed in the search input, we use `(value obj)`.

(defun handle-filter-product (div obj event)
  "Search and redisplay products."
  ;TODO: wait a little latency
  (declare (ignorable event))
  (let ((query (value obj)))
    (if (> (length query) 2)
        (display-products div (clog-contest::search-products query))
        (print "waiting for more input"))))

;; It works \o/
;;
;; ![](/blog/clog-search.gif)
;;
;; There are some caveats that need to be worked on:
;;
;; - if you type a search query of 4 letters quickly, our handler waits for an input of at least 2 characters, but it will be fired 2 other times.
;;
;;
;; And, as you noticed:
;;
;; - we didn't copy-paste a nice looking HTML template, so we have a bit of work with that :/
;;
;;
;; CLOG is not at all limited to websites like this. You can create games (there is a Snake demo), multiplayer applications (there is a chat demo)… all this by doing everything in the backend, in Common Lisp, with a lot interactivity under the fingertips.
