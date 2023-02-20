
(uiop:define-package #:meilisearch-test
  (:use :cl :ciel
        :quri)
  (:documentation
   "Trying out utilities to add documents to a Meilisearch instance and search for data.
This is basically POSTing JSON data to a URL.

https://docs.meilisearch.com/learn/getting_started/quick_start.html#add-documents

Usage:

- start meilisearch
- set *base-url*
- post-document and search-document
  - set their :document key or use with-document.
"))

(in-package :meilisearch-test)

;; (uiop:add-package-local-nickname :json :shasht)
;; weird… no "json" nickname here?
;; json is defined in ciel-user,
;; not in inherited packages. => "use" :ciel, not :ciel-user

(defparameter *base-url* "http://localhost:7700/")

(defparameter *json-headers* '(("Content-Type" . "application/json")))

(defvar *document-name* nil
  "Current document name.")

(defmacro with-document ((name) &body body)
  `(let ((*document-name* ,name))
     ,@body))

(defun %post-document (string &key (document *document-name*))
  (let ((url (merge-uris (uri (format nil "/indexes/~a/documents?primaryKey=id" document))
                         (uri *base-url*))))
    (log:info url)
    (dex:post (format nil "~a" url)
              :headers *json-headers*
              ;; a JSON string:
              ;; Meilisearch accepts JSON, CSV files and more for batch inserts.
              :content string)))

(defmethod post-document (s &key (document *document-name*))
  (declare (ignorable document))
  (error "document type not supported: ~a~
Please use a JSON string, or… TBD." (type-of s)))

(defmethod post-document ((json-string string) &key (document *document-name*))
  (%post-document json-string :document document))

(defmethod post-document ((ht hash-table) &key (document *document-name*) (pretty nil))
  "Get a document has a hash-table, transform it to a JSON string and post it to Meilisearch."
  (let ((s (json:write-json* ht :stream nil :pretty pretty)))
    (%post-document s :document document)))


(defun search-document (q &key (document-name *document-name*)
                            (pretty nil)
                            keys)
  "Search for this document.
Return: a dict, with Meilisearch keys:
- hits => vector of dicts with keys: id, name
- estimatedTotalHits
- query
- offset
- processingTimeMs"
  (let ((url (merge-uris (uri (format nil "/indexes/~a/search" document-name))
                         (uri *base-url*)))
        (json (shasht:write-json* (dict "q" q) :stream nil :pretty pretty)))
    (bind ((res (dex:post url
                          :headers *json-headers*
                          :content json))
           (dict (shasht:read-json res)))
      (print-hits dict :keys keys)
      dict)))

(defun print-hits (ht &key keys)
  "Print results from this hash-table raw result."
  (loop for hit across (access ht "hits")
     do (pprint-hit hit :keys keys)
       (format t "~%")))

(defun pprint-hit (hit &key keys)
  "Pretty-print this hit (hash-table).
If a key or a list of keys are given, print only these keys.
Otherwise, print in the form key: value, one per line."
  (if keys
      (let ((keys (ensure-list keys)))
        (loop for key in keys
           do (format t "~10a: ~70a~&" key (access hit key))))
      (pprint-key-values hit)))

(defun pprint-key-values (ht)
  "Print this HT data in the form key: value, one per line."
  (loop for key being the :hash-key of ht
     do (format t "~10a: ~70a~&" key (access ht key))))
#|
id        : 334685
title     : Ma
overview  : In this modern-day vision of Mother Mary's pilgrimage, a woman crosses the American Southwest playfully deconstructing the woman’s role in a world of roles.
genres    : #(Drama)
poster    : https://image.tmdb.org/t/p/w500/gZ32qFZ5zoIEyAe8io7D0yZJxaA.jpg
release_date: 1441411200
|#
