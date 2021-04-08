
#+(or)
(ql:quickload "sqlite")

(defvar *db* nil
  "Connection.")

(defun test-sqlite-fts5 ()
  "Test if we can use the FTS5 module (Full Text Search) with our SQLite version.

  https://www.sqlite.org/fts5.html"
  (sqlite:with-open-database (*db* ":memory:")
    (handler-case
        (progn
          (sqlite:execute-non-query *db* "CREATE VIRTUAL TABLE fts5test USING fts5 (data);")
          (sqlite:execute-non-query *db* "insert into fts5test (data)
                values ('This is a test of full-text search');")
          (values t (sqlite:execute-single *db* "select * from fts5test where data match 'full';")))
      (error (c)
          (values nil "We could not create a virtual table using fts5." c)))))
