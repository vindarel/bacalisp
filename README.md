
Training, experiments.

More CL code on Github \o/


- comby-refactoring.md: trying https://comby.dev/ as found by https://github.com/svetlyak40wt/comby-test

- `shell-utils:tty-p`: are we on a terminal window or on a dumb terminal (Emacs shell mode)?

- `issr-test`: testing http://cjackson.tk/todo-tutorial

- `local-time.lisp`: simple additions useful for me that won't be merged upstream.

- CSV parsing and manipulation with cl-csv and its companion [data-table](https://github.com/AccelerationNet/data-table/).

`cl-csv` "only" reads and parses the CSV. `data-table` allows to
access all cells by row and column name (instead of by position).

~~~lisp
;; Parse the CSV, get a data-table object.

(defun parse-csv (file)
  "Parse CSV, return a data-table object with column names and rows."
  (let ((rows (csv:read-csv (str:from-file file))))
    (make-instance 'data-table:data-table :column-names (first rows) :rows (rest rows))))

;; Iterate on rows,
;; access columns with data-table-value :row row :col-name "the col name"

(defun get-all-days (dt)
  (remove-duplicates
   (loop for row in (data-table:rows dt)
         collect (data-table:data-table-value dt :row row :col-name "Date"))
   :test #'equal))
~~~

redditors also mentioned that
[cl-duckdb](https://github.com/ak-coram/cl-duckdb) is useful and
pretty fast at parsing.
