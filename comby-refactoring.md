
I needed to replace `ppcre:regex-replace-all foo bar new` by `… (list new)`, because of a ppcre bug when the delimiter ends with two backslashes and a backquote (cl-str issue 52).

    comby 'ppcre:regex-replace :[old] :[s] :[new]' 'ppcre:regex-replace :[old] :[s] (list :[new])' -in-place str.lisp

todo: we should not replace when we already have `(list new)`.


Trivial, as a `sed`:

    comby 'cl-ppcre' 'ppcre' -in-place *.lisp