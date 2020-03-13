(defpackage #:pisi
  (:use #:cl))

(in-package :pisi)

;; Enable triple quotes.
(pythonic-string-reader:enable-pythonic-string-syntax)

"""
Parse the lines of a shell script to a list of items consisting of
a title, some lines of code, and possibly the link to an image and
some documentation.

Example:

#!/shebang

#+ Title
#img: /assets/im.png
#tag: tag1
#: doc
#: doc2
#: doc3
apt
# comment
# comment2
sh

#+ Item2
"""

#|
comments: damn, loop is so damn useful!
|#

(defstruct item
  (title "")
  (code "")
  (img "")
  (doc ""))

(defun print-item (item)
  (format nil "#+ ~a~&~
    #img: ~a~&~
    ~a~&~
    ~a" (item-title item)
    (item-img item)
    (str:unlines (mapcar (lambda (line)
                           ;; just because ppcre:regex-replace-all "^"
                           ;; didn't work for all beginning of lines.
                           (ppcre:regex-replace "^" line "#: "))
                         (str:lines (item-doc item))))
    (item-code item)))
#+nil
(let* ((par
        """#+ title
#img: /img.png
#: doc
#: doc2
foo
foo2""")
       (item (print (parse-create-item (str:lines par)))))
  (assert (string= par
                   (print-item item))))

(defun title-p (line)
  (str:starts-with-p "#+" line))

(defun new-item-p (line)
  (title-p line))

(defun line-title (line)
  (when (title-p line)
    (str:trim (str:replace-all "#+" "" line))))

(defun doc-p (line)
  (str:starts-with-p "#:" line))

(defun line-doc (line)
  (when (doc-p line)
    (str:trim (str:replace-all "#:" "" line))))
#+nil
(assert (string= "doc 1"
                 (line-doc "#: doc 1")))

(defun img-p (line)
  (str:starts-with-p "#img:" line))

(defun line-img (line)
  (when (img-p line)
    (str:trim (str:replace-all "#img:" "" line))))

(defun syntax-p (line)
  (or (title-p line)
      (doc-p line)
      (img-p line)))
#+nil
(assert (and (syntax-p "#: doc")
             (syntax-p "#:doc")
             (syntax-p "#+item")))

(defun code-p (line)
  (cond
    ((syntax-p line)
     nil)
    (t
     t)))
#+nil
(assert (and (code-p "#!/shebang")
             (code-p "")
             (code-p "# comment")
             (not (code-p "#: doc"))
             (code-p "apt")
             (code-p "  foo")))

(defun parse-create-item (lines)
  "The first line starts a new item with the #+ syntax.
  We read until the next item.
  Return a new item struct."
  (when (title-p (first lines))
    (let ((item (make-item :title (line-title (first lines)))))
      (loop for line in (cdr lines)
         until (new-item-p line)
         when (img-p line)
         do (setf (item-img item)
                  (line-img line))
         when (doc-p line)
         collect (line-doc line) into doc-lines
         when (code-p line)
         collect line into code-lines
         finally (progn
                   (setf (item-doc item)
                         (str:unlines doc-lines))
                   (setf (item-code item)
                         (str:unlines code-lines))))
      item)))
#+nil
(let* ((par
       """#+ title
#img: /img.png
#: doc
#: doc2
foo
foo2

#+ t2
bar
""")
       (item (print (parse-create-item (str:lines par)))))
  (assert (item-p item))
  (assert (not (str:contains? "t2" (print-item item)))))


(defun parse-lines (lines)
  (loop for line in lines
     for index from 0
     when (new-item-p line)
     collect (parse-create-item (subseq lines index))))
#+nil
(let* ((par
        """#+ title
#img: /img.png
#: doc
#: doc2
foo
foo2

#+ t2
bar
""")
       (items (print (parse-lines (str:lines par)))))
  (assert (= 2 (length items)))
  (assert (item-p (first items))))
