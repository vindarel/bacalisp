(defstruct (tree-node (:conc-name nil))
  key
  (children '()))

(defparameter *tree-a*
  (let* ((f (make-tree-node :key "f"))
         (e (make-tree-node :key "e"))
         (d (make-tree-node :key "d"))
         (c (make-tree-node :key "c" :children (list f)))
         (b (make-tree-node :key "b" :children (list d e))))
    (make-tree-node :key "a" :children (list b c))))

#|
            a
           / \
          b   c
         / \   \
        d   e   f

#S(TREE-NODE
   :KEY "a"
   :CHILDREN (#S(TREE-NODE
                 :KEY "b"
                 :CHILDREN (#S(TREE-NODE :KEY "d" :CHILDREN NIL)
                            #S(TREE-NODE :KEY "e" :CHILDREN NIL)))
              #S(TREE-NODE
                 :KEY "c"
                 :CHILDREN (#S(TREE-NODE :KEY "f" :CHILDREN NIL)))))
|#

(defun depth-first-search-node (fn root)
  (funcall fn (key root))
  (dolist (child (children root))
    (depth-first-search-node fn child)))

#+nil
(depth-first-search-node #'print *tree-a*)

(defun depth-first-search/postorder (fn root)
  (dolist (child (children root))
    (depth-first-search/postorder fn child))
  (funcall fn (key root)))

#+nil
(depth-first-search/postorder #'print *tree-a*)

;;;
;;; Breadth-First Search
;;;

(defun breadth-first-search-node (fn nodes)
  (let ((next-level (list)))
    (dolist (node (if (consp nodes)
                      nodes
                      (list nodes)))
      (funcall fn (key node))
      (dolist (child (children node))
        (push child next-level)))
    (when next-level
      (breadth-first-search-node fn (reverse next-level)))))

#+nil
(breadth-first-search-node #'print *tree-a*)
#|
"a"
"b"
"c"
"d"
"e"
"f"
|#

;; Pretty printing

(defun print-levels (level skip-levels)
  (dotimes (i level)
    (format t "~a    " (if (gethash i skip-levels)
                           " "
                           "|"))))

(defun print-branch (last-child-p child)
  (format t "~a−− ~a~&"
          (if last-child-p
              "−−"
              "|−")
          (key child)))

(defun pprint-tree (node &optional (level 0) (skip-levels (make-hash-table)))
  (format t "~&")
  (when (= 0 level)
    (format t "~a~&" (key node)))
  (let ((last-index (1- (length (children node)))))
    (loop for child in (children node)
       for i = 0
       for last-child-p = (= i last-index)
       do (print-levels level skip-levels)
       do (print-branch last-child-p child)
       ;; do (rutils:sethash i skip-levels last-child-p)
       do (setf (gethash i skip-levels)
                last-child-p)
       do (pprint-tree child (1+ level) skip-levels)
       do (incf i))))

#+nil
(pprint-tree *tree-a*)

#|
a
|−−− b
|    |−−− d
|    |−−− e
|−−− c
|    −−−− f

(end not correct?)
|#
