;;;
;;; Copied from Common Lisp Recipes chap. 20-4
;;; Call with:
;;; (capi:display (make-instance 'calkin-wilf-tree))

;;; editor completion: C-M-i
;;; compile defun: C-S-c


(defclass node ()
  ((value :initarg :value
          :reader value)
   (childen :initform nil
            :accessor children)))

(defmethod add-children ((node node))
  (let* ((numerator (numerator (value node)))
         (denominator (denominator (value node)))
         (sum (+ numerator denominator)))
    (setf (children node)
          (list (make-instance 'node :value (/ numerator sum))
                (make-instance 'node :value (/ sum denominator))))))

(defmethod print-object ((node node) stream)
  (with-slots (value) node
    (format stream "~a/~a" (numerator value) (denominator value))))

(defun one ()
  (make-instance 'node :value 1))

(capi:define-interface calkin-wilf-tree ()
  ()
  (:panes
   (tree
    capi:tree-view
    :reader tree
    :roots (list (one))
    :children-function #'children
    :action-callback (lambda (node interface)
                       (unless (children node)
                         (add-children node)
                         (capi:tree-view-update-item (tree interface)
                                                     node nil)))
    :action-callback-expand-p t)
   (reset-button
    capi:push-button
    :text "Reset"
    :callback-type :interface
    :callback (lambda (interface)
                (setf (capi:tree-view-roots (tree interface))
                      (list (one))))))


  (:layouts
   (default-layout
    capi:column-layout '(tree reset-button)
    :adjust :center))

  (:default-initargs 
   :best-width 400
   :best-height 300
   :title "Calkin-Wilf tree"))

(defun run ()
  (capi:display (make-instance 'calkin-wilf-tree)))