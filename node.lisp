(in-package #:cl-regex)

(defparameter *condition-types* '(:everything :head :tail))
(defvar *node-counter*)

(defmethod initialize-instance :after ((nfa nfa) &rest initargs)
  (declare (ignore initargs))
  (unless (slot-boundp nfa 'id)
    (setf (slot-value nfa 'id)
	  (incf *node-counter*))))

(defgeneric node-shift-dfa-p (node))
(defmethod node-shift-dfa-p ((node node))
  nil)
(defmethod node-shift-dfa-p ((node nfa))
  (nfa-shift-dfa-p node))

(defgeneric node-lazy-p (node))
(defmethod node-lazy-p ((node node))
  nil)
(defmethod node-lazy-p ((node nfa))
  (nfa-lazy-p node))

(defmacro do-edges ((cdt next) edges &body body)
  (with-gensyms (gedge)
    `(dolist (,gedge ,edges)
       (let ((,cdt (edge-cdt ,gedge))
	     (,next (edge-next ,gedge)))
	 ,@body))))

(defun node-connect (head cdt tail)
  (conc1f (node-edges head)
	  (make-edge :cdt cdt :next tail)))

(defun node-connect-front (head cdt tail)
  (push (make-edge :cdt cdt :next tail)
	(node-edges head)))

(defun node-connects (nodes cdt)
  "((head1 tail1) (head2 tail2) ...)のn個目のtailとn+1個目のheadをcdtで繋いでいき
最初と最後のノードを多値で返す "
  (labels ((rec (nodes)
	     (if (null (cdr nodes))
		 (apply #'values (car nodes))
		 (destructuring-bind (head1 tail1 &optional rest) (car nodes)
		   (declare (ignore rest))
		   (multiple-value-bind (head2 tail2) (rec (cdr nodes))
		     (node-connect tail1 cdt head2)
		     (values head1 tail2))))))
    (rec nodes)))

(defun node-recursive (fn head-node)
  (let ((visited (make-hash-table)))
    (labels ((rec (node)
	       (unless (gethash node visited)
		 (setf (gethash node visited) t)
		 (funcall fn node)
		 (dolist (edge (node-edges node))
		   (rec (edge-next edge))))))
      (rec head-node))))

(defgeneric node-name (node))

(defmethod node-name ((node nfa))
  (write-to-string (nfa-id node)))

(defmethod node-name ((node dfa))
  (format nil "\"~{~a,~}\"" (dfa-id-set node)))

(defgeneric node-graphviz-label (out node))

(defmethod node-graphviz-label ((out stream) (node node))
  (declare (ignore out node)))

(defmethod node-graphviz-label ((out stream) (node dfa))
  (when (dfa-accept-p node)
    (format out "~a [peripheries = 2];~%"
	    (node-name node))))

(defmethod node-graphviz-label ((out stream) (node nfa))
  (cond
    ((nfa-lazy-p node)
     (format out "~a [label = \"~a\\nlazy\"];~%"
	    (node-name node)
	    (node-name node)))
    ((nfa-shift-dfa-p node)
     (format out "~a [label = \"~a\\nshift-dfa\"];~%"
	     (node-name node)
	     (node-name node)))))

(defmethod node-graphviz-label ((out stream) (node back-reference))
  (format out "~a [label = \"back-reference ~a\"];~%"
	  (node-name node)
	  (back-reference-number node)))

(defmethod node-graphviz-label ((out stream) (node nfa-ext))
  (call-next-method)
  (awhen (nfa-ext-expr node)
    (node-to-graphviz-aux out it)))

(macrolet ((def (sym)
	     `(defmethod node-graphviz-label ((out stream) (node ,sym))
		(call-next-method)
		(format out "~a [label = \"~a\\n~a\"];~%"
			(node-name node)
			',sym
			(node-name (nfa-ext-expr node))))))
  (def positive-lookahead)
  (def negative-lookahead)
  (def positive-behind)
  (def negative-behind)
  (def standalone))

(defmethod node-graphviz-label ((out stream) (node register-begin))
  (call-next-method)
  (format out "~a [label = \"~a\\nregister-begin ~a\"];~%"
	  (node-name node)
	  (node-name node)
	  (register-number node)))

(defmethod node-graphviz-label ((out stream) (node register-end))
  (call-next-method)
  (format out "~a [label = \"~a\\nregister-end ~a\"];~%"
	  (node-name node)
	  (node-name node)
	  (register-number node)))

(defmethod node-graphviz-label ((out stream) (node repeat))
  (call-next-method)
  (format out "~a [label = \"~a\\nrepeat ~a ~a\\n~a\\n~a\"];~%"
	  (node-name node)
	  (node-name node)
	  (repeat-min node)
	  (repeat-max node)
	  (cond
	    ((nfa-shift-dfa-p node)
	     "shift-dfa")
	    ((nfa-lazy-p node)
	     "lazy"))
	  (node-name (nfa-ext-expr node))))

(defun node-to-graphviz-aux (out head-node)
  (node-recursive (lambda (node)
		    (node-graphviz-label out node)
		    (dolist (edge (node-edges node))
		      (when (edge-next edge)
			(format out "~a -> ~a [label = \"~a\"];~%"
				(node-name node)
				(node-name (edge-next edge))
				(typecase (edge-cdt edge)
				  (nfa (nfa-id (edge-cdt edge)))
				  (dfa (write-to-string (dfa-id-set (edge-cdt edge))))
				  (t (edge-cdt edge))))
			(when (typep (edge-cdt edge) 'node)
			  (node-to-graphviz-aux out (edge-cdt edge))))))
		  head-node))

(let ((counter 0))
  (defun node-to-graphviz (head-node)
    (let ((name (format nil "tmp/test-~a" (incf counter))))
      (with-open-file (out (format nil "~a.dot" name)
			   :direction :output
			   :if-exists :overwrite
			   :if-does-not-exist :create)
	(format out "digraph test {~%")
	(format out "graph [rankdir = LR];~%")
	(node-to-graphviz-aux out head-node)
	(format out "}~%"))
      (trivial-shell:shell-command
       (format nil "dot -Tgif '~a.dot' -o '~a.gif'" name name))
      name)))

(defgeneric node-walk (fn node visited))

(defmethod node-walk ((fn function) node (visited hash-table))
  (declare (ignore fn node visited)))

(defmethod node-walk ((fn function) (node node) (visited hash-table))
  (unless (gethash node visited)
    (setf (gethash node visited) t)
    (funcall fn node)
    (do-edges (cdt next) (node-edges node)
      (node-walk fn cdt visited)
      (node-walk fn next visited))))

(defmethod node-walk ((fn function) (node nfa-ext) (visited hash-table))
  (unless (gethash node visited)
    (setf (gethash node visited) t)
    (funcall fn node)
    (do-edges (cdt next) (node-edges node)
      (node-walk fn cdt visited)
      (node-walk fn next visited))
    (node-walk fn (nfa-ext-expr node) visited)))

(defun node-mapc (fn node)
  (node-walk fn node (make-hash-table)))
