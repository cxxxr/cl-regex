(in-package #:cl-regex)

(defvar *able-shift-dfa-p*)

(defgeneric nfa-to-dfa (head tail))

(defmethod nfa-to-dfa (head tail)
  (declare (ignore tail))
  head)

(defun collect-transion-e-aux (node visited)
  (let ((acc-nodes))
    (labels ((rec (node)
	       (unless (gethash node visited)
		 (setf (gethash node visited) t)
		 (push node acc-nodes)
		 (dolist (edge (node-edges node))
		   (when (and (null (edge-cdt edge))
			      (node-shift-dfa-p (edge-next edge)))
		     (rec (edge-next edge)))))))
      (rec node))
    acc-nodes))

(defun collect-transion-e (nodes)
  (let ((visited (make-hash-table :test #'eq)))
    (mapcan (lambda (node)
	      (collect-transion-e-aux node visited))
	    nodes)))

(defun collect-not-e-edges (nodes)
  (let ((table (make-hash-table :test #'eq)))
    (dolist (node nodes)
      (do-edges (cdt next) (node-edges node)
	(when (and next (node-shift-dfa-p next) cdt)
	  (cond
	    ((eq cdt :everything)
	     (loop for c from 0 to 255
		   do (push next (gethash (code-char c) table))))
	    ((consp cdt)
	     (dolist (x cdt)
	       (if (consp x)
		   (loop for c
			 from (char-code (first x))
			   to (char-code (second x))
			 do (push next (gethash (code-char c) table)))
		   (push next (gethash x table)))))
	    (t
	     (push next (gethash cdt table)))))))
    (hash-table-alist table)))

(defmethod nfa-to-dfa ((head nfa) (tail nfa))
  (cond
    ((not *able-shift-dfa-p*)
     head)
    ((not (node-shift-dfa-p head))
     (error "error"))
    (t
     (let ((id-set-table (make-hash-table :test #'equal)))
       (labels ((rec (start-nodes)
		  (let* ((nodes (collect-transion-e start-nodes))
			 (id-set (mapcar #'nfa-id nodes)))
		    (or (gethash id-set id-set-table)
			(let ((dfa-node
				(make-instance
				 'dfa
				 :id-set id-set
				 :accept-p (member (nfa-id tail) id-set))))
			  (setf (gethash id-set id-set-table) dfa-node)
			  (dolist (elt (collect-not-e-edges nodes))
			    (destructuring-bind (cdt . next-nodes) elt
			      (node-connect dfa-node cdt (rec next-nodes))))
			  dfa-node)))))
	 (rec (list head)))))))

(defmacro define-parse-tree-method (defname method-name-aux call-method-name)
  (with-gensyms (gname gargs gbody g2token g2args gparse-tree)
    `(progn
       (defgeneric ,method-name-aux (name args))
       (defmacro ,defname (,gname ,gargs &body ,gbody)
	 (with-gensyms (,g2token ,g2args)
	   `(defmethod ,',method-name-aux ((,,g2token (eql ,,gname)) ,,g2args)
	      (destructuring-bind ,,gargs ,,g2args
		,@,gbody))))
       (defun ,call-method-name (,gparse-tree)
	  (,method-name-aux (car ,gparse-tree) (cdr ,gparse-tree))))))

(define-parse-tree-method
    define-convert
  convert-aux
  convert-1)

(define-parse-tree-method
    define-parse-tree-length
  parse-tree-length-aux
  parse-tree-length)

(defun convert (parse-tree)
  (let ((*node-counter* 0)
	(*able-shift-dfa-p* t))
    (multiple-value-bind (head tail shift-dfa-p)
	(convert-1 parse-tree)
      (if shift-dfa-p
      	  (nfa-to-dfa head tail)
      	  head))))

(define-convert :star (lazy-p parse-tree)
  (multiple-value-bind (head tail shift-dfa-p)
      (convert-1 parse-tree)
    (cond (lazy-p
	   (when shift-dfa-p
	     (setf head (nfa-to-dfa head tail)))
	   (let ((node (make-instance 'nfa :lazy-p t)))
	     (node-connect node head node)
	     (values node node nil)))
	  ((and shift-dfa-p *able-shift-dfa-p*)
	   (let ((star-head (make-instance 'nfa :shift-dfa-p t))
		 (star-tail (make-instance 'nfa :shift-dfa-p t)))
	     (node-connect star-head nil head)
	     (node-connect star-head nil star-tail)
	     (node-connect tail nil head)
	     (node-connect tail nil star-tail)
	     (values star-head star-tail t)))
	  (t
	   (let ((node (make-instance 'nfa)))
	     (node-connect node head node)
	     (values node node nil))))))

(define-convert :question (lazy-p parse-tree)
  (multiple-value-bind (head tail shift-dfa-p)
      (convert-1 parse-tree)
    (cond (lazy-p
	   (when shift-dfa-p
	     (setf head (nfa-to-dfa head tail)))
	   (let ((q-head (make-instance 'nfa :lazy-p t))
		 (q-tail (make-instance 'nfa)))
	     (node-connect q-head head q-tail)
	     (node-connect q-head nil q-tail)
	     (values q-head q-tail nil)))
	  (shift-dfa-p
	   (node-connect head nil tail)
	   (values head tail t))
	  (t
	   (let ((q-head (make-instance 'nfa))
		 (q-tail (make-instance 'nfa)))
	     (node-connect q-head head q-tail)
	     (node-connect q-head nil q-tail)
	     (values q-head q-tail nil))))))

(define-convert :group (parse-tree)
  (convert-1 parse-tree))

(define-convert :back-reference (group-num)
  (let ((node (make-instance 'back-reference :number group-num)))
    (values node node nil)))

(define-convert :or (&rest parse-tree-list)
  (let* ((nodes (mapcar (lambda (parse-tree)
			  (multiple-value-bind (head tail shift-dfa-p)
			      (convert-1 parse-tree)
			    (list head tail shift-dfa-p)))
			parse-tree-list))
	 (shift-dfa-p (not (find nil nodes :key #'third)))
	 (or-head (make-instance 'nfa :shift-dfa-p shift-dfa-p))
	 (or-tail (make-instance 'nfa :shift-dfa-p shift-dfa-p)))
    (loop for (head tail) in nodes
	  do (node-connect or-head nil head)
	     (node-connect tail nil or-tail))
    (values or-head or-tail (and *able-shift-dfa-p* shift-dfa-p))))

(define-convert :register (group-num parse-tree)
  (let ((*able-shift-dfa-p*))
    (let ((register-head (make-instance 'register-begin :number group-num))
	  (register-tail (make-instance 'register-end :number group-num)))
      (multiple-value-bind (head tail)
	  (convert-1 parse-tree)
	(node-connect register-head nil head)
	(node-connect tail nil register-tail))
      (values register-head register-tail nil))))

(defmacro convert-nfa-ext-form (parse-tree &rest args)
  (with-gensyms (ghead gtail g-shift-dfa-p gnode)
    `(multiple-value-bind (,ghead ,gtail ,g-shift-dfa-p)
	 (convert-1 ,parse-tree)
       (when ,g-shift-dfa-p
	 (setf ,ghead (nfa-to-dfa ,ghead ,gtail)))
       (let ((,gnode (make-instance ,@args :expr ,ghead)))
	 (values ,gnode ,gnode nil)))))

(define-convert :repeat (lazy-p min max parse-tree)
  (convert-nfa-ext-form parse-tree
			'repeat
			:min min
			:max max
			:type (if lazy-p :lazy)))

(define-convert :positive-lookahead (parse-tree)
  (convert-nfa-ext-form parse-tree
			'positive-lookahead))

(define-convert :negative-lookahead (parse-tree)
  (convert-nfa-ext-form parse-tree
			'negative-lookahead))


(define-convert :positive-behind (parse-tree)
  (convert-nfa-ext-form parse-tree
			'positive-behind))

(defmethod parse-tree-length-aux (name args)
  (declare (ignore name args))
  0)

(define-parse-tree-length :group (parse-tree)
  (parse-tree-length parse-tree))

(define-parse-tree-length :register (parse-tree)
  (parse-tree-length parse-tree))

(define-parse-tree-length :concat (&rest parse-tree-list)
  (loop for parse-tree in parse-tree-list
	sum (parse-tree-length parse-tree)))

(define-parse-tree-length :simple (parse-tree)
  (declare (ignore parse-tree))
  1)

(define-parse-tree-length :bracket (&rest parse-tree)
  (declare (ignore parse-tree))
  1)

(define-parse-tree-length :dot ()
  1)

(define-parse-tree-length :or (parse-tree)
  (parse-tree-length (car parse-tree)))

(define-convert :negative-behind (parse-tree)
  (convert-nfa-ext-form parse-tree
			'negative-behind
			:length (parse-tree-length parse-tree)))

(define-convert :standalone (parse-tree)
  (convert-nfa-ext-form parse-tree
			'standalone))

(defun flatten-concat (parse-tree-list)
  (mapcan (lambda (parse-tree)
	    (if (eq :concat (car parse-tree))
		(cdr parse-tree)
		(list parse-tree)))
	  parse-tree-list))

(defun concat-connects (nodes)
  (labels ((rec (nodes)
	     (if (null (cdr nodes))
		 (apply #'values (car nodes))
		 (destructuring-bind (head1 tail1 &rest rest) (car nodes)
		   (declare (ignore rest))
		   (multiple-value-bind (head2 tail2) (rec (cdr nodes))
		     (cond
		       ((and (not (typep head2 'nfa-ext))
			     (not (typep head2 'register-base))
			     (typep head2 'nfa))
			(loop for edge in (node-edges head2)
			      do (node-connect tail1
					       (edge-cdt edge)
					       (edge-next edge)))
			(values head1 tail2))
		       (t
			(node-connect tail1 nil head2)
			(values head1 tail2))))))))
    (rec nodes)))

(define-convert :concat (&rest parse-tree-list)
  (setf parse-tree-list (flatten-concat parse-tree-list))
  (let ((nodes (mapcar (lambda (parse-tree)
			 (multiple-value-list (convert-1 parse-tree)))
		       parse-tree-list)))
    (let ((new-nodes))
      (or
       (loop with rest = nodes
	     while rest
	     do (destructuring-bind (head tail shift-dfa-p)
		    (car rest)
		  (declare (ignore head tail))
		  (if (or (not shift-dfa-p) (not *able-shift-dfa-p*))
		      (push (pop rest) new-nodes)
		      (let ((acc))
			(loop while (and rest (third (car rest)))
			      do (push (butlast (pop rest)) acc))
			(if (null rest)
			    (setf new-nodes (nconc acc new-nodes))
			    (let ((node (multiple-value-call
					    #'nfa-to-dfa
					  (concat-connects (nreverse acc)))))
			      (if (and (not rest) (not new-nodes))
				  (return node)
				  (let ((head (make-instance 'nfa :shift-dfa-p t))
					(tail (make-instance 'nfa :shift-dfa-p t)))
				    (node-connect head node tail)
				    (push (list head tail) new-nodes)))))))))
       (multiple-value-bind (head tail)
	   (concat-connects (nreverse new-nodes))
	 (values head
		 tail
		 (not (find-if-not #'third nodes))))))))

(defun convert-simple (x)
  (let ((head (make-instance 'nfa :shift-dfa-p t))
	(tail (make-instance 'nfa :shift-dfa-p t)))
    (node-connect head x tail)
    (values head tail t)))

(defun bracket-member (c chars)
  (dolist (elt chars)
    (if (if (consp elt)
	    (char<= (car elt) c (cadr elt))
	    (member c chars))
	(return t))))

(defun bracket-not (chars)
  (loop for code from 0 to 255
	for ch = (code-char code)
	unless (bracket-member ch chars)
	  collect ch))

(define-convert :bracket (&rest rest)
  (if (and rest (eq :not (car rest)))
      (convert-simple (bracket-not (cdr rest)))
      (convert-simple rest)))

(define-convert :head ()
  (convert-simple :head))

(define-convert :tail ()
  (convert-simple :tail))

(define-convert :dot ()
  (convert-simple :everything))

(define-convert :simple (x)
  (convert-simple x))
