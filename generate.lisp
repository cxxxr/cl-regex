(in-package #:cl-regex)

(defparameter *state-args* '(@pos0 @str @pos @begin-groups @end-groups))
(defparameter *register-begin-sym* (gensym))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *optimize-default* '(optimize (speed 3) (safety 0))))

(defgeneric gen-state-name (node))

(defmethod gen-state-name ((node nfa))
  (intern (format nil "STATE-~a" (nfa-id node))))

(defmethod gen-state-name ((node dfa))
  (intern (format nil "STATE-~{~a~^-~}" (dfa-id-set node))))

(defun gen-next (node &rest args)
  `(,(gen-state-name node)
    ,@(or args
	  *state-args*)))

(defgeneric gen-transion-aux (cdt next))

(defmethod gen-transion-aux ((cdt null) next)
  (gen-next next))

(defmethod gen-transion-aux ((cdt character) next)
  `(when (and (< @pos (length @str)) (char= ,cdt (schar @str @pos)))
     (let ((@pos (1+ @pos)))
       ,(gen-next next))))

(defmethod gen-transion-aux ((cdt node) next)
  (with-gensyms (gresult)
    `(let ((,gresult ,(gen-next cdt)))
       (when ,gresult
	 (let ((@pos (second ,gresult)))
	   ,(gen-next next))))))

(defmethod gen-transion-aux ((cdt list) next)
  `(when (and
	  (< @pos (length @str))
	  (or ,@(mapcar
		 (lambda (x)
		   (if (consp x)
		       `(char<= ,(first x)
				(schar @str @pos)
				,(second x))
		       `(char= ,x (schar @str @pos))))
		 cdt)))
     (let ((@pos (1+ @pos)))
       ,(gen-next next))))

(defmethod gen-transion-aux ((cdt (eql :head)) next)
  `(when (zerop @pos)
     ,(gen-next next)))

(defmethod gen-transion-aux ((cdt (eql :tail)) next)
  `(when (= @pos (length @str))
     ,(gen-next next)))

(defmethod gen-transion-aux ((cdt (eql :everything)) next)
  `(let ((@pos (1+ @pos)))
     ,(gen-next next)))

(defun gen-transion (edge)
  (if edge
      (gen-transion-aux (edge-cdt edge) (edge-next edge))
      (gen-last)))

(defun gen-last ()
  '(list @pos0 @pos @begin-groups @end-groups))

(defgeneric gen-state (node))

(defmethod gen-state ((node nfa))
  (case (length (node-edges node))
    (0
     (gen-last))
    (1
     (gen-transion (car (node-edges node))))
    (otherwise
     `(or ,@(mapcar #'gen-transion
		    (if (node-lazy-p node)
			(reverse (node-edges node))
			(node-edges node)))))))

(defun take-range (chars)
  (labels ((rec (chars first-char)
	     (cond
	       ((null chars)
		first-char)
	       ((null (cdr chars))
		(cons first-char (car chars)))
	       ((= 1 (- (char-code (second chars))
			(char-code (first chars))))
		(rec (cdr chars) first-char))
	       (t
		(values (cons first-char (car chars))
			(cdr chars))))))
    (multiple-value-bind (result rest)
	(rec chars (car chars))
      (values
       (if (char= (car result) (cdr result))
	   (car result)
	   result)
       rest))))

(defun parse-range-list (chars)
  (setf chars (sort chars #'char<=))
  (labels ((rec (chars)
	     (when chars
	       (multiple-value-bind (range rest-chars)
		   (take-range chars)
		 (cons range
		       (rec rest-chars))))))
    (rec chars)))

(defun gen-cond-range-chars (range-list next)
  `((or ,@(mapcar
	   (lambda (range)
	     (if (consp range)
		 `(char<= ,(car range)
			  (schar @str @pos)
			  ,(cdr range))
		 `(char= (schar @str @pos)
			 ,range)))
	   range-list))
    (let ((@pos (1+ @pos)))
      ,(gen-next next))))

(defun gen-compression-edges (edges other)
  (let ((hash (make-hash-table :test 'eq)))
    (do-edges (cdt next) edges
      (when (characterp cdt)
	(push cdt (gethash next hash))))
    `(,@(loop :for next :being :each :hash-key :of hash
		:using (hash-value cdt-list)
	      :collect (gen-cond-range-chars
			(parse-range-list cdt-list)
			next))
      (t
       ,other))))

(defmethod gen-state ((node dfa))
  (let ((other (when (dfa-accept-p node) (gen-last))))
    `(cond
       ,@(loop for edge in (node-edges node)
	       if (eq :head (edge-cdt edge))
		 collect `((zerop @pos)
			   ,(gen-next (edge-next edge)))
	       if (eq :tail (edge-cdt edge))
		 collect `((= @pos (length @str))
			   ,(gen-next (edge-next edge))))
       ((>= @pos (length @str))
	,other)
       ,@(gen-compression-edges (node-edges node) other))))

(defmethod gen-state ((node back-reference))
  (let ((index (1- (back-reference-number node))))
    (with-gensyms (gstart1 gend1 glen)
      `(let* ((,gstart1 (aref @begin-groups ,index))
	      (,gend1 (aref @end-groups ,index))
	      (,glen (- ,gend1 ,gstart1)))
	 (when (and
		(<= (+ @pos ,glen) (length @str))
		(string= @str
			 @str
			 :start1 ,gstart1
			 :end1 ,gend1
			 :start2 @pos
			 :end2 (+ @pos ,glen)))
	   (let ((@pos (+ @pos ,glen)))
	     ,(if (node-edges node)
		  (gen-next (car (node-edges node)))
		  (gen-last))))))))

(defmethod gen-state ((node positive-lookahead))
  `(when ,(gen-next (nfa-ext-expr node))
     ,(gen-transion (car (node-edges node)))))

(defmethod gen-state ((node negative-lookahead))
  `(unless ,(gen-next (nfa-ext-expr node))
     ,(gen-transion (car (node-edges node)))))

(defmethod gen-state ((node positive-behind))
  (with-gensyms (gresult)
    `(let ((,gresult ,(gen-next (nfa-ext-expr node))))
       (when ,gresult
	 (let ((@pos0 (second ,gresult))
	       (@pos (second ,gresult)))
	   ,(gen-transion (car (node-edges node))))))))

(defmethod gen-state ((node negative-behind))
  `(unless ,(gen-next (nfa-ext-expr node))
     (let ((@pos0 (+ @pos0 ,(negative-behind-length node)))
	   (@pos (+ @pos ,(negative-behind-length node))))
       ,(gen-transion (car (node-edges node))))))

(defmethod gen-state ((node standalone))
  (with-gensyms (gresult)
    `(let ((,gresult ,(gen-next (nfa-ext-expr node))))
       (when ,gresult
	 (let ((@pos (second ,gresult)))
	   ,(gen-transion (car (node-edges node))))))))

(defun gen-repeat-min (expr min)
  (with-gensyms (gcount gresult)
    `(loop for ,gcount from 1 to ,min
	   do (let ((,gresult ,(gen-next expr)))
		(if ,gresult
		    (setf @pos (second ,gresult))
		    (return nil)))
	   finally (return t))))

(defun gen-repeat (min max expr edge)
  (with-gensyms (grec gcount gresult)
    `(and
      ,(gen-repeat-min expr min)
      (labels ((,grec (,gcount @pos)
		 (if (>= ,gcount ,(- max min))
		     ,(gen-transion edge)
		     (let ((,gresult ,(gen-next expr)))
		       (if ,gresult
			   (or (,grec (1+ ,gcount)
				      (second ,gresult))
			       ,(gen-transion edge))
			   ,(gen-transion edge))))))
	(,grec 0 @pos)))))

(defun gen-repeat-lazy (min max expr edge)
  (with-gensyms (gcount gresult)
    `(and
      ,(gen-repeat-min expr min)
      (or
       (loop for ,gcount from ,min below ,max
	     do (let ((,gresult ,(gen-next expr)))
		  (when ,gresult
		    ,(if (not edge)
			 `(return ,gresult)
			 `(let* ((@pos (second ,gresult))
				 (,gresult ,(gen-transion edge)))
			    (when ,gresult
			      (return ,gresult)))))))
       ,(when edge
	  (gen-transion edge))))))

(defmethod gen-state ((node repeat))
  (let ((min (repeat-min node))
	(max (repeat-max node))
	(expr (nfa-ext-expr node))
	(edge (car (node-edges node))))
    (if (node-lazy-p node)
	(gen-repeat-lazy min max expr edge)
	(gen-repeat min max expr edge))))

(defmethod gen-state ((node register-begin))
  `(progn
     (let ((,*register-begin-sym* @pos))
       (declare (special ,*register-begin-sym*))
       ,(gen-transion (car (node-edges node))))))

(defmethod gen-state ((node register-end))
  `(progn
     (declare (special ,*register-begin-sym*))
     (setf (aref @begin-groups ,(1- (register-number node)))
	   ,*register-begin-sym*)
     (setf (aref @end-groups ,(1- (register-number node)))
	   @pos)
     ,(if (node-edges node)
	  (gen-transion (car (node-edges node)))
	  (gen-last))))

(defun gen-states (head-node num-groups)
  (let ((states))
    (node-mapc (lambda (node)
		 (push
		  `(,(gen-state-name node) ,*state-args*
		    (declare (ignorable ,@*state-args*))
		    (declare (type fixnum @pos0 @pos)
			     (type string @str)
			     (type (simple-vector ,num-groups)
				   @begin-groups
				   @end-groups))
		    ,@(let ((body (gen-state node)))
			(if (and (consp body) (eq (car body) 'progn))
			    (cdr body)
			    (list body))))
		  states))
	       head-node)
    (nreverse states)))

(defun fa-to-lisp (head-node num-groups)
  (let ((states (gen-states head-node num-groups)))
    (with-gensyms (gstr gi gresult)
      `(locally (declare ,*optimize-default*)
	 (labels ,states
	   (lambda (,gstr)
	     (loop for ,gi from 0 below (length ,gstr)
		   do (let ((,gresult
			      (,(gen-state-name head-node)
			       ,gi
			       ,gstr
			       ,gi
			       #1=(make-array ,num-groups :initial-element nil)
			       #1#)))
			(when ,gresult
			  (return (apply #'values ,gresult)))))))))))
