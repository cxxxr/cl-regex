(in-package #:cl-regex)

(defvar *group-counter*)

(defun parse-bracket (tokens)
  (let ((header
	 (if (eql #\^ (car tokens))
	     (progn
	       (pop tokens)
	       (list :bracket :not))
	   (list :bracket))))
    (let (acc)
      (do ((rest tokens (cdr rest)))
	  ((or (eq (car rest) :bracket-end)
	       (null rest))
	   (values (nconc header (nreverse acc))
		   (cdr rest)))
	(cond
	 ((and acc
	       (eql (car rest) #\-)
	       (cdr rest)
	       (not (eq (cadr rest) :bracket-end)))
	  (push (list (pop acc)
		      (cadr rest))
		acc)
	  (setf rest (cdr rest)))
	 ((and (eql (car rest) #\\)
	       (cdr rest)
	       (not (eq (cadr rest) :bracket-end)))
	  (push (cadr rest) acc))
	 (t
	  (push (car rest) acc)))))))

(defun parse-expand-repeat (n expr lazy-p)
  `(:concat
    ,@(loop repeat n
	    collect expr)
    (:star ,lazy-p
	   ,expr)))

(labels ((retacc (acc)
	   (setq acc (nreverse acc))
	   (cond ((null (cdr acc))
		  (car acc))
		 ((not (keywordp (car acc)))
		  (cons :concat acc))
		 (t
		  acc))))
  (macrolet ((lazy-p (tokens)
	       `(when (eq :question (cadr ,tokens))
		  (pop ,tokens)
		  :lazy)))
    (defun parse-1 (tokens acc)
      (if (null tokens)
	  (retacc acc)
	  (let ((x (car tokens)))
	    (case x
	      ((:plus)
	       (assert acc)
	       (let ((expr (parse-expand-repeat 1
						(car acc)
						(lazy-p tokens))))
		 (parse-1 (cdr tokens)
			  (cons expr (cdr acc)))))
	      ((:star :question)
	       (assert acc)
	       (let ((expr
		       (list x
			     (lazy-p tokens)
			     (car acc))))
		 (parse-1 (cdr tokens)
			  (cons expr (cdr acc)))))
	      ((:or)
	       (multiple-value-bind (expr rest)
		   (parse-1 (cdr tokens) nil)
		 (values (list* :or
				(retacc acc)
				(if (and (consp expr) (eq :or (car expr)))
				    (cdr expr)
				    (list expr)))
			 rest)))
	      ((:register
		:group
		:positive-lookahead
		:negative-lookahead
		:positive-behind
		:negative-behind
		:standalone)
	       (let (group-num)
		 (when (eq x :register)
		   (setf group-num (incf *group-counter*)))
		 (multiple-value-bind (expr rest)
		     (parse-1 (cdr tokens) nil)
		   (let ((expr (if (eq x :register)
				   (list x group-num expr)
				   (list x expr))))
		     (parse-1 rest (cons expr acc))))))
	      ((:closed-paren)
	       (values (retacc acc) (cdr tokens)))
	      ((:bracket-begin)
	       (multiple-value-bind (expr rest)
		   (parse-bracket (cdr tokens))
		 (parse-1 rest
			  (cons expr acc))))
	      ((:dot :head :tail)
	       (parse-1 (cdr tokens) (cons (list x) acc)))
	      (otherwise
	       (cond
		 ((and (consp x) (eq :repeat (car x)))
		  (destructuring-bind (min max) (cdr x)
		    (let ((expr)
			  (lazy-p (lazy-p tokens)))
		      (cond
			((eql min max)
			 (setf expr (cons :concat
					  (loop repeat min
						collect (car acc)))))
			(max
			 (setf expr (list :repeat
					  lazy-p
					  min max (car acc))))
			(t
			 (setf expr (parse-expand-repeat
				     min
				     (car acc)
				     lazy-p))))
		      (parse-1 (cdr tokens)
			       (cons expr (cdr acc))))))
		 ((and (consp x) (eq :back-reference (car x)))
		  (parse-1 (cdr tokens)
			   (cons (list :back-reference (cadr x)) acc)))
		 ((and (stringp x) (= 1 (length x)))
		  (parse-1 (cdr tokens) (cons (list :simple (character x)) acc)))
		 ((or (stringp x) (characterp x))
		  (parse-1 (cdr tokens) (cons (list :simple x) acc)))
		 (t
		  (error "unexpected token: ~a" x))))))))))

(defun parse (tokens)
  (let ((*group-counter* 0))
    (values (parse-1 tokens nil)
	    *group-counter*)))
