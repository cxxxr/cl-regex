(in-package #:cl-regex)

(defmacro with-accumlate (reverse-p &body body)
  (with-gensyms (greturn-values)
    `(let ((,greturn-values))
       (labels ((collect (x)
		  (push x ,greturn-values)))
	 ,@body
	 (if ,reverse-p
	     ,greturn-values
	     (nreverse ,greturn-values))))))

(define-modify-macro conc1f (obj)
  (lambda (place obj)
    (nconc place (list obj))))

(declaim (inline bool))

(defun bool (x)
  (not (null x)))
