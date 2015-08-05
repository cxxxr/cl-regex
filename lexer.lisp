(in-package #:cl-regex)

#|

. -> :dot
* -> :star
+ -> :plus
? -> :question
^ -> :head
$ -> :tail
(regexp) -> :register regexp :closed-paren
\n -> (:back-reference n)
[char-list] -> :bracket-begin char-list... :bracket-end
{n,m} -> (:repeat n m)
{n,} -> (:repeat n T)
{n} -> (:repeat n n)
| -> :or
(?# text) -> comment
(?:regexp) -> :group regexp :closed-paren
(?=regexp) -> :positive-lookahead regexp :closed-paren
(?!regexp) -> :negative-lookahead regexp :closed-paren
*? -> :star :question
+? -> :plus :question
?? -> :question :question
{n,m}? -> (:repeat n m) :question
{n,}? -> (:repeat n T) :question
(n}? -> (:repeat n n) :question
(?<=pattern) -> :positive-behind pattern :closed-paren
(?<!pattern) -> :negative-behind pattern :closed-paren
(?>pattern) -> :standalone pattern :closed-paren

|#

(defparameter *escape-sequences*
  '((#\a . #\Bel)
    (#\b . #\Backspace)
    (#\t . #\Tab)
    (#\n . #\Newline)
    (#\f . #\Page)
    (#\r . #\Return)))

(defparameter *alias-char-class*
  '((#\d . "[0-9]")
    (#\D . "[^0-9]")
    (#\w . "[a-zA-Z0-9]")
    (#\W . "[^a-zA-Z0-9]")
    (#\s . "[ \\t\\f\\r\\n]")
    (#\S . "[^ \\t\\f\\r\\n]")))

;; (文字
;;  それを表わすシンボル
;;  一つ前の文字列を分割するフラグ
;;  *special-chars*の要素になるか)
(defparameter *special-char-table*
  '((#\( :register nil t)
    (#\) :closed-paren nil t)
    (#\[ :bracket-begin nil t)
    (#\] :bracket-end nil t)
    (#\| :or nil t)
    (#\. :dot nil t)
    (#\* :star t t)
    (#\+ :plus t t)
    (#\? :question t t)
    (#\{ '#:dummy t nil)))

(defparameter *special-chars*
  (mapcan (lambda (elt)
	    (if (fourth elt)
		(list (car elt))))
	  *special-char-table*))

(defstruct (lexer (:constructor make-lexer-internal))
  (str "" :type string :read-only t)
  (len 0 :type fixnum :read-only t)
  (pos 0 :type fixnum))

(defun make-lexer (str)
  (make-lexer-internal :str str
		       :len (length str)))

(defun lex-end-p (lexer)
  (<= (lexer-len lexer)
      (lexer-pos lexer)))

(defun lex-peek-char (lexer)
  (schar (lexer-str lexer)
	 (lexer-pos lexer)))

(defun lex-next-char-1 (lexer)
  (prog1 (lex-peek-char lexer)
    (incf (lexer-pos lexer))))

(defun lex-next-char (lexer)
  (unless (lex-end-p lexer)
    (let ((c1 (lex-next-char-1 lexer)))
      (if (lex-end-p lexer)
	  (values c1 nil)
	  (values c1 (lex-peek-char lexer))))))

(defun lex-skip-char (lexer)
  (incf (lexer-pos lexer)))

(defmacro release-acc (acc)
  `(prog1 (coerce (nreverse ,acc) 'string)
     (setq ,acc nil)))

(defun lex-digits (lexer)
  (loop with acc
	do (let* ((c1 (lex-peek-char lexer)))
	     (if (digit-char-p c1)
		 (progn
		   (push c1 acc)
		   (lex-skip-char lexer))
		 (return (if acc
			     (parse-integer
			      (coerce (nreverse acc)
				      'string))))))))

(defun lex-repeat-2 (lexer)
  (let* ((pos
	   (lexer-pos lexer))
	 (result
	   (block nil
	     (let ((n)
		   (m nil))
	       (setf n (lex-digits lexer))
	       (unless n (return))
	       (case (lex-next-char-1 lexer)
		 (#\,
		  (when (char= #\} (lex-peek-char lexer))
		    (lex-skip-char lexer)
		    (return (list n m)))
		  (setf m (lex-digits lexer))
		  (unless m (return))
		  (return (if (char= #\} (lex-next-char-1 lexer))
			      (list n m))))
		 (#\}
		  (return (list n n)))
		 (t
		  (return)))))))
    (or result
	(progn
	  (setf (lexer-pos lexer) pos)
	  nil))))

(defun lex-repeat-1 (lexer)
  (let ((nums (lex-repeat-2 lexer)))
    (if nums
	`(:repeat ,@nums)
	#\{)))

(defun lex-paren-question (lexer)
  (lex-skip-char lexer)
  (let ((pos (lexer-pos lexer)))
    (let ((result (case (lex-next-char-1 lexer)
		    (#\#
		     (loop until (char= (lex-next-char-1 lexer) #\)))
		     t)
		    (#\: (list :group))
		    (#\= (list :positive-lookahead))
		    (#\! (list :negative-lookahead))
		    (#\<
		     (case (lex-next-char-1 lexer)
		       (#\= (list :positive-behind))
		       (#\! (list :negative-behind))))
		    (#\> (list :standalone)))))
      (or result
	  (progn
	    (setf (lexer-pos lexer) pos)
	    nil)))))

(defun lex-special-char (lexer c1 c2)
  (with-accumlate nil
    (aif (and (char= c1 #\()
	      (char= c2 #\?)
	      (lex-paren-question lexer))
	 (when (consp it) (mapc #'collect it))
	 (progn
	   (collect (second (assoc c1 *special-char-table*)))
	   (when (and (or (char= c1 #\()
			  (char= c1 #\|))
		      (char= c2 #\^))
	     (lex-skip-char lexer)
	     (collect :head))))))

(defun lex-slash (lexer c1 c2)
  (declare (ignore c1))
  (with-accumlate nil
    (acond
     ((cdr (assoc c2 *escape-sequences*))
      (collect it)
      (lex-skip-char lexer))
     ((cdr (assoc c2 *alias-char-class*))
      (dolist (tok (lex-tokens-1 (make-lexer it)))
	(collect tok))
      (lex-skip-char lexer))
     ((and (characterp c2) (digit-char-p c2))
      (collect `(:back-reference ,it))
      (lex-skip-char lexer))
     (c2
      (collect c2)
      (lex-skip-char lexer))
     (t
      (collect #\\)))))

(defun lex-tokens-1 (lexer)
  (with-accumlate nil
    (when (char= #\^ (lex-peek-char lexer))
      (lex-skip-char lexer)
      (collect :head))
    (let (mode-fn)
      (labels ((brace-mode (c1 c2)
		 (acond
		  ((char= c1 #\])
		   (setf mode-fn #'normal-mode)
		   (collect :bracket-end))
		  ((and (char= c1 #\\)
			(cdr (assoc c2 *escape-sequences*)))
		   (collect it)
		   (lex-skip-char lexer))
		  (t
		   (collect c1))))
	       (normal-mode (c1 c2)
		 (cond
		   ((char= c1 #\[)
		    (collect :bracket-begin)
		    (setf mode-fn #'brace-mode))
		   ((and (char= c1 #\$) (member c2 '(nil #\) #\|)))
		    (collect :tail))
		   ((eql c1 #\{)
		    (collect (lex-repeat-1 lexer)))
		   ((member c1 *special-chars*)
		    (mapc #'collect (lex-special-char lexer c1 c2)))
		   ((char= c1 #\\)
		    (mapc #'collect (lex-slash lexer c1 c2)))
		   (t
		    (collect c1)))))
	(setf mode-fn #'normal-mode)
	(loop until (lex-end-p lexer)
	      do (multiple-value-bind (c1 c2)
		     (lex-next-char lexer)
		   (funcall mode-fn c1 c2)))))))

(defun lex-tokens (str)
  (lex-tokens-1 (make-lexer str)))
