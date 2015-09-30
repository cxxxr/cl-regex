;;;; cl-regex.lisp

(in-package #:cl-regex)

(defun parse-regex (str)
  (parse (lex-tokens str)))

(defun re-graphviz (str)
  (node-to-graphviz (convert (parse-regex str))))

(defun re-to-lisp (str)
  (multiple-value-bind (parse-tree num-groups)
      (parse-regex str)
    (fa-to-lisp (convert parse-tree) num-groups)))

(defun re-compile (str)
  (eval (re-to-lisp str)))

(defun match (regex str)
  (funcall (eval (re-compile regex)) str))

(define-compiler-macro match (&whole form regex string)
  (declare (ignore form))
  (if (stringp regex)
      `(funcall ,(re-compile regex) ,string)
      `(match ,regex ,string)))
