;;;; cl-regex.lisp

(in-package #:cl-regex)

(defun parse-regex (str)
  (parse (lex-tokens str)))

(defun re-compile (str)
  (multiple-value-bind (parse-tree num-groups)
      (parse-regex str)
    (fa-to-lisp (convert parse-tree) num-groups)))

(defun define-match (name str)
  (setf (symbol-function name) (eval (re-compile str))))

(defun re-graphviz (str)
  (node-to-graphviz (convert (parse-regex str))))

(defun match (regex str)
  (funcall (eval (re-compile regex)) str))
