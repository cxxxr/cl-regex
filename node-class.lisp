(in-package #:cl-regex)

(defclass node ()
  ((edges
    :initform nil
    :initarg :edges
    :accessor node-edges
    :type list)))

(defclass nfa (node)
  ((id
    :initarg :id
    :reader nfa-id
    :type fixnum)
   (shift-dfa-p
    :initarg :shift-dfa-p
    :initform nil
    :reader nfa-shift-dfa-p
    :type boolean)
   (lazy-p
    :initarg :lazy-p
    :initform nil
    :reader nfa-lazy-p
    :type boolean)))

(defclass dfa (node)
  ((id-set
    :initarg :id-set
    :reader dfa-id-set
    :type cons)
   (accept-p
    :initarg :accept-p
    :reader dfa-accept-p
    :type boolean)))

(defclass nfa-ext (nfa)
  ((expr
    :initarg :expr
    :reader nfa-ext-expr
    :type node)))

(defclass back-reference (nfa)
  ((number
    :initarg :number
    :reader back-reference-number
    :type fixnum)))

(defclass positive-lookahead (nfa-ext)
  ())

(defclass negative-lookahead (nfa-ext)
  ())

(defclass positive-behind (nfa-ext)
  ())

(defclass negative-behind (nfa-ext)
  ((length
    :initarg :length
    :reader negative-behind-length
    :type fixnum)))

(defclass standalone (nfa-ext)
  ())

(defclass register-base (nfa)
  ((number
    :initarg :number
    :reader register-number
    :type fixnum)))

(defclass register-begin (register-base) ())
(defclass register-end (register-base) ())

(defclass repeat (nfa-ext)
  ((min
    :initarg :min
    :reader repeat-min
    :type fixnum)
   (max
    :initarg :max
    :reader repeat-max
    :type fixnum)))

(defstruct edge
  (cdt nil :read-only t)
  (next nil :type node :read-only t))
