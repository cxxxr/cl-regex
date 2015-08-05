;;;; cl-regex.asd

(asdf:defsystem #:cl-regex
  :description "Describe cl-regex here"
  :depends-on (#:anaphora
               #:alexandria
	       #:trivial-shell)
  :serial t
  :components ((:file "package")
               (:file "cl-regex")
	       (:file "util")
	       (:file "lexer")
	       (:file "parser")
	       (:file "node-class")
	       (:file "node")
	       (:file "convert")
	       (:file "generate")))
