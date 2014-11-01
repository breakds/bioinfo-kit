(asdf:defsystem #:bioinfo-kit
    :serial t
    :depends-on (:basicl)
    :components ((:file "lisp/packages")
		 (:file "lisp/utilities")
		 (:file "lisp/pattern-match")))


