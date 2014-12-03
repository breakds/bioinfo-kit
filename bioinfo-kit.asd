(asdf:defsystem #:bioinfo-kit
    :serial t
    :depends-on (:basicl
                 :struct-wrapper)
    :components ((:file "lisp/packages")
		 (:file "lisp/utilities")
		 (:file "lisp/pattern-match")
                 (:file "lisp/oric")
                 (:file "lisp/sequence")
                 (:file "lisp/motif")
                 (:file "lisp/eulerian")
                 (:file "lisp/assembly")))


