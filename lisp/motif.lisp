;;;; motif.lisp
;;;; Author: Break Yang <breakds@gmail.com>

(defun motif-enumeration (dna-list pattern-length tolerance)
  (let ((tested (make-hash-table :test #'equal))
        
  
