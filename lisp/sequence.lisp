;;;; sequence.lisp
;;;; Description: Protein Sequencing Routines.

(in-package #:breakds.bioinfo-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +rna-to-amino+ (make-hash-table :test #'equal))
  (defun init-sequence ()
    (let ((codon-table '((#\I "AUU" "AUC" "AUA")
                         (#\L "CUU" "CUC" "CUA" "CUG" "UUA" "UUG")
                         (#\V "GUU" "GUC" "GUA" "GUG")
                         (#\F "UUU" "UUC")
                         (#\M "AUG")
                         (#\C "UGU" "UGC")
                         (#\A "GCU" "GCC" "GCA" "GCG")
                         (#\G "GGU" "GGC" "GGA" "GGG")
                         (#\P "CCU" "CCC" "CCA" "CCG")
                         (#\T "ACU" "ACC" "ACA" "ACG")
                         (#\S "UCU" "UCC" "UCA" "UCG" "AGU" "AGC")
                         (#\Y "UAU" "UAC")
                         (#\W "UGG")
                         (#\Q "CAA" "CAG")
                         (#\N "AAU" "AAC")
                         (#\H "CAU" "CAC")
                         (#\E "GAA" "GAG")
                         (#\D "GAU" "GAC")
                         (#\K "AAA" "AAG")
                         (#\R "CGU" "CGC" "CGA" "CGG" "AGA" "AGG")
                         (#\* "UAA" "UAG" "UGA"))))
      (loop for codons in codon-table
         do (loop for codon in (rest codons)
               do (setf (gethash codon +rna-to-amino+) 
                        (car codons))))))
  (init-sequence))

(declaim (inline rna-to-amino))
(defun rna-to-amino (rna)
  (coerce (loop for i from 0 below (length rna) by 3
             collect (gethash (subseq rna i (+ i 3))
                              +rna-to-amino+))
          'string))
  


