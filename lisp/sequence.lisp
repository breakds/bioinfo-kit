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

(defun transcribe (dna)
  (let ((rna (copy-seq dna)))
    (loop for i below (length rna)
       when (eq (char rna i) #\T)
       do (setf (char rna i) #\U))
    rna))

(declaim (inline rna-to-amino))
(defun rna-to-amino (rna)
  (coerce (loop for i from 0 to (- (length rna) 3) by 3
             collect (gethash (subseq rna i (+ i 3))
                              +rna-to-amino+))
          'string))

(def-string-fun peptite-all-matches (text peptite)
  (labels ((solve (dna)
	     (let ((occurs (kmp-all-matches (rna-to-amino (transcribe dna))
					    peptite)))
	       (loop for pos in occurs
		  collect (subseq dna (* pos 3) 
				  (* (+ pos (length peptite)) 3))))))
    (let ((rc (reverse-complement text)))
      (append (solve text)
	      (solve (subseq text 1))
	      (solve (subseq text 2))
	      (mapcar #`,(reverse-complement x1) 
		      (solve rc))
	      (mapcar #`,(reverse-complement x1) 
		      (solve (subseq rc 1)))
	      (mapcar #`,(reverse-complement x1) 
		      (solve (subseq rc 2)))))))


    
  


