;;;; sequence.lisp
;;;; Description: Protein Sequencing Routines.

(in-package #:breakds.bioinfo-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +rna-to-amino+ (make-hash-table :test #'equal))
  (defparameter +mass-table+ (make-hash-table))
  (defmacro init-mass-table (mass-table)
    (progn (setf +mass-table+ (make-hash-table))
           (loop for pair in mass-table
              do (push (car pair) (gethash (cadr pair)
                                           +mass-table+
                                           nil)))
           `(progn (defun amino-mass (amino)
                     (case amino ,@mass-table (t 0)))
                   (defun lookup-mass (mass)
                     (gethash mass +mass-table+)))))
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
  (init-mass-table ((#\G 57)
                    (#\A 71)
                    (#\S 87)
                    (#\P 97)
                    (#\V 99)
                    (#\T 101)
                    (#\C 103)
                    (#\I 113)
                    (#\L 113)
                    (#\N 114)
                    (#\D 115)
                    (#\K 128)
                    (#\Q 128)
                    (#\E 129)
                    (#\M 131)
                    (#\H 137)
                    (#\F 147)
                    (#\R 156)
                    (#\Y 163)
                    (#\W 186)))
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

(def-string-fun peptide-all-matches (text peptide)
  (labels ((solve (dna)
	     (let ((occurs (kmp-all-matches (rna-to-amino (transcribe dna))
					    peptide)))
	       (loop for pos in occurs
		  collect (subseq dna (* pos 3) 
				  (* (+ pos (length peptide)) 3))))))
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

(declaim (inline amino-mass))
(defun amino-mass (amino)
  (case amino
    (#\G 57)
    (#\A 71)
    (#\S 87)
    (#\P 97)
    (#\V 99)
    (#\T 101)
    (#\C 103)
    (#\I 113)
    (#\L 113)
    (#\N 114)
    (#\D 115)
    (#\K 128)
    (#\Q 128)
    (#\E 129)
    (#\M 131)
    (#\H 137)
    (#\F 147)
    (#\R 156)
    (#\Y 163)
    (#\W 186)
    (t 0)))

(defun peptide-mass (peptide)
  (loop for amino across peptide
     sum (amino-mass amino)))

(def-string-fun peptide-spectrum (peptide)
  (let* ((repeated (concatenate 'string peptide peptide))
         (len (length peptide))
         (total-mass (peptide-mass peptide))
         (spectrum (make-hash-table)))
    (loop for sub-len from 1 to (ash len -1)
       do (loop 
             for pos below len
             for mass = (peptide-mass (subseq repeated pos 
                                              (+ pos sub-len)))
             do (progn (incf (gethash mass spectrum 0))
                       (when (< sub-len (- len sub-len))
                         (incf (gethash (- total-mass mass)
                                        spectrum
                                        0))))))
    spectrum))

(defun print-spectrum (spectrum)
  (format t "~a"
          (loop 
             for mass being the hash-keys of spectrum
             for count being the hash-values of spectrum
             append (map-n #`,mass count))))

         
    


    
  


