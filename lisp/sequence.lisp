;;;; sequence.lisp
;;;; Description: Protein Sequencing Routines.

(in-package #:breakds.bioinfo-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +rna-to-amino+ (make-hash-table :test #'equal))
  (defparameter +max-amino-mass+ 186)
  (defparameter +mass-table+ (make-hash-table))
  (defparameter +candidate-max-decay-rate+ 0.1)
  (defmacro init-mass-table (mass-table)
    `(progn (defun amino-mass (amino)
	      (case amino ,@mass-table (t 0)))
	    (defun lookup-mass (mass)
	      (gethash mass +mass-table+))
	    ,@(mapcar #`(push ,(car x1)
			      (gethash ,(cadr x1) +mass-table+ nil))
		      mass-table)))
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

(defun mass-seq-spectrum (mass-seq)
  (let* ((repeated (append mass-seq mass-seq))
	 (len (length mass-seq))
	 (total-mass (apply #'+ mass-seq))
	 (spectrum (make-hash-table)))
    (incf (gethash 0 spectrum 0))
    (incf (gethash total-mass spectrum 0))
    (labels ((create-accu-sum (seq) 
	       (let ((result (make-array (+ (length seq) 1)
					 :initial-element 0))
		     (accu 0))
		 (loop 
		    for i below (length seq)
		    for mass in seq
		    do (setf (aref result (1+ i))
			     (incf accu mass)))
		 result))
	     (subseq-sum (accu-sum i j)
	       (- (aref accu-sum (+ i j))
		  (aref accu-sum i))))
      (let ((accu-sum (create-accu-sum repeated)))
	(loop for sub-len from 1 to (ash len -1)
	   do (loop 
		 for pos below len
		 for mass = (subseq-sum accu-sum pos sub-len)
		 do (progn (incf (gethash mass spectrum 0))
			   (when (< sub-len (- len sub-len))
			     (incf (gethash (- total-mass mass)
					    spectrum
					    0))))))
	spectrum))))

(defun mass-seq-linear-spectrum (mass-seq)
  (let* ((len (length mass-seq))
	 (total-mass (apply #'+ mass-seq))
	 (spectrum (make-hash-table)))
    (incf (gethash 0 spectrum 0))
    (incf (gethash total-mass spectrum 0))
    (labels ((create-accu-sum (seq) 
	       (let ((result (make-array (+ (length seq) 1)
					 :initial-element 0))
		     (accu 0))
		 (loop
		    for i below (length seq)
		    for mass in seq
		    do (setf (aref result (1+ i))
			     (incf accu mass)))
		 result))
	     (subseq-sum (accu-sum i j)
	       (- (aref accu-sum (+ i j))
		  (aref accu-sum i))))
      (let ((accu-sum (create-accu-sum mass-seq)))
	(loop for sub-len from 1 to (- len 1)
	   do (loop 
		 for pos from 0 to (- len sub-len)
		 for mass = (subseq-sum accu-sum pos sub-len)
		 do (incf (gethash mass spectrum 0)))))
    spectrum)))

(def-string-fun peptide-spectrum (peptide)
  (mass-seq-spectrum (loop for amino across peptide
			collect (amino-mass amino))))

(def-string-fun peptide-linear-spectrum (peptide)
  (mass-seq-linear-spectrum (loop for amino across peptide
                               collect (amino-mass amino))))

(defun spectrum-from-list (mass-list)
  (let ((spectrum (make-hash-table)))
    (loop for mass in mass-list
       do (incf (gethash mass spectrum 0)))
    spectrum))
	  

(defun print-spectrum (spectrum)
  (format t "~a"
          (sort (loop 
		   for mass being the hash-keys of spectrum
		   for count being the hash-values of spectrum
		   append (map-n #`,mass count))
		#'<)))

(defun spectrum-to-list (spectrum)
  (sort 
   (loop
      for mass being the hash-keys of spectrum
      for count being the hash-values of spectrum
      collect (list mass count))
   #2`,(< (car x1) (car x2))))

(defun diff-spectrum (spectrum &optional (extension nil))
  (let ((spec-list (spectrum-to-list spectrum))
	(diff (make-hash-table)))
    (loop for piece on spec-list
       do (loop 
	     for upper in piece
	     for mass-diff = (- (car upper) 
	     			(caar piece))
	     until (< +max-amino-mass+ mass-diff)
	     when (if extension
                      (and (>= mass-diff 57)
                           (<= mass-diff 200))
                      (lookup-mass mass-diff))
	     do (incf (gethash mass-diff diff 0)
	     	      (* (cadr upper)
	     		 (cadar piece)))))
    diff))

(defun identify-candidates (spectrum)
  (let* ((spec-list (sort (spectrum-to-list (diff-spectrum spectrum))
			  #2`,(> (cadr x1)
				 (cadr x2))))
	 (previous (cadar spec-list)))
    (loop 
       for pair in spec-list
       until (> (* previous +candidate-max-decay-rate+) 
                (cadr pair))
       do (setf previous (cadr pair))
       collect (car pair))))


(defun spectrum>= (spec-a spec-b)
  (not (loop 
	  for mass being the hash-keys of spec-b
	  when (not (gethash mass spec-a))
	  return t)))

(defun spectrum= (spec-a spec-b)
  (when (= (hash-table-count spec-a)
	   (hash-table-count spec-b))
    (not (loop 
	    for mass being the hash-keys of spec-a
	    for count being the hash-values of spec-a
	    when (not (gethash mass spec-b))
	    return t))))

(defun sequence-1 (mass-list)
  (let* ((spectrum (spectrum-from-list mass-list))
	 (candidates (identify-candidates spectrum))
	 (total-mass (apply #'max mass-list))
	 answer)
    (format t "candidates: ~{~a ~}~%" candidates)
    (format t "total-mass: ~a~%" total-mass)
    (labels ((expand (pieces) 
	       (loop for piece in pieces
		  append (mapcar #`,(cons x1 piece)
				 candidates)))
	     (consistent-filter (pieces)
	       (let (survived)
		 (loop for piece in pieces
		    do (let ((piece-spec (mass-seq-linear-spectrum piece)))
			 (when (spectrum>= spectrum piece-spec)
			   (if (and (= (apply #'+ piece) total-mass)
			            (spectrum= spectrum 
					       (mass-seq-spectrum piece)))
			       (push piece answer)
			       (push piece survived)))))
		 survived)))
      (let ((pieces '(nil)))
	(loop for new-pieces = (consistent-filter (expand pieces))
	   while new-pieces
	   do (setf pieces new-pieces))))
    answer))

(defun format-sequenced (answer)
  (format t "~{~a~^-~}~%" answer))

(defun spectrum-intersection-score (spec-a spec-b)
  (loop 
     for mass being the hash-keys of spec-b
     for count being the hash-values of spec-b
     sum (min (gethash mass spec-a 0) count)))

(defun identify-candidates-extra (spectrum top-k)
  (let* ((spec-list (sort (spectrum-to-list (diff-spectrum spectrum t))
			  #2`,(> (cadr x1)
				 (cadr x2)))))
    (print spec-list)
    (loop 
       for i below top-k
       for pair in spec-list
       collect (car pair))))

(defun sequence-2 (mass-list &optional 
                               (top-candidate-num 10)
                               (extension 0))
  "Sequencing with leading board. If EXTENSION is specified, we will
  consider extended set of aminos and keep top EXTENSION candidates as
  hypothesis aminos."
  (let* ((spectrum (spectrum-from-list mass-list))
         (candidates (if (zerop extension)
                         (identify-candidates spectrum)
                         (identify-candidates-extra spectrum extension)))
         (total-mass (apply #'max mass-list))
         best)
    (format t "candidates: ~{~a~^ ~}~%" candidates)
    (format t "total-mass: ~a~%" total-mass)
    (labels ((expand (pieces) 
	       (loop for piece in pieces
		  append (mapcar #`,(cons x1 piece)
				 candidates)))
             (try-update (piece)
               (let ((score (spectrum-intersection-score 
                             spectrum
                             (mass-seq-spectrum piece))))
                 (cond ((not best) (setf best (list (list score piece))))
                       ((> score (caar best)) (setf best (list (list score piece))))
                       ((= score (caar best)) (push (list score piece) best)))))
             (linear-score (piece)
               (spectrum-intersection-score
                spectrum
                (mass-seq-linear-spectrum piece)))
             (top-k-with-ties (scored-pieces k &optional tie-score)
               (when (not (null scored-pieces))
                 (cond ((> k 1) (cons (cadar scored-pieces)
                                      (top-k-with-ties (rest scored-pieces)
                                                       (1- k))))
                       ((= k 1) (cons (cadar scored-pieces)
                                      (top-k-with-ties (rest scored-pieces)
                                                       0
                                                       (caar scored-pieces))))
                       ((= (caar scored-pieces) tie-score) 
                        (cons (cadar scored-pieces)
                              (top-k-with-ties (rest scored-pieces)
                                               (1- k)
                                               tie-score)))
                       (t nil))))
             (elect-leaders (pieces)
               (let (survived)
                 (loop for piece in pieces
                    do (let ((mass (apply #'+ piece)))
                         (cond ((= mass total-mass) (try-update piece))
                               ((< mass total-mass) (push (list (linear-score piece) piece)
                                                          survived)))))
                 (top-k-with-ties (sort survived #2`,(> (car x1) (car x2)))
                                  top-candidate-num))))
      (let ((pieces '(nil)))
        (loop for new-pieces = (elect-leaders (expand pieces))
           while new-pieces
           do (setf pieces new-pieces))))
    best))
                 

                 
                      
                 

    
    
    


	
	       
  
		    
      
	 
         
    


    
  


