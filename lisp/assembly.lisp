;;;; assmebly.lisp
;;;; Description: DNA Assembly and its friends.

(defun dna-composition (dna subseq-length)
  (sort (loop for i from 0 to (- (length dna) subseq-length)
           collect (subseq dna i (+ i subseq-length))) 
        #'string-lessp))
  

