;;;; motif.lisp
;;;; Author: Break Yang <breakds@gmail.com>

(defun motif-enumeration (dna-list pattern-length tolerance)
  (let ((template (coerce (loop for i below pattern-length collect #\T)
                          'string)))
    ;; This gen-neighbors call generates all the possible pattern with
    ;; length pattern-length.
    (loop for candidate in (gen-neighbors template pattern-length)
       when (every #`,(not (null (approximate-pattern-match x1
                                                            candidate
                                                            tolerance)))
                   dna-list)
       collect candidate)))

(defun best-fit-distance (dna pattern)
  (loop for i from 0 to (- (length dna) (length pattern))
     minimize (hamming-distance pattern
                                (subseq dna 
                                        i (+ i (length pattern))))))

(defun best-fit-score (dna-list pattern)
  (apply #'+ (mapcar #`,(best-fit-distance x1 pattern)
                     dna-list)))

(defun median-string (dna-list pattern-length)
  (let ((template (coerce (loop for i below pattern-length collect #\T)
                          'string))
        (best (list (1+ pattern-length) "")))
    ;; This gen-neighbors call generates all the possible pattern with
    ;; length pattern-length.
    (loop 
       for candidate in (gen-neighbors template pattern-length)
       for score = (best-fit-score dna-list candidate)
       when (< score (car best))
       do (setf best (list score candidate)))
    (cadr best)))
         
  
  
                                  

        
        
  
