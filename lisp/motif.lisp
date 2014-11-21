;;;; motif.lisp
;;;; Author: Break Yang <breakds@gmail.com>

(in-package #:breakds.bioinfo-kit)

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

(defun profile-from-list (profile-list)
  (let* ((size (length (car profile-list))))
    (make-array (list 4 size)
                :initial-contents profile-list)))

(defun profile-size (profile)
  (second (array-dimensions profile)))

(defun profile-fit-score (pattern profile)
  (let ((score 1.0))
    (loop 
       for nucl across pattern
       for i from 0
       do (setf score (* (aref profile (nucleobase-id nucl) i)
                         score)))
    score))

(defun profile-match (dna profile)
  (let ((size (profile-size profile))
        best)
    (loop 
       for i below (- (length dna) size)
       for score = (profile-fit-score 
                    (subseq dna i (+ i size))
                    profile)
       when (or (null best) (> score (car best)))
       do (setf best (list score i)))
    (subseq dna (cadr best) 
            (+ (cadr best) size))))

(defun motifs-profile (motifs)
  (let ((profile (make-array (list 4 (length (car motifs)))))
        (unit (/ 1 (length motifs))))
    (loop for motif in motifs
       do (loop for i below (length (car motifs))
             do (incf (aref profile 
                            (nucleobase-id (char motif i))
                            i)
                      unit)))
    profile))

(defun profile-score (profile)
  "Sum of number of minorities. The lower the best."
  (loop for i below (profile-size profile)
     sum (- 1.0 (max (aref profile 0 i)
                     (aref profile 1 i)
                     (aref profile 2 i)
                     (aref profile 3 i)))))

(defun greedy-motif-search (dna-list pattern-size)
  (let ((best-score pattern-size)
        (best-motifs nil))
    (loop 
       for i below (- (length (car dna-list)) pattern-size)
       for candidate = (subseq (car dna-list) i (+ i pattern-size))
       do (let ((profile (motifs-profile (list candidate)))
                (motifs (list candidate)))
            (loop 
               for dna in (rest dna-list)
               for current-best = (profile-match dna profile)
               do (setf motifs (cons current-best motifs)
                        profile (motifs-profile motifs)))
            (let ((new-score (profile-score profile)))
              (if (< new-score best-score)
                  (setf best-score new-score
                        best-motifs motifs)))))
    (nreverse best-motifs)))

(defun motifs-profile-laplace (motifs)
  (let* ((unit (/ 1 (+ (length motifs) 4)))
         (profile (make-array (list 4 (length (car motifs)))
                              :initial-element unit)))
    (loop for motif in motifs
       do (loop for i below (length (car motifs))
             do (incf (aref profile 
                            (nucleobase-id (char motif i))
                            i)
                      unit)))
    profile))

(defun greedy-motif-search-1 (dna-list pattern-size)
  (let ((best-score pattern-size)
        (best-motifs nil))
    (loop 
       for i below (- (length (car dna-list)) pattern-size)
       for candidate = (subseq (car dna-list) i (+ i pattern-size))
       do (let ((profile (motifs-profile (list candidate)))
                (motifs (list candidate)))
            (loop 
               for dna in (rest dna-list)
               for current-best = (profile-match dna profile)
               do (setf motifs (cons current-best motifs)
                        profile (motifs-profile-laplace motifs)))
            (let ((new-score (profile-score (motifs-profile motifs))))
              (if (< new-score best-score)
                  (setf best-score new-score
                        best-motifs motifs)))))
    (nreverse best-motifs)))



  
                        
  
  
         
  
  
                                  

        
        
  
