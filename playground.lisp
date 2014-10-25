(defun get-prefix-func (pattern)
  "Derives the prefix function for the provided pattern. This is the
  core to KMP algorithm."
  ;; The prefix function is defined as following:
  ;; pre(i) = the longest length of prefix match given there is
  ;; already a match from index 0 to index i-1,
  (let ((prefix (make-array (1+ (length pattern))
                            :element-type 'fixnum
                            :initial-element 0)))
    (labels ((first-time-solve (index target-char)
               (let ((guess (aref prefix index)))
               (if (char= (char pattern guess)
                          target-char)
                   (1+ guess)
                   (if (zerop guess)
                       0
                       (first-time-solve guess
                                         target-char))))))
      (loop for i from 2 to (length pattern)
         do (setf (aref prefix i)
                  (first-time-solve (1- i) 
                                    (char pattern (1- i))))))
    (lambda (matched) (aref prefix matched))))

(defun kmp-all-matches (text pattern)
  (let ((prefix-func (get-prefix-func pattern))
        (pattern-len (length pattern))
        (text-len (length text)))
    (labels ((try-match (begin matched accu)
               (let* ((pos (+ begin matched))
                      (new-accu (if (= matched pattern-len)
                                    (cons begin accu)
                                    accu)))
                 (cond ((>= pos text-len) new-accu)
                       ((or (= matched pattern-len) 
                            (not (char= (char text pos)
                                        (char pattern matched))))
                        (let ((new-matched (funcall prefix-func matched)))
                          (try-match (+ begin (max 1 (- matched new-matched)))
                                     new-matched
                                     new-accu)))
                       (t (try-match begin (1+ matched) accu))))))
      (try-match 0 0 nil))))

(defun frequent-word (text len)
  (let ((count-table (make-hash-table :test #'equal))
        (result nil)
        (current-max 0))
    (loop 
       for i from 0 to (- (length text) len)
       for j = (+ i len)
       do (incf (gethash (subseq text i j) count-table 0)))
    (loop 
       for key being the hash-keys of count-table
       for count being the hash-values of count-table
       do (cond ((> count current-max) 
                 (setf current-max count
                       result (list key)))
                ((= count current-max) (push key result))))
    result))

(defun reverse-complement (text)
  (coerce (nreverse (loop for c across text
                       collect (case c
                                 (#\A #\T)
                                 (#\T #\A)
                                 (#\C #\G)
                                 (#\G #\C))))
          'string))
                
(defun find-clumps (text pattern-len window occurrence)
  (let ((count-table (make-hash-table :test #'equal))
        (result-table (make-hash-table :test #'equal)))
    (labels ((word-at (pos)
               (subseq text pos (+ pos pattern-len)))
             (update-in (pos)
               (when (>= (incf (gethash (word-at pos)
                                        count-table 0))
                         occurrence)
                 (update-result pos)))
             (update-out (pos)
               (decf (gethash (word-at pos)
                              count-table)))
             (update-result (pos)
               (setf (gethash (word-at pos)
                              result-table) t)))
      (loop for front below (length text)
         when (>= front (1- pattern-len))
         do (update-in (1+ (- front pattern-len)))
         when (>= front window)
         do (update-out (- front window))))
    (loop for key being the hash-keys of result-table
       collect key)))

(defun find-clumps-from-file (filename pattern-len window occurrence)
  (with-open-file (*standard-input* filename
                                    :direction :input)
    (find-clumps (read-line) pattern-len window occurrence)))
  

                
                  

      
    
         
                                       
                        

                     
                        
           

