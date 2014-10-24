(defun get-prefix-func (pattern)
  (let ((prefix (make-array (length pattern) 
                         :element-type 'fixnum
                         :initial-element 0)))
    (labels ((first-time-solve (index target-char)
               (let ((guess (aref prefix index)))
               (if (char= (char pattern guess)
                          target-char)
                   (1+ guess)
                   (if (zerop guess)
                       0
                       (first-time-solve (1+ guess) 
                                         target-char))))))
      (loop for i from 1 to (1- (length pattern))
         do (setf (aref prefix i) 
                  (first-time-solve (1- i) (char pattern i))))
      (lambda (matched-length)
        (if (zerop matched-length) 
            1
            (- matched-length (aref prefix (1- matched-length))))))))

(defun kmp-match-all (text pattern)
  (let ((jump-func (get-jump-func pattern)))
    (labels ((try-match (begin current accu)
               (format t "~a ~a~%" begin current)
               (cond ((>= current (length text)) 
                      (if (= (- current begin) (length pattern))
                          (cons begin accu)
                          accu))
                     ((or (= (- current begin) (length pattern))
                          (not (char= (char text current) 
                                      (char pattern (- current begin)))))
                      (try-match (+ begin 
                                    (funcall jump-func (- current begin)))
                                 current
                                 (cons begin accu)))
                     (t (try-match begin (1+ current) accu)))))
      (try-match 0 0 nil))))


(defun kmp-match-all (text pattern)
  (let ((jump-func (get-jump-func pattern)))
    (labels ((try-match (begin matched-length accu)
               (cond ((= matched-length (length pattern)))
                     (kmp-match-all (+ begin (funcall jump-func matched-length))
                                    

               
               (cond ((>= current (length text)) 
                      (if (= (- current begin) (length pattern))
                          (cons begin accu)
                          accu))
                     ((or (= (- current begin) (length pattern))
                          (not (char= (char text current) 
                                      (char pattern (- current begin)))))
                      (try-match (+ begin 
                                    (funcall jump-func (- current begin)))
                                 current
                                 (cons begin accu)))
                     (t (try-match begin (1+ current) accu)))))
      (try-match 0 0 nil))))

  

                 
                     
                        
           

                       
                   
                          
               
                   
    
