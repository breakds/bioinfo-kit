;;;; assmebly.lisp
;;;; Description: DNA Assembly and its friends.

(in-package #:breakds.bioinfo-kit)

(defun dna-composition (dna subseq-length)
  (sort (loop for i from 0 to (- (length dna) subseq-length)
           collect (subseq dna i (+ i subseq-length))) 
        #'string-lessp))

(defun path-assembly (dna-path)
  (concatenate 'string
               (first dna-path)
               (coerce (loop for piece in (rest dna-path)
                          collect (char piece (1- (length piece))))
                       'string)))

(defun overlap (patterns)
  "Generates an overlap graph."
  (loop for a in patterns
     append (loop for b in patterns
               when (and (not (equal a b))
                         (equal (subseq a 1)
                                (subseq b 0 (1- (length a)))))
               collect (list a b))))

(defun print-overlaps (overlap-graph)
  (loop for edge in overlap-graph
     do (format t "~a -> ~a~%" (car edge) (cadr edge))))

(defun wrapped-overlap (input-filename)
  (with-open-file (in input-filename
                      :direction :input)
    (with-open-file (*standard-output* 
                     (format nil "~a.ans" input-filename)
                     :direction :output)
      (print-overlaps (overlap (loop for x = (read-line in nil nil)
                                  while x
                                  collect x))))))

(defun prefix-dict (patterns)
  "Create prefix->postfix dictionaries, which can be served as edges
of the debruijn graph."
  (let ((dict (make-hash-table :test #'equal)))
    (loop for pattern in patterns
       do (push (subseq pattern 1)
                (gethash (subseq pattern 0 (1- (length pattern))) 
                         dict nil)))
    dict))

(defun print-prefix-dict (graph &optional (out t))
  (loop
     for node being the hash-keys of graph
     for ends being the hash-values of graph
     do (format out "~a -> ~{~a~^,~}~%" node ends)))

(def-wrapped debruijn
  (let* ((pattern-size (read in))
         (text (read-line in))
         (graph (prefix-dict (loop for i to (- (length text)
                                               pattern-size)
                                collect (subseq text i (+ pattern-size
                                                          i))))))
    (print-prefix-dict graph out)))


(def-wrapped debruijn-1
  (let* ((patterns (loop for pattern = (read-line in nil nil) 
                      while pattern
                      collect pattern))
         (graph (prefix-dict patterns)))
    (print-prefix-dict graph out)))

    

    

    
                                    

      

                      
    
  


  

