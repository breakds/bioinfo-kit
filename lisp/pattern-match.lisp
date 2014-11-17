;;;; pattern-match.lisp
;;;; Author: Break Yang <breakds@gmail.com>

(in-package #:breakds.bioinfo-kit)

(def-string-fun reverse-complement (text)
  (labels ((iter (k accu)
             (if (>= k (length text)) 
                 (coerce accu 'string)
                 (iter (1+ k) (cons (case (char text k) 
                                      (#\A #\T)
                                      (#\T #\A)
                                      (#\C #\G)
                                      (#\G #\C))
                                    accu)))))
    (iter 0 nil)))



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

(def-string-fun kmp-all-matches (text pattern)
  "KMP pattern match algorithm for all occurrens."
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

(def-string-fun frequent-word (text len)
  "Find the most frequent word(s) with length LEN in TEXT."
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

(def-string-fun find-clumps (text pattern-len window occurrence)
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

(def-string-fun hamming-distance (text-a text-b)
  (if (= (length text-a) (length text-b))
      (loop 
	 for a across text-a
	 for b across text-b
	 when (not (char= a b))
	 summing 1)
      (error "Two input strings are not of equal length.")))

(def-string-fun approximate-pattern-match (text pattern tolerance)
  (loop for i from 0 to (- (length text) (length pattern))
     when (<= (hamming-distance pattern (subseq text i (+ i (length pattern))))
              tolerance)
     collect i))

(defun gen-neighbors (pattern distance)
  (labels ((flipped-word (word pos new-digit)
             (let ((result (copy-seq word)))
               (setf (char result pos) new-digit)
               result))
           (flip-digits (start flips current)
             (let ((remainder (- (length pattern) start)))
               (cond ((or (= flips 0) (= remainder 0)) nil)
                     (t (append (mapcan (lambda (new-digit)
                                          (let ((new-word (flipped-word current
                                                                        start 
                                                                        new-digit)))
                                            (cons new-word
                                                  (flip-digits (1+ start)
                                                               (1- flips)
                                                               new-word))))
                                        (remove-if #`,(char= (char current start) x1)
                                                   '(#\A #\C #\G #\T)))
                                (flip-digits (1+ start) flips current)))))))
    (cons pattern (flip-digits 0 distance pattern))))

(def-string-fun frequent-with-mismatch (text pattern-length tolerance
                                             enable-rc)
  (let ((records (make-hash-table :test #'equal)))
    (loop 
       for i below (- (length text) pattern-length)
       for word = (subseq text i (+ i pattern-length))
       do (aif (gethash word records)
               (setf (gethash word records)
                     (list (1+ (car it))
                           (cadr it)))
               (setf (gethash word records)
                     (list 1 (union (if enable-rc 
                                        (gen-neighbors (reverse-complement word)
                                                       tolerance)
                                        nil)
                                    (gen-neighbors word tolerance))))))
    (let ((counter (make-hash-table :test #'equal))
          (max-occurs 0)
          max-list)
      (loop for record being the hash-values of records
         do (loop for word in (cadr record)
               do (incf (gethash word counter 0) (car record))))
      (loop 
         for candidate being the hash-keys of counter
         for occurs being the hash-values of counter
         do (cond ((> occurs max-occurs) 
                   (setf max-occurs occurs
                         max-list (list candidate)))
                  ((= occurs max-occurs)
                   (push candidate max-list))))
      (values max-list max-occurs))))







