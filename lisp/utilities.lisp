;;;; utilities.lisp
;;;; Author: Break Yang <breakds@gmail.com>

(in-package #:breakds.bioinfo-kit)

(defmacro def-string-fun (name args &body body)
  (with-gensyms (arg input)
    `(progn (defun ,(symb name '-impl) ,args ,@body)
	    (defmacro ,name ,(mapcar #`,(symb x1 '-1) args)
	      (with-gensyms ,args
		(reduce (lambda (wrapped arg-input-pair)
			  (destructuring-bind (,arg ,input) arg-input-pair
			    (if (consp ,input)
				(case (car ,input)
				  (:file `(let ((,,arg (with-open-file (in ,(second ,input)
									   :direction :input)
							 (read-line))))
					    ,wrapped))
				  (:file-stream `(with-open-file (,,arg ,(second ,input)
									:direction :input)
						   ,wrapped))
				  (t `(let ((,,arg ,,input))
					,wrapped)))
				`(let ((,,arg ,,input))
				   ,wrapped))))
			(mapcar (lambda (x y) (list x y))
				(list ,@args)
				(list ,@(mapcar #`,(symb x1 '-1) args)))
			:initial-value (list ',(symb name '-impl) ,@args)))))))


(defmacro union-string-lists (&rest string-list)
  (reduce (lambda (result current-list)
	    (if (null result)
		current-list
		`(union ,current-list ,result
			:test #'equal)))
	  string-list
	  :initial-value nil))
	    

(defun sub-circ-seq (original start len)
  (let ((end (+ start len)))
    (if (<= end (length original))
        (subseq original start end)
        (concatenate 'string 
                     (subseq original start 
                             (length original))
                     (subseq original 0 (- end (length original)))))))
      
