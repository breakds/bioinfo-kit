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

