;;;; utilities.lisp
;;;; Author: Break Yang <breakds@gmail.com>

(in-package #:breakds.bioinfo-kit)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun load-string (string-spec)
    (if (consp string-spec)
	(case (car string-spec)
	  (:file `(with-open-file (in ,string-spec
				      :direction :input)
		    (read-line in)))
	  (t (error "Unknown string specifier.")))
	string-spec)))

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defun load-stream (stream-spec)
;;     (if (consp stream-spec)
;; 	(case (car string-spec)
;; 	  (:file `(with-open-file (in ,string-spec
;; 				      :direction :input)
;; 		    (read-line in)))
;; 	  (t (error "Unknown string specifier.")))
;; 	string-spec)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun transform-string-args (transform-string transform-stream args)
    (mapcar (lambda (arg)
	      (if (consp arg)
		  (case (car arg)
		    (:string (funcall transform-string
				      (second arg)))
		    (:stream (funcall transform-stream
				      (second arg))))
		  arg))
	    args)))
	
(defmacro def-string-fun (name args &body body)
  (let ((fun-name name))
    `(progn (defun ,(symb fun-name '-impl) 
		,(transform-string-args #`,x1 #`,x1 args)
	      ,@body)
	    (defmacro ,fun-name 
		,(transform-string-args #`,(symb x1 '-str) #`,x1 args)
	      `(,',(symb fun-name '-impl) 
		   ,',(transform-string-args #`,(load-string (symb x1 '-str))
					     #`,x1
					     args))))))
