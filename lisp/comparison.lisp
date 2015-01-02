;;;; comparison.lisp
;;;; Description: Biological sequence comparisons

(in-package #:breakds.bioinfo-kit)

(defun change-money (target coins)
  (let ((numbers (make-hash-table)))
    (setf (gethash 0 numbers) 0)
    (labels ((calculate (target)
               (aif (gethash target numbers)
                    it
                    (setf (gethash target numbers)
                          (1+ (apply #'min (mapcar #'calculate 
                                                   (remove-if #`,(< x1 0)
                                                              (mapcar #`,(- target x1)
                                                                      coins)))))))))
      (calculate target))))

         

    
