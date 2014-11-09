;;;; oric.lisp
;;;; Author: Break Yang <breakds@gmail.com>

(in-package #:breakds.bioinfo-kit)

(def-string-fun skew-array (text)
  (labels ((assemble (k accu)
             (if (< k (length text))
                 (assemble (1+ k) (cons (case (char text k)
                                          (#\C (1- (car accu)))
                                          (#\G (1+ (car accu)))
                                          (t (car accu)))
                                        accu))
                 (nreverse accu))))
    (assemble 0 '(0))))

(def-string-fun minimize-skew (text)
  (labels ((assemble (k last-skew min min-stack)
             (if (< k (length text))
                 (let ((new-skew (+ last-skew
                                    (case (char text k)
                                      (#\C -1)
                                      (#\G 1)
                                      (t 0)))))
                   (cond ((< new-skew min) (assemble (1+ k) new-skew new-skew
                                                     (list (1+ k))))
                         ((= new-skew min) (assemble (1+ k) new-skew min
                                                     (cons (1+ k) min-stack)))
                         (t (assemble (1+ k) new-skew min
                                      min-stack))))
                 min-stack)))
    (assemble 0 0 0 '(0))))

                        


