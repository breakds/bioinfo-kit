;;;; eulerian.lisp
;;;; Description: Eulerian cycle and path.

(in-package #:breakds.bioinfo-kit)

(declaim (inline copy-edge-dict))
(defun copy-edge-dict (input-dict)
  (let ((result (make-array (length input-dict) :initial-element nil)))
    (loop for i below (length input-dict)
       do (setf (aref result i) (aref input-dict i)))
    result))

(defun path-generator (path-set node-num)
  (let ((stack (list (car path-set)))
        (ready (make-array node-num :initial-element nil)))
    (loop for path in (cdr path-set)
       do (push path (aref ready (car path))))
    (lambda ()
      (loop until (or (null stack) 
                      (not (null (car stack))))
         do (pop stack))
      (when (not (null stack))
        (when (aref ready (caar stack))
          (let ((trigger (pop (car stack))))
            (push (pop (aref ready trigger)) stack)))
        (pop (car stack))))))

(defun unbalanced-nodes (edge-dict)
  "Returns unbalanced nodes from a graph described by EDGE-DICT. The
  return value will be a pair of nodes (A B) where A has smaller
  in-degree and B has smaller out-degree."
  (let ((in-degree (make-array (length edge-dict) 
                               :element-type 'fixnum
                               :initial-element 0))
        a b)
    (loop for ends across edge-dict
       do (loop for end in ends
             do (incf (aref in-degree end))))
    (loop for i below (length edge-dict)
       do (case (- (length (aref edge-dict i))
                   (aref in-degree i))
            (0 nil)
            (1 (setf a i))
            (-1 (setf b i))
            (t (error "Invalid unbalanced node"))))
    (list a b)))

(defun eulerian-cycle (input-edge-dict &optional (start nil))
  (let ((edge-dict (copy-edge-dict input-edge-dict))
        (visit-dict (make-array (length input-edge-dict)
                                :element-type '(unsigned-byte 8)
                                :initial-element 0))
        (candidates nil))
    (labels ((next-node (current accu)
               (if (null (aref edge-dict current))
                   (nreverse (cons current accu))
                   (let ((new-node (pop (aref edge-dict current))))
                     (when (and (zerop (aref visit-dict current))
                                (not (null (aref visit-dict current))))
                       (incf (aref visit-dict current))
                       (push current candidates))
                     (next-node new-node (cons current accu)))))
             (next-cycle (start accu)
               (let ((new-cycle (next-node start nil)))
                 (loop for candidate = (car candidates) 
                    while candidate
                    do (if (aref edge-dict candidate)
                           (return)
                           (pop candidates)))
                 (if (null candidates)
                     (nreverse (cons new-cycle accu))
                     (next-cycle (car candidates)
                                 (cons new-cycle accu))))))
      (let ((cycles (next-cycle (if start start 0) nil)))
        (path-generator cycles
                        (length input-edge-dict))))))


;;; ---------- Graph Representation Utilities ----------

(defun edge-dict-from-list (edge-list)
  (let ((inverse-dict (make-hash-table :test #'equal))
        (id -1))
    (loop for edges in edge-list
       do (loop for node in edges
             when (null (gethash node inverse-dict nil))
             do (setf (gethash node inverse-dict) (incf id))))
    (let ((dict (make-array (1+ id) :initial-element nil))
          (result (make-array (1+ id) :initial-element nil)))
      (loop 
         for original-name being the hash-keys of inverse-dict
         for new-id being the hash-values of inverse-dict
         do (setf (aref dict new-id) original-name))
      (loop 
         for edges in edge-list
         for node = (gethash (car edges) inverse-dict)
         do (loop 
               for end-name in (cdr edges)
               for end = (gethash end-name inverse-dict)
               do (push end (aref result node))))
      (values result (lambda (x) (aref dict x))))))

(defun eulerian-cycle-from-list (edge-list)
  (multiple-value-bind (edge-dict translator)
      (edge-dict-from-list edge-list)
    (destructuring-bind (start end) (unbalanced-nodes edge-dict)
      (when start
        (push start (aref edge-dict end)))
      (let* ((cycle (eulerian-cycle edge-dict (if start start 0)))
             (converted (loop for node = (funcall cycle) while node
                           collect (funcall translator node))))
        (if end
            (subseq converted 0 (1- (length converted)))
            converted)))))

(defun eulerian-assembly (patterns)
  (let* ((graph (prefix-dict patterns))
         (edge-list (loop 
                       for start being the hash-keys of graph
                       for ends being the hash-values of graph
                       collect (cons start ends))))
    (multiple-value-bind (edge-dict translator)
        (edge-dict-from-list edge-list)
      (destructuring-bind (start end) (unbalanced-nodes edge-dict)
        (when start
          (push start (aref edge-dict end)))
        (let* ((cycle (eulerian-cycle edge-dict (if start start 0)))
               (result-list (nreverse (coerce (funcall translator 
                                                       (funcall cycle))
                                              'list))))
          (loop 
             for node = (funcall cycle)
             while node
             for pattern = (funcall translator node)
             for x = (char pattern (1- (length pattern)))
             do (push x result-list))
          (coerce (nreverse (if end (rest result-list) result-list)) 'string))))))

(defun universal-string (k)
  (let ((mask (1- (ash 1 (1- k))))
        (edge-dict (make-array (ash 1 k) :initial-element nil)))
    ;; populate binary edge dictionary
    (loop for i below (ash 1 k)
       do (let ((left (ash (logand i mask) 1)))
            (push left (aref edge-dict i))
            (push (1+ left) (aref edge-dict i))))
    (let ((format-string (format nil "~~~a,'0b: ~~{~~~a,'0b~~^,~~}~~%" k k)))
      (loop for i below (ash 1 k)
         do (format t format-string i (aref edge-dict i))))
    (let ((cycle (eulerian-cycle edge-dict))
          (result-list nil))
      (loop 
         for node = (funcall cycle)
         while node
         do (push (logand node 1) result-list))
      (format nil "~{~a~}" (nreverse (rest result-list))))))

      
      

(defun parse-edge (edge-descriptor)
  (let ((arrow-pos (car (kmp-all-matches edge-descriptor " -> "))))
    (cons (subseq edge-descriptor 0 arrow-pos)
          (split-sequence:split-sequence #\, (subseq edge-descriptor 
                                                     (+ arrow-pos 4))))))

;;; ---------- Wrappers ----------

(def-wrapped eulerian-cycle
  "handles both eulerian-cycle and eulerian-path."
  (let* ((edge-list (loop for line = (read-line in nil nil) 
                       while line
                       collect (parse-edge line)))
         (cycle (eulerian-cycle-from-list edge-list)))
    (format t "total edges: ~a~%cycle-length: ~a~%"
            (loop for edge in edge-list sum (1- (length edge)))
            (1- (length cycle)))
    (format out "~{~a~^->~}~%" cycle)))

(def-wrapped eulerian-assembly
  ;; Ignore the first line, which is the pattern length, as we can
  ;; deduce it from the patterns anyway.
  (read-line in)
  (let* ((patterns (loop for line = (read-line in nil nil)
                      while line collect line))
         (assembly (eulerian-assembly patterns)))
    (format out "~a~%" assembly)))






            
    
    

    
    
    
               
               
             
      

                     
    


