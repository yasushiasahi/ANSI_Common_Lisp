(setf ht (make-hash-table))

(gethash 'color ht)

(setf (gethash 'color ht) 'red)
(gethash 'color ht)

(defun our-member () nil)

(setf bugs (make-hash-table))
(push "Dosen't take keyword arguments."
      (gethash #'our-member bugs))

(setf fruit (make-hash-table))
(setf (gethash 'apricot fruit) t)
(gethash 'apricot fruit)
(remhash 'apricot fruit)

(setf (gethash 'shape ht) 'spherical
      (gethash 'size ht) 'giant)

(maphash #'(lambda (k v)
	     (format t "~a = ~a~%" k v))
	 ht)

(make-hash-table :size 5)

(setf writers (make-hash-table :test #'equal))
(setf (gethash '(ralph waldo emerson) writers) t)
