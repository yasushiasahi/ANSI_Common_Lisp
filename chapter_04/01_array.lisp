(setf arr (make-array '(2 3) :initial-element nil))

(aref arr 0 0)
(setf (aref arr 0 0) 'b)
(aref arr 0 0)

(setf vec (make-array 4 :initial-element nil))
(vector "a" 'b 3)
