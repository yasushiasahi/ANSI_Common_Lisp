(setf y '(a b c))

(let ((x (car y)))
  (cons x x))

((lambda (y)
   (cons (car y) (car y))) y)


(setf x '(10 20 30))
(setf z 25)

(let* ((w (car x))
       (y (* w z)))
  (cons w y))

((lambda (x z)
   (setf w (car x))
   (setf y (* w z))
   (cons w y))
 x z)

(defun mystery (x y)
  (cond ((null y) nil)
	((eql (car y) x) 0)
	(t (let ((z (mystery x (cdr y))))
	     (and z (+ z 1))))))

(defun my-pow (num)
  (cond ((<= num 5) num)
	(t (sqrt num))))

(defun precedes (obj vec)
  (let (prev)
    (remove-duplicates (reduce (lambda (acc v)
				 (let ((new-acc (if (eql v obj)
						    (cons prev acc)
						    acc)))
				   (setf prev v)
				   new-acc))
			       vec
			       :initial-value nil))))

(defun intersperse (obj list)
  (cond ((null list) nil)
	((eql (length list) 1) (apply #'list (car list) (intersperse obj (cdr list))))
	(t (apply #'list (car list) obj (intersperse obj (cdr list))))))

(defun intersperse-roop (obj list)
  (let ((len (length list)))
    (do ((i 0 (1+ i))
	 (acc nil (apply #'list obj (nth (- len i 1) list) acc)))
	((= i len) (cdr acc)))))

(setf num-list '(3 7 9 7 9 9 7 7 8 6 3 3 7))
