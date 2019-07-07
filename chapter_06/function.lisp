(fboundp '+)
(symbol-function '+)

(setf (symbol-function 'add2)
      (lambda (x) (+ x 2)))

(add2 5)

(defun add2 (x)
  (+ x 2))

(defun primo (lst) (car lst))

(defun (setf primo) (val lst)
  (setf (car lst) val))

(defun foo (x)
  "Implements an enhanced paradigm of diversity."
  x)

(documentation 'foo 'function)

(labels ((add10 (x) (+ x 10))
	 (consa (x) (cons 'a x)))
  (consa (add10 3)))

(labels ((len (lst)
	   (if (null lst)
	       0
	       (+ (len (cdr lst)) 1))))
  (len '(a b c)))

(defun our-funcall (fn &rest args)
  (apply fn args))

(defun philosoph (thing &optional property)
  (list thing 'is property))

(philosoph 'death)
(philosoph 'death 'fear)

(defun philosoph2 (thing &optional (property 'fun))
  (list thing 'is property))

(philosoph2 'death)
(philosoph2 'death 'fear)

(defun philosoph3 (thing &optional (property (+ 1 2)))
  (list thing 'is property))

(philosoph3 'death)

(defun keylist (a &key x y z)
  (list a x y z))

(keylist 1)
(keylist 1 :y 2)
(keylist 1 :y 3 :x 2)

(defun our-adjoin (obj lst &rest args)
  (if (apply #'member obj lst args)
      lst
      (cons obj lst)))

(destructuring-bind ((&key w x) &rest y) '((:w 3) a)
  (list w x y))

(defun single? (lst)
  (and (consp lst) (null (cdr lst))))

(single? '(a))
(single? '(a b))

(defun append1 (lst obj)
  (append lst (list obj)))

(append1 '(a b c) 'd)

(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

(map-int #'identity 10)
(map-int #'(lambda (x) (random 100)) 10)

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

(filter #'(lambda (x)
	    (and (evenp x) (+ x 10)))
	'(1 2 3 4 5 6 7))

(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
	     (max (funcall fn wins)))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (> score max)
	      (setf wins obj
		    max score))))
	(values wins max))))

(defun combiner (x)
  (ctypecase x
    (number #'+)
    (list #'append)
    (t #'list)))

(defun combine (&rest args)
  (apply (combiner (car args))
	 args))

(combine 1 2 3 4)
(combine '(a b c) '(e f g))

(setf fn (let ((i 3))
	   (lambda (x) (+ x i))))

(funcall fn 2)

(defun add-to-list (num lst)
  (mapcar #'(lambda (x)
	      (+ x num))
	  lst))

(defun make-adder (n)
  (lambda (x)
    (+ x n)))

(setf add3 (make-adder 3))
(funcall add3 2)

(setf add27 (make-adder 27))
(funcall add27 2)

(let ((counter 0))
  (defun reset ()
    (setf counter 0))
  (defun stamp ()
    (setf counter (1+ counter))))

(list (stamp) (stamp) (reset) (stamp))

(mapcar (complement #'oddp)
	'(1 2 3 4 5 6))

(defun our-complement (f)
  (lambda (&rest args)
    (not (apply f args))))

(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    (lambda (&rest args)
      (reduce (lambda (v f) (funcall f v))
	      rest
	      :initial-value (apply fn1 args)))))

(defun disjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
	(lambda (&rest args)
	  (or (apply fn args) (apply disj args))))))

(defun conjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((conj (apply #'conjoin fns)))
	(lambda (&rest args)
	  (and (apply fn args) (apply conj args))))))

(defun curry (fn &rest args)
  (lambda (&rest args2)
    (apply fn (append args args2))))

(defun recurry (fn &rest args)
  (lambda (&rest args2)
    (apply fn (append args2 args))))

(defun always (x) (lambda (&rest args) x))

(let ((x 10))
  (defun foo () x))

(let ((x 20)) (foo))

(let ((x 10))
  (defun foo ()
    (declare (special x))
    x))

(let ((x 20))
  (declare (special x))
  (foo))

(setf x 30)

(foo)

(princ 32)
(let ((*print-base* 16))
  (princ 32))

(defun foo (x) (+ x 1))
(compiled-function-p #'foo)

(compile 'foo)
