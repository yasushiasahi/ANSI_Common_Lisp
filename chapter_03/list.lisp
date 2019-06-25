(setf x (cons 'a nil))
(car x)
(cdr x)

(setf y (list 'a 'b 'c))
(cdr y)

(setf z (list 'a (list 'b 'c) 'd))
(cdr z)
(car (cdr z))

(defun our-listp (x)
  (or (null x) (consp x)))

(defun our-atom (x)
  (not (consp x)))

(eql (cons 'a nil) (cons 'a nil))

(setf x (cons 'a nil))
(eql x x)
(eql x (cons 'a nil))
(equal x (cons 'a nil))

(defun our-equal (x y)
  (format t "~%x:~a, y:~a" x y)
  (or (eql x y)
      (and (consp x)
	   (consp y)
	   (our-equal (car x) (car y))
	   (our-equal (cdr x) (cdr y)))))

(setf x '(a b c))
(setf y x)
(eql x y)

(setf x '(a b c)
      y (copy-list x))
(eql x y)

(defun our-copy-list (lst)
  (format t "~%lst = ~a" lst)
  (if (atom lst)
      lst
      (cons (car lst) (our-copy-list (cdr lst)))))

(append '(a b) '(c d) '(e))
(append  nil nil nil nil)

(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))

(defun compr (elt n lst)
  (format t "~%elt:~a n:~a lst:~a" elt n lst)
  (if (null lst)
      (list (n-elts elt n))
      (let ((next (car lst)))
	(if (eql next elt)
	    (compr elt (+ n 1) (cdr lst))
	    (cons (n-elts elt n)
		  (compr next 1 (cdr lst)))))))

(defun n-elts (elt n)
  (if (> n 1)
      (list n elt)
      elt))

(setf l '(1 1 1 0 1 0 0 0 0 1))

(defun uncompress (lst)
  (if (null lst)
      nil
      (let ((elt (car lst))
	    (rest (uncompress (cdr lst))))
	(if (consp elt)
	    (append (apply #'list-of elt)
		    rest)
	    (cons elt rest)))))

(defun list-of (n elt)
  (if (zerop n)
      nil
      (cons elt (list-of (- n 1) elt))))

(nth 0 '(a b c))
(nth 2 '(a b c))

(nthcdr 2 '(a b c d))

(defun our-nthcdr (n lst)
  (format t "~%~a ~a" n lst)
  (if (zerop n)
      lst
      (our-nthcdr (1- n) (cdr lst))))

(last '(a b c))

(mapcar #'(lambda (x)
	    (+ x 10))
	'(1 2 3))

(mapcar #'list
	'(a b c)
	'(1 2 3 4)
	'(foo bar baz))

(mapcar #'(lambda (x y)
	    (+ x y))
	'(1 2 3 4)
	'(10 20 30))

(mapcar #'(lambda (x) x)
	 '(a b c))

(maplist #'(lambda (x)
	     (format t "~%~a" x)
	     x)
	 '(a b c))

(setf tree '(a (b c) d))
(copy-tree tree)

(defun our-copy-tree (tr)
  (format t "~%lst = ~a" tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
	    (our-copy-tree (cdr tr)))))

(defun our-copy-list (lst)
  (format t "~%lst = ~a" lst)
  (if (atom lst)
      lst
      (cons (car lst)
	    (our-copy-list (cdr lst)))))

(substitute 'y 'x '(and (integerp x) (zerop (mod x 2))))
(subst 'y 'x '(and (integerp x) (zerop (mod x 2))))

(setf tr '(and (integerp x) (zerop (mod x 2))))

(defun our-subst (y x lst)
  (format t "~%lst = ~a" lst)
  (if (equal x lst)
      y
      (let (head (if (consp (car lst))
		     (our-subst y x (car lst))
		     (car lst)))
	(cons head (our-subst y x (cdr lst))))))

(defun our-subst (new old tree)
  (format t "~%lst = ~a" tree)
  (if (eql old tree)
      new
      (if (atom tree)
	  tree
	  (cons (our-sbst new old (car tree))
		(our-sbst new old (cdr tree))))))

(defun our-len (lst)
  (if (null lst)
      0
      (1+ (our-len (cdr lst)))))

(defun our-member (obj lst)
  (if (eql (car lst) obj)
      lst
      (our-member obj (cdr lst))))

(member 'b '(a b c))

(member '(a) '((a) (z)))
(member '(a) '((a) (z)) :test #'equal)
(member 'a '((a b) (c d)))
(member 'a '((a b) (c d)) :key #'car)
(member 'a '((a b) (c d)) :test (lambda (a b)
				  (format t "~%a:~a b:~a" a b)))
(member 'a '((a b) (c d)) :test (lambda (a b)
				  (eql a (car b))))
(member 2 '((1) (2)) :test #'equal :key #'car)

(member-if #'oddp '(2 3 4))

(defun our-member-if (func lst)
  (if (null lst)
      nil
      (if (funcall func (car lst))
	  lst
	  (our-member-if func (cdr lst)))))

(adjoin 'b '(a b c))
(adjoin 'z '(a b c))

(union '(a b c) '(c b s))
(intersection '(a b c) '(b b c))
(set-difference '(a b c d e) '(b e))
