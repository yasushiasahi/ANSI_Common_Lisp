(+ (- 5 1) (+ 3 7))

(list 1 (+ 2 3))

(if (listp 1)
    (+ 1 2)
    (+ 3 4))

(list (and (listp 3) t) (+ 1 2))

(cons 'a (cons 'b (cons 'c nil)))
(cons 'a (list 'b 'c))
(cons 'a '(b c))

(defun get4th (lst)
  (car (cdr (cdr (cdr lst)))))

(defun bigger (a b)
  (if (< a b)
      b
      a))

;; 引数のリスト内にnilがあればTをかえす。
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
	   (enigma (cdr x)))))

;; リストyの中でxが何番目にあるかをかえす。なかった場合はnil
(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
	  0
	  (let ((z (mystery x (cdr y))))
	    (and z (+ z 1))))))

(car (car (cdr '(a (b c) d))))

(or 13 (/ 1 0))

(apply #'list 1 nil)

(defun is-contains-list (lst)
    (if (null lst)
	nil
	(or (listp (car lst))
	    (is-contains-list (cdr lst)))))

(defun dot-dot (num)
  (if (> 1 num)
      nil
      (progn
	(format t ".")
	(dot-dot (1- num)))))

(defun count-a (lst)
  (if lst
      (if (equal 'a (car lst))
	  (+ 1 (count-a (cdr lst)))
	  (count-a (cdr lst)))
      0))

(defun summit (lst)
  (setf lst (remove nil lst))
  (apply #'+ lst))

(defun summit2 (lst)
  (if lst
      (let ((x (car lst)))
	(if (null x)
	    (summit2 (cdr lst))
	    (+ x (summit2 (cdr lst)))))
      0))
