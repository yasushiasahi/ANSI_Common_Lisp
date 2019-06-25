1
(+ 2 3)
(+ 2 3 4)
(+ 2 3 4 5)
(/ (- 7 1) (- 4 2))

(quote (+ 3 5))
'(+ 3 5)

"ora et labora"
'Artichoke

'(my 3 "Sons")
'(the list (a b c) has 3 elements)

(list 'my (+ 2 1) "Sons")

(list '(+ 2 1) (+ 2 1))

()
nil

(eq () nil)

(cons 'a '(b c d))

(cons 'a nil)
(cons 'a (cons 'b ()))
(list 'a 'b)

(car '(a b c))
(cdr '(a b c))

(car (cdr (cdr '(a b c d))))
(third '(a b c d))

t

(listp '(a b c))
(listp 23)

(null nil)
(null ())
(null '(a))
(not nil)
(not ())

(if (listp '(a b c))
    (+ 1 2)
    (+ 5 6))

(if (listp 23)
    (+ 1 2)
    (+ 5 6))

(if (listp 23)
    (+ 1 2))

(if 27 1 2)

(and t (+ 1 2))
(or t (+ 1 2))

(defun our-third (x)
  (car (cdr (cdr x))))

(our-third '(a b c d))

(defun sum-greater (x y z)
  (> (+ x y) z))

(sum-greater 1 4 3)
(sum-greater 1 2 3)

(defun our-member (obj lst)
  (if (null lst)
      nil
      (if (eql (car lst) obj)
	  lst
	  (our-member obj (cdr lst)))))

(our-member 'b '(a b c))
(our-member 'z '(a b c))

(format t "~A plus ~A equals ~A.~%" 2 3 (+ 2 3))

(defun askem (string)
  (format t "~A" string)
  (read))

					;(askem "How old are you? ")

(let ((x 1) (y 2))
  (+ x y))

(defun ask-number ()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
	val
	(ask-number))))

(defparameter *glob* 99)

(defconstant limit (+ *glob* 1))

(boundp '*glob*)

(setf *glob* 98)

(let ((n 10))
  (setf n 2)
  n)

(setf x (list 'a 'b 'c))

(setf (car x) 'n)

(setf a 'b
      c 'd
      e 'f)

(setf lst '(c a r a t))
(remove 'a lst)
(setf lst (remove 'a lst))m

(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

(defun show-squares2 (start end)
  (if (> i end)
      'done
      (progn
	(format t "~A ~A~%" i (* i i))
	(show-squares2 (1+ start) end))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (1+ len)))
    len))

(defun our-length2 (lst)
  (if (null lst)
      0
      (+ (our-length2 (cdr lst)) 1)))

(function +)
#'+

(apply #'+ '(1 2 3))
(+ 1 2 3)
(apply #'+ 1 2 '(3 4 5))
(funcall #'+ 1 2 3)

(lambda (x y)
  (+ x y))

((lambda (x) (+ x 100)) 5)

(funcall #'(lambda (x) (+ x 100))
	 5)

(funcall (lambda (x) (+ x 100))
	 5)
(typep 27 'integer)
