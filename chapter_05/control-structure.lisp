(progn
  (format t "a")
  (format t "b")
  (+ 11 12))

(block head
  (format t "Here we go.")
  (return-from head 'idea)
  (format t "We'll never see this."))

(block nil
  (return 27))

(dolist (x '(a b c d e))
  (format t "~a " x)
  (if (eql x 'c)
      (return 'done)))

(defun foo ()
  (return-from foo 27))

(defun read-integer (str)
  (let ((accum 0))
    (dotimes (pos (length str))
      (let ((i (digit-char-p (char str pos))))
	(if i
	    (setf accum (+ (* accum 10) i))
	    (return-from read-integer nil))))
    accum))

(tagbody
   (setf x 0)
 top
   (setf x (1+ x))
   (format t "~a " x)
   (if (< x 10) (go top)))

(let ((x 7)
      (y 2))
  (format t "Number")
  (+ x y))

((lambda (x) (1+ x)) 1)

((lambda (x y)
   (format t "Numver")
   (+ x y))
 7
 2)

(let ((x 2)
      (y (1+ x)))
  (+ x y))

((lambda (x y) (+ x y)) 2
			(1+ x))

(let* ((x 2)
      (y (1+ x)))
  (+ x y))

(let ((x 2))
  (let ((y (1+ x)))
    (+ x y)))

(destructuring-bind (w (x y) . z) '(a (b c) d e)
  (list w x y z))

(setf that 5)

(when (oddp that)
  (format t "Hmm, that's odd.")
  (+ that 1))

(if (oddp that)
    (progn
      (format t "Hmm, that's odd.")
      (+ that 1)))

(unless (evenp that)
  (format t "Hmm, that's odd.")
  (+ that 1))

(defun out-member (obj lst)
  (if (atom lst)
      nil
      (if (eql obj (car lst))
	  lst
	  (out-member obj (cdr lst)))))

(defun out-member (obj lst)
  (cond ((atom lst) nil)
	((eql obj (car lst)) lst)
	(t (out-member obj (cdr lst)))))

(cond (99))

(defun month-length (mon)
  (case mon
    ((jan mar may jul aug oct dec) 31)
    ((apr jun sept nov) 30)
    (feb (if (leap-yer) 29 28))
    (otherwise "unknown month")))

(case 99 (99))

(defun show-squares (start end)
  (do ((i start (1+ i)))
      ((> i end) 'done)
    (format t "~a ~a~%" i (* i i))))

(let ((x 'a))
  (do ((x 1 (1+ x))
       (y x x))
      ((> x 5))
    (format t "(~a ~a)   " x y)))

(do* ((x 1 (1+ x))
      (y x x))
     ((> x 5))
  (format t "(~a ~a)  " x y))

(dolist (x '(a b c d) 'done)
  (format t "~a " x))

(dotimes (x 5 x)
  (format t "~a " x))

(mapc (lambda (x y)
	(format t "~a ~a  " x y))
      '(hip flip slip)
      '(hop flop slop))

(values 'a nil (+ 2 4))

((lambda ()
   ((lambda ()
      (values 1 2)))))

(let ((x (values 1 2)))
  x)

(values)
(let ((x (values)))
  x)

(multiple-value-bind (x y z) (values 1 2 3)
  (list x y z))

(multiple-value-bind (x y z) (values 1 2)
  (list x y z))

(multiple-value-bind (s m h) (get-decoded-time)
  (format nil "~a:~a:~a" h m s))

(multiple-value-call #'+ (values 1 2 3))

(multiple-value-list (values 'a 'b 'c))

(defun super ()
  (catch 'abort
    (format t "We'll see this.")
    (sub)
    (format t "We'll never see this.")))

(defun sub ()
  (throw 'abort 99))

(catch 'abort
  (unwind-protect
       (throw 'abort 99)
    (setf x 2)))

(setf mon '(31 28 31 30 31 30 31 31 30 31 30 31))

(multiple-value-call #'+ (apply #'values mon))
(apply #'+ mon)

(setf nom (reverse mon))

(setf sums (maplist (lambda (x)
		      (format t "~a~%" x))
		    nom))

(setf sums (maplist (lambda (x)
		      (apply #'+ x))
		    nom))

(reverse sums)

(defconstant month
  #(31 59 90 120 151 181 212 243 273 304 334 365))

(defconstant yzero 2000)

(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
	   (not (zerop (mod y 100))))))

(defun date->num (d m y)
  (+ (- d 1) (month-num m y) (year-num y)))

(defun month-num (m y)
  (+ (svref month (1- m))
     (if (and (> m 2) (leap? y)) 1 0)))

(defun year-num (y)
  (let ((d 0))
    (if (>= y yzero)
	(dotimes (i (- y yzero) d)
	  (incf d (year-days (+ yzero i))))
	(dotimes (i (- yzero y) (- d))
	  (incf d (year-days (+ y i)))))))

(defun year-days (y) (if (leap? y) 366 365))

(defun num->date (n)
  (multiple-value-bind (y left) (num-year n)
    (multiple-value-bind (m d) (num-month left y)
      (values d m y))))

(defun num-year (n)
  (if (< n 0)
      (do* ((y (- yzero 1) (1- y))
	    (d (- (year-days y)) (- d (year-days y))))
	   ((<= d n) (values y (- n d))))
      (do* ((y yzero (1+ y))
	    (prev 0 d)
	    (d (year-days y) (+ d (year-days y))))
	   ((>= d n) (values y (- n prev))))))

(defun num-month (n y)
  (if (leap? y)
      (cond ((= n 59) (values 2 29))
	    ((> n 59) (nmon (1- n)))
	    (t (nmon n)))
      (nmon n)))

(defun nmon (n)
  (let ((m (position n month :test #'<)))
    (values m (+ 1 (- n (svref month (- m 1)))))))

(defun date+ (d m y n)
  (num->date (+ (date->num d m y) n)))
