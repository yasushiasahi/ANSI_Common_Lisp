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
