(setf path (make-pathname :name "myfile"))
(setf str (open path :direction :output
		     :if-exists :supersede))

(format str "Something~%")
(close str)

(setf str (open path :direction :input))
(read-line str)
(close str)

(with-open-file (str path :direction :output
			  :if-exists :supersede)
  (format str "HOGEHOGE~%"))

(progn
  (format t "Please enter your name: ")
  (read-line))

(defun pseudo-cat (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof)
	       (read-line str nil 'eof)))
	((eql line 'eof))
      (format t "~a~%" line))))

(read-from-string "a b c")

(prin1 "Hello")
(princ "Hello")

(format nil "Dear ~A,~% Our records indicate..."
	"Mr. Malatesta")

(format t "~S ~A" "z" "z")

(format nil "~10,2,0,'*,' F" 26.21875)
(format nil "~,2,,,F" 26.21875)
(format nil "~,2F" 26.21875)

(defstruct buf
  vec (start -1) (used -1) (new -1) (end -1))

(defun bref (buf n)
  (svref (buf-vec buf)
	 (mod n (length (buf-vec buf)))))
