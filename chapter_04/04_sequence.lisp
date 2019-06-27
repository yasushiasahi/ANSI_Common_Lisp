(elt '(a b c) 1)
(elt #(a b c) 1)
(elt "abc" 1)

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
	 (do ((forward 0 (+ forward 1))
	      (back (- len 1) (- back 1)))
	     ((or (> forward back)
		  (not (eql (elt s forward)
			    (elt s back))))
	      (> forward back))))))

(position #\a "fantasia")
(position #\a "fantasia" :start 3 :end 5)
(position #\a "fantasia" :from-end t)
(position 'a '((c d) (a b)) :key #'car)
(position '(a b) '((c d) (a b)) :test #'equal)
(position 3 '(1 0 7 5) :test #'<)

(defun my-second-word (str)
  (let* ((start (1+ (position #\space str)))
	 (end (position #\space str :start start)))
    (subseq str start end)))

(position-if #'oddp '(2 3 4 5))

(find #\a "cat")
(find-if #'characterp "ham")

(find-if #'(lambda (x)
	     (eql (car x) 'complate))
	 lst)

(find 'compalate lst :key #'car)

(remove-duplicates "abracadabra")

(reduce #'intersection '((b r a d 's) (b a d) (c a t)))

(defun tokens (str test start)
  (let ((p1 (position-if test str :start start)))
    (if p1
	(let ((p2 (position-if #'(lambda (c)
				   (not (funcall test c)))
			       str :start p1)))
	  (cons (subseq str p1 p2)
		(if p2
		    (tokens str test p2)
		    nil)))
	nil)))

(defun constituent (c)
  (and (graphic-char-p c)
       (not (char= c #\space))))

(tokens "ab12 3cde.f
gh" #'alpha-char-p 0)

(tokens "ab12 3cde.f
gh" #'constituent 0)

(defun parse-date (str)
  (let ((toks (tokens str #'constituent 0)))
    (list (parse-integer (first toks))
	  (parse-month (second toks))
	  (parse-integer (third toks)))))

(defconstant month-names
  #("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec"))

(defun parse-month (str)
  (let ((p (position str month-names
		     :test #'string-equal)))
    (if p
	(+ p 1)
	nil)))
