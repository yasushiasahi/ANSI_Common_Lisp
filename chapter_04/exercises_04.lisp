(defun quarter-turn (array)
  (let* ((dim (array-dimensions array))
	 (len (car dim))
	 (newArr (make-array dim)))
    (loop for i below len
	  do (loop for j below len
		   do (setf (aref newArr j (- len 1 i)) (aref array i j))))
    newArr))



'((a b c d)		      ;第一要素の最後の値以外は右に1ずれる
  (e f g h)		      ;cdr要素のcarは上に1ずれる
  (i j k l)		      ;最後の要素以外の最後の要素は下に1ずれる
  (m n o p))				;最終要素のcdrは左に1ずれる


'((e a b c)
  (i j f d)
  (m k g h)
  (n o p l))




(setf l '((nil nil nil a)
	  (nil nil nil b)
	  (nil nil nil c)
	  (nil nil nil d)))

(defun hoge (list)
  (let* ((parent-len (length list))
	 (child-lens (mapcar #'length list))
	 (turn? (every (lambda (v) (eql v parent-len))
		       (cons parent-len child-lens))))
    (if turn?
	(let* ((i 0)
	       (lest-list (mapcar (lambda (c-lst)
				    (format t "i:~a" i)
				    (cond ((eql i 0) (progn (setf i (1+ i))
							    (subseq c-lst 0 (1- parent-len))))
					  ((eql i (1- parent-len)) (progn (setf i (1+ i))
									  (subseq c-lst 1)))
					  (t (progn (setf i (1+ i))
						    (cons (car c-lst) (last c-lst))))))
				  list))
	       (fuga-list (mapcar #'remove-duplicates list lest-list)))
	  fuga-list))))


(defun quarter-turn-list (list)
  (let* ((len (length list))
	(acc (make-list len)))
    (let ((i (1- len)))
      (mapc (lambda (child-list)
	      (let ((j (1- len)))
		(mapc (lambda (item)
			;;(setf (nth j (nth i acc)) item)
			(setf (nth j acc) (cons item (nth j acc)))
			(format t "i:~a j:~a acc:~a~%" i j acc)
			(setf j (1- j)))
		      (reverse child-list)))
	      (setf i (1- i)))
	    list))
    acc))

(defun quarter-turn-try (list)
  (let* ((len (length list))
	 (acc (make-list len)))
    (mapc (lambda (child-list)
	    (let ((idx (1- len)))
	      (mapc (lambda (item)
		      (format t "idx:~a acc:~a~%" idx acc)
		      (setf (nth idx acc) (cons item (nth idx acc)))
		      (setf idx (1- idx)))
		    (reverse child-list))))
	  list)
    acc))

(setf l '((a b c d)
	  (e f g h)
	  (i j k l)
	  (m n o p)))


(setf  '((M I E A)
	  (N J F B)
	  (O K G C)
	  (P L H D)))

(setf nl (make-list 4 :initial-element (make-list 4)))

(do ((i 0 (1+ i)))
    ((< i 5) (format t "~a~%" i)))

 (do ((temp-one 1 (1+ temp-one))
       (temp-two 0 (1- temp-two)))
     ((> (- temp-one temp-two) 5) (format t "~a ~a~%" temp-one temp-two)))

(loop for i in #(a b) collect i)

(loop for i below 5 do (print i))



(setf arr #2a((A B C D)
	      (E F G H)
	      (I H K L)
	      (M N O P)))


(defun my-copy-list (list)
  (reduce (lambda (acc val)
	    (cons (if (consp val)
		      (my-copy-list val)
		      val)
		  acc))
	  (reverse list)
	  :initial-value nil))

(defun my-reverse (list)
  (reduce (lambda (acc val)
	    (cons (if (consp val)
		      (my-copy-list val)
		      val)
		  acc))
	  list
	  :initial-value nil))

(setf alist '((1 . "ぬ") (2 . "ふ") (3 . "う") (4 . "え") (5 . "お") (6 . "７") (8 . "や") (9 . "ゆ")))

(defun alist-to-hash (alist)
  (let ((hash (make-hash-table)))
    (mapc (lambda (dot)
	    (setf (gethash (car dot) hash) (cdr dot)))
	  alist)
    hash))

(setf hash (alist-to-hash alist))

(defun hash-to-alist (hash)
  (let (alist)
    (maphash (lambda (key val)
	       (setf alist (cons (cons key val)
				 alist)))
	     hash)
    alist))
