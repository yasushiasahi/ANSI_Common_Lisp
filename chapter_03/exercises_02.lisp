(defun new-union (list-a list-b)
  (if list-b
      (if (member (car list-b) list-a)
	  (new-union list-a
		     (cdr list-b))
	  (new-union (append list-a (list (car list-b)))
		     (cdr list-b)))
      list-a))

;; (defun occurrences list
;;   (let (result)
;;     (labels ((count (dlist)
;; 	       (if (eql (car dlist) (cadr dlist))
;; 		   (1+ (count (cdr dlist)))
;; 		   ((count (cdr dlist))))))
;;       )))

(defun occurrences (list)
  (let (result)
    (labels ((func (lst)
	       (if lst
		   (let ((c (car lst)))
		     (push (cons c (count c lst))
			   result)
		     (func (remove c lst))))))
      (func list)
      (sort result #'> :key #'cdr))))


;; (defun count (dlist)
;;   (if (eql (car dlist) (cadr dlist))
;;       (1+ (count (cdr dlist)))
;;       (count (cdr dlist))))

(defun pos+-re (list)
  (let (acc
	(len (length list)))
    (labels ((func (lst)
	       (if lst
		   (let ((idx (- len (length lst))))
		     (push (+ idx (car lst))
			   acc)
		     (func (cdr lst))))))
      (func list)
      (reverse acc))))

(defun pos+-map (list)
  (let ((idx -1))
    (mapcar (lambda (el)
	      (setf idx (1+ idx))
	      (+ idx el))
	    list)))

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
      ;;(list n elt)
      (cons n elt)
      elt))

(setf l '(1 1 1 0 1 0 0 0 0 1))

(defun showdots (list)
  (let ((close-tag (append (list nil)
			   (mapcar (lambda (el)
				     (format t "(~a . " el)
				     ")")
				   list))))
    (mapc #'princ close-tag)))

(defun showdots (list)
  (let ((close-tag (append (list nil)
			   (mapcar (lambda (el)
				     ;;(format t "~%el:~a consp:~a   " el (consp el))
				     (if (consp el)
					 (progn
					   (showdots el)
					   (format t " . ")
					   "")
					 (progn
					   (format t "(~a . " el)
					   ")")))
				   list))))
    ;;(format t "~%close-tag:~a     " close-tag)
    (mapc #'princ close-tag)
    nil))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end)
	      (reverse path)
	      (bfs end
		   (append (cdr queue)
			   (new-path path node net))
		   net))))))

(defun new-path (path node net)
  (mapcar #'(lambda (n)
	      (cons n path))
	  (cdr (assoc node net))))

(setf min '((a b c) (b c) (c d)))
