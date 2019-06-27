(sort "elbow" #'char<)
(aref "abc" 1)
(char "abc" 1)

(let ((str (copy-seq "Merlin")))
  (setf (char str 3) #\k)
  str)

(equal "fred" "fred")
(equal "fred" "Fred")
(string-equal "fred" "Fred")

(format nil "~a or ~a" "truth" "dare")

(concatenate 'string "not " "to worry")
(concatenate 'list "not " "to worry")
