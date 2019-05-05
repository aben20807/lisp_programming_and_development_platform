(setq L '(("red" 3) ("blue" 4)))
(assoc "red" L) ; ("red" 3)
(elt L 0) ; ("red" 3)
(nth 0 L) ; ("red" 3)
(aref L 0) ; wrong type
(push '("green" 2) L)
(setf)


(setq V [3 4 5])
V ; [3 4 5]
(aset V 0 13)
V ; [13 4 5]
(aref V 0) ; 13
(elt V 0) ; 13
(nth 0 V) ; wrong type

(defun each-letter (s)
  (cl-loop for i from 0 to (- (length s) 1) do
	   (message "%c at %d" (elt s i) i)))
(each-letter "dog")

(defun each-letter (s)
  (cl-loop
   for c elements of s using (index i) do
           (message "%c at %d" c i)))
(each-letter "dog")
