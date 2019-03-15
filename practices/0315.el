(set 'foo 7)
foo

(defun foo () 9)
(setq foo 8)
foo

(setq goo 'foo)
(set goo 45)
foo

(defun add2 (x)
  (+ 2 x))
(mapcar #'add2
	'(1 2 3))

(setq p #'add2)
(mapcar p '(3 4 5))

(mapcar (lambda (x) (+ 2 x))
	'(5 6 7))

(progn
  1
  (setq bar 8)
  (message "%s is %s" 'bar bar)
  (setq bar 9)
  (message "%s turn to %s" 'bar bar)
  99
  )

(defun new-func () 3)
(new-func)
