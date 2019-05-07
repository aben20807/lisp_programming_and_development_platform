(defun init ()
  (create-node))

(defun create-node ()
  "A node contains three cons to store 4 value: ((key . value) left . right)"
  (cons (cons "nil" "nil") (cons "nil" "nil")))

(defun search-key (tree key)
  (cond
    ((equal "nil" (caar tree)) tree) ; Return current root if not found
    ((= key (caar tree)) tree)
    ((< key (caar tree))(search-key (cadr tree) key))
    ((> key (caar tree))(search-key (cddr tree) key))
    ))

(defun insert (tree key val)
  (let ((target (search-key tree key)))
    (if (equal "nil" (caar target)) ; If target have not initialized the key
      (progn
        (setcar target (cons key (cons val nil)))           ; Init a val into a list
        (setcdr target (cons (create-node) (create-node)))  ; Create left and right node
        ;        [car|cdr]
        ; [caar|cdar] [cadr|cddr]
        ; (key) (val)   |    |
        ;               v    v
        ;            [node][node]
        )
      (setcdr (car target) (cons val (cdar target))) ; Add (preppand) value into a list
      )
    ))

(defun get-val (tree key)
  (let ((ret (search-key tree key)))
    (if (equal "nil" (caar ret))
      (prog1 nil (print "not found!")) ; Return nil when key is not found
      (cdar ret))
    ))

(defun str-length-of-val (tree key)
  "Return the length of all strings in val."
  (apply #'+ (mapcar #'length (get-val tree key)))
  )

(defun length-of-root (tree)
  "Return the length of the root node"
  (+ 6
     (length (number-to-string (caar tree)))
     (str-length-of-val tree (caar tree))
     (- (length (get-val tree (caar tree))) 1)
     ))

(defun print-btree-helper (tree indent prefix)
  (if (or (equal "nil" tree) (equal "nil" (caar tree)))
    ()
    (print-btree-helper (cddr tree) (+ indent (length-of-root tree)) "↗")
    (princ (make-string indent ?\ )) ; Create the indent spaces
    (princ prefix)
    (princ "[")
    (princ (caar tree))
    (princ "|")
    (princ (cdar tree))
    (princ "]\n")
    (print-btree-helper (cadr tree) (+ indent (length-of-root tree)) "↘")
    ))

(defun num-of-child (node)
  "Return the number of child of node by checking if the child node is same as empty node."
  (cond
    ((and (equal (create-node) (cadr node)) (equal (create-node) (cddr node))) 0)
    ((and (not (equal (create-node) (cadr node))) (not (equal (create-node) (cddr node)))) 2)
    (t 1)
    ))

(defun print-btree (tree)
  (print-btree-helper tree 0 "→") t)


(setq debug-on-error t)
(setq R nil)
(let ((R (init)))
  (dotimes (i 10)
    (insert R (random 10) "OuO"))
  (insert R 3 "OuO")
  (print-btree R)
  (print  (search-key R 3))
  (print  (get-val R 30))
  (print (cadr (search-key R 3)))
  (print (num-of-child (search-key R 3)))
  )
