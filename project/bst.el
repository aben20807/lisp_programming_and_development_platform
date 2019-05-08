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

(defun biggest-node (tree)
  (if (equal "nil" (caar (cddr tree)))
    tree
    (biggest-node (cddr tree))
    ))

(defun delete-node (tree key)
  (let ((target (search-key tree key)))
    (cond
      ((equal "nil" (caar target))
        (prog1 nil (print "not found! cannot delete"))) ; Return nil when key is not found
      ((= 0 (num-of-child target))
       (setcar target (cons "nil" "nil"))
       (setcdr target (cons "nil" "nil")))
      ((= 1 (num-of-child target))
       (cond
         ((equal "nil" (caar (cadr target)))
          (setcar target (car (cddr target)))
          (setcdr target (cdr (cddr target))))
         ((equal "nil" (caar (cddr target)))
          (setcar target (car (cadr target)))
          (setcdr target (cdr (cadr target))))
         (t (print (cadr target)))
         )
       )
      ((= 2 (num-of-child target))
       (let ((big-kv (car (biggest-node (cadr target)))))
         (delete-node tree (car big-kv))
         (setcar target big-kv)
       )))
    ))

(defun print-btree (tree)
  (print-btree-helper tree 0 "→") t)


(setq debug-on-error t)
(setq R nil)
(let ((R (init)))
  ; (dotimes (i 10)
  ;   (insert R (random 10) "OuO"))
  ; (insert R 4 "OuO")
  ; (insert R 4 "OuO")
  ; (insert R 2 "OuO")
  ; (insert R 3 "OuO")
  ; (insert R 3 "OuO")
  ; (insert R 5 "OuO")
  (dotimes (i 20)
    (insert R (random 10) "OuO"))
  (print-btree R)
  (print (num-of-child (search-key R 4)))
  (delete-node R 4)
  (print-btree R)
  )
