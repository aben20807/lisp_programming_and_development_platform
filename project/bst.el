(defun init ()
  (create-node))

(defun create-node ()
  "A node contains three cons to store 4 value: ((key . value) left . right)"
  (cons (cons "nil" "nil") (cons "nil" "nil")))

(defun get-key-of-root (node)
  (caar node))

(defun set-key-of-root (node key)
  (setcar (car node) key))

(defun get-val-of-root (node)
  (cdar node))

(defun set-val-of-root (node val)
  (setcdr (car node) val))

(defun get-l-subtree (node)
  (cadr node))

(defun set-l-subtree (node lnode)
  (setcar (cdr node) lnode))

(defun get-r-subtree (node)
  (cddr node))

(defun set-r-subtree (node rnode)
  (setcdr (cdr node) rnode))

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
    ((equal "nil" (caar node))
      (prog1 nil (print "not found! cannot get the number of child."))) ; Return nil when key is not found
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
      ((= 0 (num-of-child target)) ; Directly delete the node
       (setcar target (cons "nil" "nil"))
       (setcdr target (cons "nil" "nil")))
      ((= 1 (num-of-child target)) ; Let the child to replace target
       (cond
         ((equal "nil" (caar (cadr target))) ; Has node in the right
          (setcar target (car (cddr target)))
          (setcdr target (cdr (cddr target))))
         ((equal "nil" (caar (cddr target))) ; Has node in the left
          (setcar target (car (cadr target)))
          (setcdr target (cdr (cadr target))))
         (t (error "never reach"))
         )
       )
      ((= 2 (num-of-child target))
       (let ((big-kv (car (biggest-node (cadr target))))) ; Store the key and value
         (delete-node tree (car big-kv)) ; Delete the biggest node smaller than target
         (setcar target big-kv)
       )))
    ))


(defun tree-to-vine (tree)
  (let ((tail tree)
        (rsst (get-r-subtree tree)))
    (cond
      ((equal "nil" (get-key-of-root rsst)) ())
      ((equal "nil" (get-key-of-root (get-l-subtree rsst)))
       (tree-to-vine rsst))
      (t
       (let ((tamp (get-l-subtree rsst)))
         (set-l-subtree rsst (get-r-subtree tamp))
         (set-r-subtree tamp rsst)
         (set-r-subtree tail tamp)
         (tree-to-vine tail)
       ))
    )))

(defun balance (tree)
  (let ((pse (init)))
    ; pseudo-root
    (insert pse -1 "pseudo-root")
    (setcdr (cdr pse) tree)
    (print-btree pse)
    (princ "---\n")
    (tree-to-vine pse)
    (print-btree pse)
    (delete-node pse -1)
    pse
    ))

(defun print-btree (tree)
  (print-btree-helper tree 0 "→") t)


(setq debug-on-error t)
(setq R nil)
(let ((R (init))
      (pse (init)))
  (dotimes (i 20)
    (insert R (random 10) "OuO"))
  ; (insert R 2 "OuO")
  ; (insert R 3 "OuO")
  ; (insert R 1 "OuO")
  ; (insert R 4 "OuO")
  ; (insert R 0 "OuO")
  (print-btree R)

  (princ "---\n")
  ; (print (num-of-child (search-key R 4)))
  ; (print (get-val R 4))
  ; (delete-node R 4)
  ; (print-btree R)

  (princ "---\n")
  (setq R (balance R))
  (print-btree R)
  )
