(require 'cl-lib)
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
    ((equal "nil" (get-key-of-root tree)) tree) ; Return current root if not found
    ((= key (get-key-of-root tree)) tree)
    ((< key (get-key-of-root tree))(search-key (get-l-subtree tree) key))
    ((> key (get-key-of-root tree))(search-key (get-r-subtree tree) key))
    ))

(defun insert (tree key val)
  (let ((target (search-key tree key)))
    (if (equal "nil" (get-key-of-root target)) ; If target have not initialized the key
      (progn
        (set-key-of-root target key)
        (set-val-of-root target (cons val nil))
        (set-l-subtree target (create-node))
        (set-r-subtree target (create-node))
        ;        [car|cdr]
        ; [caar|cdar] [cadr|cddr]
        ; (key) (val)   |    |
        ;               v    v
        ;            [node][node]
        )
      (set-val-of-root target (cons val (get-val-of-root target))) ; Add (preppand) value into a list
      )
    ))

(defun get-val-by-key (tree key)
  (let ((ret (search-key tree key)))
    (if (equal "nil" (get-key-of-root ret))
      (prog1 nil (print "not found!")) ; Return nil when key is not found
      (get-val-of-root ret))
    ))

(defun str-length-of-val (tree key)
  "Return the length of all strings in val."
  (apply #'+ (mapcar #'length (get-val-by-key tree key)))
  )

(defun length-of-root (tree)
  "Return the length of the root node"
  (+ 6
     (length (number-to-string (get-key-of-root tree)))
     (str-length-of-val tree (get-key-of-root tree))
     (- (length (get-val-by-key tree (get-key-of-root tree))) 1)
     ))

(defun print-btree-helper (tree indent prefix)
  (if (or (equal "nil" tree) (equal "nil" (get-key-of-root tree)))
    ()
    (print-btree-helper (get-r-subtree tree) (+ indent (length-of-root tree)) "↗")
    (princ (make-string indent ?\ )) ; Create the indent spaces
    (princ prefix)
    (princ "[")
    (princ (get-key-of-root tree))
    (princ "|")
    (princ (get-val-of-root tree))
    (princ "]\n")
    (print-btree-helper (get-l-subtree tree) (+ indent (length-of-root tree)) "↘")
    ))

(defun print-btree (tree)
  (print-btree-helper tree 0 "→") t)

(defun num-of-child (node)
  "Return the number of child of node by checking if the child node is same as empty node."
  (cond
    ((equal "nil" (get-key-of-root node))
      (prog1 nil (print "not found! cannot get the number of child."))) ; Return nil when key is not found
    ((and (equal (create-node) (get-l-subtree node)) (equal (create-node) (get-r-subtree node))) 0)
    ((and (not (equal (create-node) (get-l-subtree node))) (not (equal (create-node) (get-r-subtree node)))) 2)
    (t 1)
    ))

(defun biggest-node (tree)
  (if (equal "nil" (get-key-of-root (get-r-subtree tree)))
    tree
    (biggest-node (get-r-subtree tree))
    ))

(defun delete-node (tree key)
  (let ((target (search-key tree key)))
    (cond
      ((equal "nil" (get-key-of-root target))
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

(defun size-of-r-tree (tree size)
  (if (equal "nil" (get-key-of-root (get-r-subtree tree)))
    (+ size 1)
    (size-of-r-tree (get-r-subtree tree) (+ size 1))
    )
  )


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
         (tree-to-vine tail) ; Recursive
       ))
    )))

(defun compress (root cnt)
  (let ((scanner root))
    (cl-loop for i from 1 to cnt do
      (let ((child (get-r-subtree scanner)))
        (if (equal "nil" child) ()
          (if (or (equal "nil" (get-r-subtree scanner)) (equal "nil" (get-r-subtree child))) ()
            (set-r-subtree scanner (get-r-subtree child))) ; else
          (setq scanner (get-r-subtree scanner))
          (if (or (equal "nil" (get-r-subtree child)) (equal "nil" (get-l-subtree scanner))) ()
            (set-r-subtree child (get-l-subtree scanner))) ; else
          (set-l-subtree scanner child)
        ))
      )
    ))

(defun vine-to-tree (root size)
  (let ((leaves (- (+ size 1 ) (expt 2 (floor (log (+ size 1) 2)))))) ; Get the number of leaves
    ; (print leaves)
    (compress root leaves)
    (setq size (- size leaves))
    (while (> size 1)
            ; (print-btree root) ; debug
             (compress root (floor (/ size 2)))
             (setq size (floor (/ size 2)))
             )
    ))

(defun balance (tree)
  (let ((pse (init)))
    ; pseudo-root
    (insert pse -1 "pseudo-root")
    (setcdr (cdr pse) tree)
    ; (print-btree pse)

    ; Convert into a sorted list
    (tree-to-vine pse)
    ; (print-btree pse)

    ; Compress a balanced tree
    (vine-to-tree pse (- (size-of-r-tree pse 0) 1))

    ; Remove the pseudo-root
    (delete-node pse -1)
    pse ; Return the balanced tree
    ))


(setq debug-on-error t)
(setq R nil)
(let ((R (init))
      (pse (init)))
  ; (insert R 4 "OuO")
  ; (print R)
  (insert R 1 "OuO")
  (insert R 2 "OuO")
  (insert R 3 "OuO")
  ; (insert R 9 "OuO")
  ; (insert R 0 "OuO")
  ; (print R)
  (dotimes (i 20)
    (insert R (random 20) "OuO"))
  ; (insert R 2 "OuO")
  ; (insert R 3 "OuO")
  ; (insert R 1 "OuO")
  ; (insert R 0 "OuO")
  (print-btree R)

  ; (princ "---\n")
  ; (print (num-of-child (search-key R 4)))
  ; (print (get-val-by-key R 4))
  ; (delete-node R 4)
  ; (print-btree R)

  (princ "\n\n---\n\n\n")
  (setq R (balance R))
  (print-btree R)
  ; (print (size-of-r-tree R 0))
  )
