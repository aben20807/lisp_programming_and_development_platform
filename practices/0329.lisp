(defun nicknames (name)
  (list
   (concatenate 'string "big " name)
   (concatenate 'string name "y")))

(setq names '("fred" "ted" "ben"))

(mapcar #'nicknames names)(("big fred" "fredy") ("big ted" "tedy") ("big ben" "beny"))
(mapcan #'nicknames names)("big fred" "fredy" "big ted" "tedy" "big ben" "beny")

(defun all-nicknames (names)
  (if (null names)
      nil
      (nconc (nicknames (car names))
	     (all-nicknames (cdr names)))))

(all-nicknames names)("big fred" "fredy" "big ted" "tedy" "big ben" "beny")


(setq bookshop-data
      '(("townA" . ("shop3" "shop4"))
	("townB" . ("shop5"))))

(defun bookshops (town)
  (assoc town bookshop-data))

(list "townA" (bookshops "townA"))
(values "townA" (bookshops "townA"))

(let ((town (find-if #'bookshops towns)))
  (values town (bookshops town)))

(defun find-books (towns)
  (if (null towns)
      nil
      (let ((shops (bookshops (car towns))))
	(if shops
	    (values (car towns) shops)
	    (find-books (cdr towns))))))
