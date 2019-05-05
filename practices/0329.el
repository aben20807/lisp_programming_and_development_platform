(defun nicknames (name)
  (list
   (concat "big " name)
   (concat name "y")))

(setq names '("fred" "ted" "ben"))

(mapcar #'nicknames names)(("big fred" "fredy") ("big ted" "tedy") ("big ben" "beny"))
(mapcan #'nicknames names)("big fred" "fredy" "big ted" "tedy" "big ben" "beny")

(defun all-nicknames (names)
  (if (null names)
      nil
      (nconc (nicknames (car names))
	     (all-nicknames (cdr names)))))

(all-nicknames names)("big fred" "fredy" "big ted" "tedy" "big ben" "beny")
names
