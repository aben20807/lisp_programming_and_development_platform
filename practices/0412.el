(setqA
      '((pine . cone) (oak . acorn)))
(intern-soft aa)
(intern-soft pine)

(setq A
      '(("pine" . "cone") ("oak" . "acorn")))
(intern-soft "pine")

(cl-defun foo
    (&key (fred "fredy")
	  &key (ted "teddy"))
  (format "fred is %s, ted is %s" fred ted))
(foo)

(define-derived-mode dict-mode text-mode "dictionary mode"
    (local-set-key "\t" #'self-insert-command))
