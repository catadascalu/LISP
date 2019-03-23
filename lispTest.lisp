
(defun subst(L L1 E)
  (cond
  ((NULL L) NIL)
  ((and(numberp (CAR L))(equal(CAR L) E)) (APPEND L1 (subst (CDR L) L1 E)))
  ((and(numberp (CAR L))(not(equal (CAR L) E))) (APPEND (list(CAR L)) (subst (CDR L) L1 E)))
  ((and(atom (CAR L))(equal (CAR L) E)) (APPEND L1 (subst (CDR L) L1 E)))
  ((and(atom (CAR L))(not(equal (CAR L) E))) (APPEND (list(CAR L)) (subst (CDR L) L1 E)))
  ((listp (CAR L)) (CONS (subst (CAR L) L1 E) (subst (CDR L) L1 E)))
  )
)

(defun concat(L1 L2)
  (cond
    ((NULL L1) L2)
    ((NULL L2) L1)
      (t (concat(APPEND L1 (list(CAR L2))) (CDR L2)))
  )
)

(defun subst2(L L1 E)
  (cond
  ((NULL L) list())
  ((equal(CAR L) E)(APPEND L1 (subst (CDR L) L1 E)))
  ((not(equal (CAR L) E)) (APPEND (list(CAR L)) (subst (CDR L) L1 E)))
  ((listp (CAR L)) (CONS (subst (CAR L) L1 E) (subst (CDR L) L1 E)))
  )
)
