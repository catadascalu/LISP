(defun merge(L1 L2)
  (cond
      ((NULL L2) L1)
      ((NULL L1) L2)
      ((< (CAR L1) (CAR L2)) (CONS (CAR L1) (merge (CDR L1) L2)))
      ((> (CAR L1) (CAR L2)) (CONS (CAR L2) (merge L1 (CDR L2))))
      ((= (CAR L1) (CAR L2)) (CONS (CAR L1) (CONS (CAR L2) (merge (CDR L1) (CDR L2)))))
    )
)
