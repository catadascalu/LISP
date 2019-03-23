(defun concatenate(L1 L2)
  (cond
    ((NULL L1) L2)
    ((NULL L2) L1)
    ((and (not(NULL L1)) (not(NULL L2))) (CONS (CAR L2) (concatenate L1 (CDR L2))))
  )
)

(defun replace2(E L1 L2)
  (cond
    ((NULL L1) L1)
    ((= E (CAR L1)) (replace E (concatenate (CDR L1) L2) L2))
    ((not(= E (CAR L1))) (CONS (CAR L1) (replace E (CDR L1) L2)))
  )
)

(defun replace(E L1 L2)
  (cond
    ((NULL L1) L1)
    ((= E (CAR L1)) (concatenate (replace E (CDR L1) L2) L2))
    ((not(= E (CAR L1))) (CONS (CAR L1) (replace E (CDR L1) L2)))
  )
)
