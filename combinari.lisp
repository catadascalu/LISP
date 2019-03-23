(defun comb(L n)
  (cond
    ((= n 0) (list nil))
    ((NULL L) nil)
    (t (append(comb (cdr L) n) (mapcan #'(lambda(elem) (list(cons (CAR L) elem))) (comb(CDR L) (- n 1)))))))
(defun removeE(L e)
  (cond
    ((NULL L) nil)
    ((equal e (CAR L)) (CDR L))
    (t (cons (CAR L) (removeE (CDR L) e)))))
(defun perm(L)
  (cond
    ((NULL L) (list nil))
    (t (mapcan #'(lambda(elem) (mapcar #'(lambda(oneP) (cons elem oneP)) (perm (removeE L elem)))) L))))
(defun aranj(L n)
  (cond
  ((NULL L) (list nil))
  (t (mapcan #'(lambda(elem) (perm elem)) (comb L n)))
  )
)
