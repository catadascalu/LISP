(defun left_subtree(L M N)
  (cond
    ((NULL L) NIL)
    ((<= N M) (append (list (CAR L) (CADR L)) (left_subtree (CDDR L) (+ M (CADR L)) (+ N 1))))
    ((= N (+ M 1)) NIL)
  )
)

(defun LT(L)
  (cond
    ((NULL L) NIL)
    (t (left_subtree (CDDR L) 0 0))
  )
)

(defun right_subtree(L M N)
  (cond
    ((NULL L) NIL)
    ((<= N M) (right_subtree (CDDR L) (+ M (CADR L)) (+ N 1)))
    ((= N (+ M 1)) L)
  )
)

(defun RT(L)
  (cond
    ((NULL L) NIL)
    (t (right_subtree (CDDR L) 0 0))
  )
)

(defun max_height(LEFT RIGHT)
  (cond
    ((<= LEFT RIGHT) RIGHT)
    ((> LEFT RIGHT) LEFT)
  )
)

(defun count_levels(L)
  (cond
    ((NULL L) 0)
    ((and (NULL (LT L)) (NULL (RT L))) 1)
    (t (+ 1 (max_height (count_levels (LT L)) (count_levels (RT L)))))
  )
)
