(defun add_end(E L)
  (cond
    ((NULL L) (CONS E L))
    ((not(NULL L)) (CONS (CAR L) (add_end E (CDR L))))
  )
)

(defun reverse(L)
  (cond
    ((NULL L) L)
    ((not(NULL L)) (add_end (CAR L) (reverse (CDR L))))
  )
)
(defun get_div(L1 K1 C1)
  (cond
    ((= C1 0) (truncate(/ (+ L1 K1) 10)))
    ((not(= C1 0)) (truncate(/ (+ (+ L1 K1) C1) 10)))
  )
)

(defun get_mod(L1 K1 C1)
  (cond
    ((= C1 0) (mod (+ L1 K1) 10))
    ((not(= C1 0)) (mod (+ (+ L1 K1) C1) 10))
  )
)


(defun sum_of_lists(L K C)
  (cond
    ((and(and (NULL L) (NULL K)) (= C 0)) L)
    ((and(and (NULL L) (NULL K)) (not(= C 0))) (list C))
    (t (CONS (get_mod (CAR L) (CAR K) C) (sum_of_lists (CDR L) (CDR K) (get_div (CAR L) (CAR K) C))))
  )
)

(defun sum(L K C)
  (cond
    ((NULL L) L)
    ((not(NULL L)) (reverse (sum_of_lists L K C)))
  )
)

(defun sum_final(L K)
  (cond
    ((and(NULL L) (NULL K)) 0)
    ((NULL L) K)
    ((NULL K) L)
    ((not(or(NULL L) (NULL K))) (reverse (sum_of_lists (reverse L) (reverse K) 0)))
  )
)
