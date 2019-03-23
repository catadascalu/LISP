(defun maxValue (l)
  (cond
  ((numberp l) l)
  ((atom l) 0)
  ((listp l) (apply #'max(mapcar #'maxValue l)))
  )
)

(defun transf (l)
  (cond
  ((numberp l) (list l))
  ((atom l) (list 0))
  ((listp l) (apply #'max(mapcan #'transf l)))
  )
)
