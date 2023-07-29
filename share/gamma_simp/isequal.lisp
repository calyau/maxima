(defmfun $isequal (a b) 
  (eq t (meqp a b)))

(defmfun $isunequal (a b)
  (eq t (mnqp a b)))