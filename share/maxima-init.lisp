;; Pre-allocating memory and more aggressive allocate growth 
;; parameters substantially improves GCL performance
#+gcl
(progn

  #+gmp (si::set-gmp-allocate-relocatable t)

  ;; If you really need smaller initial memory footprint 
  ;; comment these pre-allocations out 
  (si::allocate-relocatable-pages 400 t)
  (si::allocate 'cons 	1000 t)
  (si::allocate 'fixnum 100  t)
  (si::allocate 'symbol 200  t)
  (si::allocate 'array	100  t)
  (si::allocate 'string	200  t)
  (si::allocate 'cfun 	400  t)

  ;; It is not recommended to make hole size less than 1000
  (si::set-hole-size 4000)

  (si::allocate-growth 'cons 	1 10000 66 33)
  (si::allocate-growth 'fixnum 	1 10000 66 33)
  (si::allocate-growth 'symbol 	1 10000 66 33)
  (si::allocate-growth 'package	1 10000 66 33)
  (si::allocate-growth 'array 	1 10000 66 33)
  (si::allocate-growth 'string 	1 10000 66 33)
  (si::allocate-growth 'cfun 	1 10000 66 33)
  (si::allocate-growth 'sfun 	1 10000 66 33)
  
  (si::allocate-growth 'relocatable  1 10000 66 33)
  ;(si::allocate-growth 'contiguous   1 10000 66 33)

  )
