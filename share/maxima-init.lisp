;;pre-allocating memory substantial improves GCL performance
#+gcl
(progn
  (si::allocate-relocatable-pages 2000 t)
  (si::allocate 'cfun 200 t)
  (si::allocate 'fixnum 200 t)
  (si::allocate 'cons 400 t)
  (si::allocate 'symbol 100 t))
