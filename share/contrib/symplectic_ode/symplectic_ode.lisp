;;;; Common Lisp/Maxima code for symplectic integration of time independent separable hamiltonian 
;;;; systems.

;;;; Copyright (C) 2017, Barton Willis <willisb@unk.edu>
;;;; This work is licensed under a Creative Commons Attribution 4.0 International License.

(in-package :maxima)

;;; Let's have version numbers 1,2,3, ...
($put "symplectic_ode" 1 '$version)

;;; Define a mlist msimpind. This definition should be somewhere else.
(setf (get 'mlist 'msimpind) (list 'mlist 'simp))

;;; A Maxima coerce function. This function is adequate for the needs of symplectic_ode, but
;;; it might be useful to extend it to additional types.
(defun mcoerce (x ntype)
  "Either convert x to type ntype or signal an error. The allowable values of ntype are float, bfloat, fixnum, rational, and any (no coercion)." 
  (let ((type-test) (xx x))
       (cond 
         ((eql ntype '$float) 
          (setq xx ($float x))
          (setq type-test #'$floatnump)) ;what happens to binary32 numbers?

         ((eql ntype '$bfloat) 
          (setq xx ($bfloat x))
          (setq type-test #'$bfloatp))

         ((eql ntype 'fixnum)
          (setq xx (coerce x 'fixnum))
          (setq type-test #'fixnump))

         ((eql ntype '$rational)
          (setq xx ($rationalize x))
          (setq type-test #'$ratnump))

         ((eql ntype '$any) 
          (setq type-test #'(lambda (x) (declare (ignore x)) t)))

         (t (merror (intl:gettext "Unknown type ~M") ntype)))
       (if (funcall type-test xx) xx
           (merror (intl:gettext "Unable to convert ~M to type ~M") x ntype))))

;;; Possibly coerce-float-fun (defined in plot.lisp) is an alternative to expr-to-compiled-fun. 
;;; But unlike coerce-float-fun, the function expr-to-compiled-fun calls the compiler. After 
;;; compiling, return the function identifier.

(defun expr-to-compiled-fun (e ntype args)
  "Return a compiled CL function args |--> e where the args are modedeclared to be type ntype. When ntype isn't one of the types 
  accepted by modedeclare, the type is changed to the type any. The argument args must be a CL list. Return the identifier for the function."

  (unless (member ntype (list '$any '$boolean '$fixnum '$number '$rational '$float))
    (mtell  (intl:gettext  "Type ~M is unknown to the compiler; changing to type any.") ntype)
    (setq ntype '$any))

  (let ((f (gensym)))
       (meval `((mdefine) ((,f) ,@args) ((mprogn) (($modedeclare) ((mlist) ,@args) ,ntype) ,e)))
       (second (mfuncall '$compile f)))) ;compile returns a Maxima list of function identifiers.

;;; The Poisson bracket {f,g}.
(defun $poisson_bracket (f g p q)
  "Return the poisson bracket {f,g} for the canonical coordinates p & q. Either both p & q must be
  Maxima lists or both must be equal length mapatoms."
  (cond 
    ((and ($listp p) ($listp q))
     (setq p (rest p))
     (setq q (rest q)))
    ((and ($mapatom p) ($mapatom q))
     (setq p (list p))
     (setq q (list q)))
    (t 
     (merror (intl:gettext 
              "Either the third and fourth arguments to poisson_bracket must (i) both be lists or (ii) both be mapatoms"))))

  (reduce #'add 
          (mapcar #'(lambda (pk qk) (sub 
                                     (mul ($diff f qk) ($diff g pk)) 
                                     (mul ($diff f pk) ($diff g qk)))) p q)))

;;; Hash table for coefficients of symplectic methods; each value is a list of the 
;;; form ((c1 d1) (c2 d2) ...)

(defvar *symplectic-method-coefficients* (make-hash-table :size 4 :test 'eql))

(setf (gethash '$symplectic_euler *symplectic-method-coefficients*) 
      (list (cons 1 1)))

(setf (gethash '$verlet *symplectic-method-coefficients*) 
      (list (cons 0 (div 1 2)) 
            (cons 1 (div 1 2))))

(setf (gethash '$symplectic_third_order *symplectic-method-coefficients*)
      (list (cons 1 (div -1 24)) 
            (cons (div -2 3) (div 3 4)) 
            (cons (div  2 3) (div 7 24))))

(setf (gethash '$symplectic_fourth_order *symplectic-method-coefficients*)
      (let ((k (div 1 (sub 2 (power 2 (div 1 3))))))            
           (list 
            (cons (div k 2) k)
            (cons (div (mul (sub 1 (power 2 (div 1 3))) k) 2) (mul (mul -1 (power 2 (div 1 3))) k))
            (cons (div (mul (sub 1 (power 2 (div 1 3))) k) 2) k)
            (cons (div k 2) 0))))

;;; Usage: symplectic_ode(ham, [p1,..., pn], [q1, ... , qn], 
;;;                    [p01, ... p0n], [q01, ... q0n], dt, N, [method], [ntype])

;;;   ham = time independent hamiltonian (function of p1 ... pn and q1 ... qn only)
;;;   p1 ... pn; q1 ... qn = canonical  momenta and position, respectively
;;;   p01 .. p0n, q01 ... q0n  = initial values for p1 ... pn & q1 ... qn, respectively
;;;   dt = time step--can be negative
;;;   N = number of time steps--must be a positive fixnum
;;;   method = integration method (default symplectic_euler); must be one of symplectic_euler, 
;;;            verlet, symplectic_third_order, symplectic_fourth_order
;;;   ntype = optional number type (default float)--can be float, rational, or any (no type)

;;; The function symplectic returns a two member list of two list. The first list has the form
;;; [[p00,p10,p20, ...],[p01,p11, p21, ..] ...] where pij = i-th momentum at time step j.

;;; For a separable hamiltonian of the form ham = F(p) + G(q), symplectic_ode approximately 
;;; integrates the hamiltonian equation using a method that preserves the poisson brackets of p & q. 
;;; For a description, see https://en.wikipedia.org/wiki/Symplectic_integrator#A_first-order_example. 
;;; For non separable hamiltonians, the method does not in general preserve the poisson brackets.

;;; Basically, the method is

;;; for k = 1 ... n
;;;   q <-  q + c(k)*diff(ham,p)*dt
;;;   p <-  p - d(k)*diff(ham,q)*dt ;use updated position q

;;; For example, the symplectic Euler has c(1)=1, d(1)=1, and n=1. This code has built-in methods 
;;; for the symplectic Euler, the Verlet method, and third and fourth order methods due to Ronald Ruth.
;;; A hash table *symplectic-method-coefficients* contains the coefficients for c(1), d(1), ... for 
;;; each of the methods. 

;;; There is no user level mechanism for appending methods, but a new method can be 
;;; added by including a new entry in the hash table *symplectic-method-coefficients*. The coefficients
;;; should be Maxima expressions in exact form---depending on the optional value ntype (number type)
;;; the coefficients are converted at runtime to the correct type.

;;; This function creates and compiles functions for updating p & q. The arguments to these functions
;;; are modedeclared to have type ntype (default $float). Since float is a Maxima option variable,
;;; users need to remember to quote float. Of course, hand coding of these functions could increase 
;;; speed or accuracy, but the automatically generated functions are convenient.

(defun $symplectic_ode (ham p q po qo dt NN &optional (sym-method '$symplectic_euler) (ntype '$float))
  "symplectic ode solver: Usage: (non scalar or scalar version)
       symplectic_ode(ham, [p1,..., pn], [q1, ... , qn], [p01, ... p0n], 
             [q01, ... q0n], dt, N, [method], [ntype]) 
       symplectic_ode(ham, p, q, po, qo, dt, [method],[ntype])
  
  where
  
  ham = time independent hamiltonian (function of p1 ... pn and q1 ... qn only)
  p1 ... pn; q1 ... qn = canonical  momenta and position, respectively
  p01 .. p0n, q01 ... q0n  = initial values for p1 ... pn & q1 ... qn, respectively
  dt = time step--can be negative
  N = number of time steps--must be a positive fixnum
  method = integration method (default symplectic_euler); must be one of symplectic_euler, 
  verlet, symplectic_third_order, symplectic_fourth_order
  ntype = optional number type (default float)--can be float, rational, or any (no type)"


  (let* ((update-p) (update-q) (cfs) (ddt (gensym)) (args) (poo) (qoo) 
         (xmlist (get 'mlist 'msimpind)) (scalar-case) (N (mcoerce NN 'fixnum)))
        
        (declare (type fixnum N))
        (cond 
          ((every #'$listp (list p q po qo)) ;Maxima to CL list conversion
           (setq scalar-case nil)
           (setq po (list (mapcar #'(lambda (x) (mcoerce x ntype)) (rest po))))
           (setq qo (list (mapcar #'(lambda (x) (mcoerce x ntype)) (rest qo))))
           (setq p (rest p))
           (setq q (rest q)))

          ((every #'$mapatom (list p q po qo))
           (setq scalar-case t)
           (setq po (list (list (mcoerce po ntype))))
           (setq qo (list (list (mcoerce qo ntype))))
           (setq p (list p))
           (setq q (list q)))

          (t (merror (intl:gettext "Either the second through fifth arguments must all be lists or all must be mapatoms"))))

        

        (setq args (append p q (list ddt))) 
        (setq update-p (mapcar #'(lambda (pk qk) 
                                         (expr-to-compiled-fun (sub pk (mult ddt ($diff ham qk))) ntype args)) p q))

        (setq update-q (mapcar #'(lambda (pk qk) 
                                        (expr-to-compiled-fun (add qk (mult ddt ($diff ham pk))) ntype args)) p q))
    
        (setq cfs (gethash sym-method *symplectic-method-coefficients* ))

        (when (not cfs)
          (let ((all-methods nil)) ;build a list of all methods
               (maphash #'(lambda (a b) (declare (ignore b)) (push a all-methods)) *symplectic-method-coefficients*)
               (push xmlist all-methods)
               (merror (intl:gettext "The method must be a member of ~M, but found ~M") all-methods sym-method)))

        (setq cfs (mapcar #'(lambda (x) (cons (mcoerce (mul dt (car x)) ntype) 
                                              (mcoerce (mul dt (cdr x)) ntype))) cfs))

        (while (> N 0)
               (decf N 1)   
               (setq poo (first po))
               (setq qoo (first qo))   
               (dolist (cx cfs)
                 (setq args (append poo qoo (list (car cx))))
                 (setq poo (mapcar #'(lambda (fk) (apply fk args)) update-p))
                 (setq args (append poo qoo (list (cdr cx))))
                 (setq qoo (mapcar #'(lambda (fk) (apply fk args)) update-q)))
               (push poo po)
               (push qoo qo)) 

        (cond 
          (scalar-case
            (setq po (reduce #'append (reverse po)))
            (setq qo (reduce #'append (reverse qo))))
          (t
           (setq po (mapcar #'(lambda (x) (push xmlist x)) (reverse po)))
           (setq qo (mapcar #'(lambda (x) (push xmlist x)) (reverse qo)))))  

        (mapcar #'fmakunbound update-p) ;The functions update-p and update-q are no longer needed.
        (mapcar #'fmakunbound update-q)
        (push xmlist po) ;Convert po & qo from CL lists to Maxima lists.
        (push xmlist qo) ;Arguably circumventing simplification by pushing (mlist simp) is a bug.
        (list xmlist po qo)))