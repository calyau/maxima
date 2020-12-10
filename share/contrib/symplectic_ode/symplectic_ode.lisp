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
  equal length Maxima lists or both must be ."
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

(defvar *symplectic-method-coefficients* (make-hash-table :size 5 :test 'eql))
(defvar *symplectic-method-digits* (make-hash-table :size 5 :test 'eql))

(setf (gethash '$symplectic_euler *symplectic-method-coefficients*) 
      (list (cons 1 1)))
(setf (gethash '$symplectic_euler *symplectic-method-digits*) '$inf)

(setf (gethash '$verlet *symplectic-method-coefficients*) 
      (list (cons 0 (div 1 2)) 
            (cons 1 (div 1 2))))
(setf (gethash '$verlet *symplectic-method-digits*) '$inf)

(setf (gethash '$symplectic_third_order *symplectic-method-coefficients*)
      (list (cons 1 (div -1 24)) 
            (cons (div -2 3) (div 3 4)) 
            (cons (div  2 3) (div 7 24))))
(setf (gethash '$symplectic_third_order *symplectic-method-digits*) '$inf)

(setf (gethash '$symplectic_fourth_order *symplectic-method-coefficients*)
      (let ((k (div 1 (sub 2 (power 2 (div 1 3))))))            
           (list 
            (cons (div k 2) k)
            (cons (div (mul (sub 1 (power 2 (div 1 3))) k) 2) (mul (mul -1 (power 2 (div 1 3))) k))
            (cons (div (mul (sub 1 (power 2 (div 1 3))) k) 2) k)
            (cons (div k 2) 0))))
(setf (gethash '$symplectic_fourth_order *symplectic-method-digits*) '$inf)

;; see Kostas Tselios and T. E. Simos, ``Optimized fifth order symplectic integrators 
;;; for orbital problems,'' Revista Mexicana de Astronom´ıa y Astrof´ısica, 49, 11–24 
;;; (2013)

;;; cut and paste coefficients from article--the article says something about using 40 
;;; digits, but the coeffficeints are given to 57 digits. Let's be cautious and say these
;;; are good to 40 digits.

#|
c1 = 0.112569584468347104973189684884327785393840239333314075493 ,
c2 = 0.923805029000837468447500070054064432491178527428114178991 ,
c3 = −1.362064898669775624786044007840908597402026042205084284026 ,
c4 = 0.980926531879316517259793318227431991923428491844523669724 ,
c5 = 0.400962967485371350147918025877657753577504227492190779513 ,
c6 = 0.345821780864741783378055242038676806930765132085822482512 ,
c7 = −0.402020995028838599420412333241250172914690575978880873429 ,
d1 = 0.36953388878114957185081450061701658106775743968995046842 ,
d2 = −0.032120004263046859169923904393901683486678946201463277409 ,
d3 = −0.011978701020553903586622444048386301410473649207894475166 ,
d4 = 0.51263817465269673604202785657395553607442158325539698102 ,
d5 = −0.334948298035883491345320878224434762455516821029015086331 ,
d6 = 0.021856594741098449005512783774683495267598355789295971623 ,
d7 = 0.47501834514453949720351208570106713494289203770372938037
|#

(setf (gethash '$symplectic_fifth_order *symplectic-method-coefficients*)
      (mapcar #'(lambda (a b) (cons a b))
              (list
               (div 5652885872144153264930671803559342469576954521714583915513
                    50216813883093446110686315385661331328818843555712276103168)
               (div 362426134418756238993387367402013039958979964654753376945
                    392318858461667547739736838950479151006397215279002157056)
               (div -4274909969574666030908365902533105271208568565239518523207
                    3138550867693340381917894711603833208051177722232017256448)
               (div 3078687817773247970859746019343925292770783479419914796411
                    3138550867693340381917894711603833208051177722232017256448)
               (div 629221334757054438917474234802547577988423100782937501865
                    1569275433846670190958947355801916604025588861116008628224)
               (div 4341517001601166158290432120166041572705763778637534302909
                    12554203470773361527671578846415332832204710888928069025792)
               (div -5047053371114805865785231327168622346738894852075304064687
                    12554203470773361527671578846415332832204710888928069025792)) 
              (list
               (div 2319601814552342659136359435206712577059674598536966859091
                    6277101735386680763835789423207666416102355444464034512896)
               (div -3225928552003184381834780354143261178475501353749927450079
                    100433627766186892221372630771322662657637687111424552206336)
               (div -4812257597683015175999923204116192762946117963505537572213
                    401734511064747568885490523085290650630550748445698208825344)
               (div 402235246967237878148042461203282093231351221007491081787
                    784637716923335095479473677900958302012794430558004314112)
               (div -4205009085731718839023168486933272699269839833464512439795
                    12554203470773361527671578846415332832204710888928069025792)
               (div 274392137557984949615301132056520558869905595208602668291
                    12554203470773361527671578846415332832204710888928069025792)
               (div 5963476957294596320417393096022684997935176483307435518905
                    12554203470773361527671578846415332832204710888928069025792)))) 

(setf (gethash '$symplectic_fifth_order *symplectic-method-digits*) 40)

;;; Usage: symplectic_ode(ham, [p1,..., pn], [q1, ... , qn], 
;;;                    [p01, ... p0n], [q01, ... q0n], dt, N, [method], [ntype])

;;;   ham = time independent hamiltonian (function of p1 ... pn and q1 ... qn only)
;;;   p1 ... pn; q1 ... qn = canonical  momenta and position, respectively
;;;   p01 .. p0n; q01 ... q0n  = initial values for p1 ... pn & q1 ... qn, respectively
;;;   dt = time step--can be negative
;;;   N = number of time steps--must be a positive fixnum
;;;   method = integration method (default symplectic_euler); must be one of symplectic_euler, 
;;;            verlet, symplectic_third_order, symplectic_fourth_order
;;;   ntype = optional number type (default float)--can be float, rational, or any (no type)

;;; The function symplectic returns a two member list of two lists. The first list has the form
;;; [[p00,p10,p20, ...],[p01,p11, p21, ..] ...] where pij = i-th momentum at time step j.

;;; For a separable hamiltonian of the form ham = F(p) + G(q), symplectic_ode approximately 
;;; integrates the hamiltonian equation using a method that preserves the poisson brackets of p & q. 
;;; For a description, see https://en.wikipedia.org/wiki/Symplectic_integrator#A_first-order_example. 
;;; For non separable hamiltonians, the method does not in general preserve the Poisson brackets.

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

          ((every #'(lambda (s) (not ($listp s))) (list p q po qo))
           (setq scalar-case t)
           (setq po (list (list (mcoerce po ntype))))
           (setq qo (list (list (mcoerce qo ntype))))
           (setq p (list p))
           (setq q (list q)))

          (t (merror (intl:gettext "Either the second through fifth arguments must all be lists or all must be nonlists."))))

        
        (when (or (some #'(lambda (s) (not ($symbolp s))) p)
                  (some #'(lambda (s) (not ($symbolp s))) q))
              (merror "The second and third arguments must be either be symbols or lists of symbols."))    

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
        
        (setq poo (car po))
        (setq qoo (car qo))
        (while (> N 0)
               (decf N 1)   
               (dolist (cx cfs)
                 (setq args (append poo qoo (list (car cx))))
                 (setq poo (mapcar #'(lambda (fk) (apply fk args)) update-p))
                 (setq args (append poo qoo (list (cdr cx))))
                 (setq qoo (mapcar #'(lambda (fk) (apply fk args)) update-q)))
               (push poo po)
               (push qoo qo)) 

        (cond 
          (scalar-case
            (setq po (mapcar #'car (reverse po)))
            (setq qo (mapcar #'car (reverse qo))))
          (t
           (setq po (mapcar #'(lambda (x) (push xmlist x)) (reverse po)))
           (setq qo (mapcar #'(lambda (x) (push xmlist x)) (reverse qo)))))  

        (mapcar #'fmakunbound update-p) ;The functions update-p and update-q are no longer needed.
        (mapcar #'fmakunbound update-q)
        (push xmlist po) ;Convert po & qo from CL lists to Maxima lists.
        (push xmlist qo) ;Arguably circumventing simplification by pushing (mlist simp) is a bug.
        (list xmlist po qo)))