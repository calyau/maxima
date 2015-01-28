; file lll.lisp
; Maxima Interface

(defun maxima::$latticereduce (L) ; L is maxima list of maxima lists
  (if (or
       (not (listp L))
       (< (length L) 2)
       (not (equal (apply 'max (mapcar 'length (cdr L)))
                   (apply 'min (mapcar 'length (cdr L))) ))  )
      (maxima::merror "Latticereduce needs a list lists as input")) 
  (let* ((vs (mapcar 'cdr (cdr L)))
         (n (length vs))
         (v (make-array (list n))))
    (loop for i from 0 to (- n 1) do
          (setf (aref v i) (make-array (list n) :initial-contents (nth i vs))))
    (setf v (lll v))
    (cons '(maxima::mlist)
          (mapcar #'(lambda (u) (cons '(maxima::mlist) (coerce u 'list)))
                  (coerce v 'list)))
))


(defun maxima::$integerrelations (L)
  (let ((res (integerrelations (cdr L))))
    (cons '(maxima::mlist) (car res))
    ))

(defun maxima::$floatrelations (L)
  (let ((res (floatrelations (cdr L))))
    (cons '(maxima::mlist) (car res))
    ))

(defun maxima::$recognize (x)
  (convert2AlgNum x)) 

; linear algebra helpers

(defun norm2 (a)
  (declare (type (vector number *) a))
  (loop for i from 0 to (- (length a) 1)  sum
        (* (aref a i) (aref a i) )))


(defun vadd (a b)
  (declare (type (vector number *) a))
  (declare (type (vector number *) b))
  (if (not (= (length a) (length b)))
      (error "vadd: length must coincide")
    (let ((res (make-array (list (length a)) ))
        (n (length a)))
      (loop for i from 0 to (- n 1) do
            (setf (aref res i) (+ (aref a i) (aref b i))))
      res)))

(defun vsub (a b)
  (declare (type (vector number *) a))
  (declare (type (vector number *) b))
  (if (not (= (length a) (length b)))
      (error "vadd: length must coincide")
    (let ((res (make-array (list (length a)) ))
        (n (length a)))
      (loop for i from 0 to (- n 1) do
            (setf (aref res i) (- (aref a i) (aref b i))))
      res)))

(defun smul (s a)
  (declare (type (vector number *) a))
  (declare (type (vector number *) b))
  (let ((res (make-array (list (length a)) ))
        (n (length a)))
      (loop for i from 0 to (- n 1) do
            (setf (aref res i) (* s (aref a i))))
      res))
 

(defun dot (a b)
  (declare (type (vector number *) a))
  (declare (type (vector number *) b))
  (if (not (= (length a) (length b)))
      (error "vadd: length must coincide")
    (loop for i from 0 to (- (length a) 1) sum
          (* (aref a i) (aref b i)))))

(defvar *gsbasis* nil)


(defun gso (f mu) ; returns Gram-Schmidt OGS, updates mu
  (declare (type vector a))
  (declare (type (array number (* *) mu)))
  (let* ((n (length f))
         (g (make-array (list n) )))
    (loop for j from 0 to (- n 1) do (setf (aref g j) (aref f j)))
    (loop for j from 0 to (- n 1) do
          (loop for k from 0 to (- j 1) do
                (if (> (norm2 (aref g k)) 0)
                    (progn
                      (setf (aref mu j k)
                            (/ (dot (aref f j) (aref g k))
                               (dot (aref g k) (aref g k))))
                      (setf (aref g j) (vsub (aref g j) (smul (aref mu j k) (aref g k))))
                      ))      
                ))
    g
    ))

(defun lll (f) ; f is vetor of n vectors
  (declare (type (vector vector *) f))
  (let* ((n (length f))
        (g (make-array (list n) ))
        (gg nil)
        (i 1)
        (temp nil)
        (mu (make-array (list n n) :initial-element 0)))
    (loop for j from 0 to (- n 1) do
          (setf (aref g j) (aref f j))
          (setf (aref mu j j) 1))
    (setf gg (gso g mu))
    (do () ((>= i n) )
      (loop for j from (- i 1) downto 0 do
            (setf (aref g i)
                  (vsub (aref g i) (smul (nint (aref mu i j)) (aref g j))))
            (setf gg (gso g mu))
            )
      (if (and (> i 0) (> (norm2 (aref gg (- i 1))) (* 2 (norm2 (aref gg i)))))
          (progn
            (setf temp (aref g (- i 1)))
            (setf (aref g (- i 1)) (aref g i))
            (setf (aref g i) temp)
            (setf gg (gso g mu))
            (setf i (- i 1))
            )
        (setf i (+ i 1)))
      )
    g
    )
  )   

(defun nint (q)
       (car (list (floor (+ q (/ 1 2))))))

(defun test1 ()
  (lll #( #(12 2) #(13 4))))

(defun test2 ()
  (lll #( #(12 2) #(1 2))))

(defun test3 ()
  (lll #( #(1 2) #(12 2) )))

(defun test4 ()
  (lll #( #(1 2) #(9 -4))))

(defun integerRelations (L) ; L ist list of integers
; returns (rel def) such that dotproduct def of rel and L is small
  (let* ((n (length L))
        (z nil) (g nil) (res nil) (optdef 0) (optres nil) (def 0)
        (f (make-array (list (+ n 1)))))
    (setf (aref f 0) (make-array (list (+ n 1)) :initial-element 0))
    (loop for i from 1 to n do
          (setf z  (make-array (list (+ n 1)) :initial-element 0))
          (setf (aref z 0) (nth (- i 1) L))
          (setf (aref z i) 1)
          (setf (aref f i) z))
    (setf g (lll f))
    (setf optdef most-POSITIVE-fixnum)
    (loop for k from 1 to n do
          (setf def 0)
          (setf res nil)
          (loop for i from n downto 1 do
          (setf res (cons (aref (aref g k) i) res))
          (setf def (+ def (* (nth (- i 1) L) (aref (aref g k) i))))
          )
          (if (< (abs def) (abs optdef))
              (progn
                (setf optdef def)
                (setf optres res)
                ))
          )
    (list optres optdef)
    ))

(defun test5 ()
  (print "Should give ((3 -7 4) 0)")
  (integerRelations '(5707963267 4142135623 2967764890)))


(defun nkomma (a)
  (- a (car (list (floor a)))))


(defun FloatRelations (x &optional (digits 10)) ; x is a list of floats or doubles
  (let ((n (length x)) (digs nil) (i 0) (nkomma 0)
        (nkommaStellen0) (xx 0) (expo nil) (rel nil) (def 0))
    (setf nkommaStellen
          (lambda (x) ; x ist float
            (let ((s 0) (i 0))
              (do () ((< (abs (nkomma (* x (expt 10d0 s)))) (expt 1d-1 digits)))
                (setf s (+ s 1)))
              s)))
  (setf digs (mapcar nkommaStellen x))
  (setf expo (apply 'max digs)) 
  (setf xx (mapcar #'(lambda (u) (car (list (floor (* u (expt 10d0 expo))))))  x))
  (setf rel (IntegerRelations xx))
  (list (car rel) (/ (cadr rel) (expt 10.0d0 expo)))
  ))


(defun test6 ()
  (FloatRelations (list .5707963267d0 .4142135623d0 .2967764890d0)))


(defun mkdif (a b) (list '(MAXIMA::MPLUS) a (list '(MAXIMA::MMINUS) b)) )
(defun mksum (a b) (list '(MAXIMA::MPLUS) a b) )
(defun mkprod (a b) (list '(MAXIMA::MTIMES) a b) )
(defun mkexpt (a b) (list '(MAXIMA::MEXPT) a b))
(defun mkeq (a b) (list '(MAXIMA::MEQUAL) a b))

(defun max2str (expr)
  (maxima::mfuncall 'maxima::$string expr)
  )

(defun convert2AlgNum (x &optional (digits 10)) ; x is float or double
  (let ((n 0) (nn 6) ;; nn: maximal search degree 
        (xs nil) (res nil) (def 0) (sol nil)
        (opt nil) (mini 0)
        (var (gentemp "$v")))
    (loop for n from 2 to nn do
          (if (not (null opt)) (return opt))
          (setf xs (loop for i from 0 to n collect (expt x i)))
          (setf res  (FloatRelations xs digits))
          (setf def (cadr res))
          (if (< (abs def) (expt 10d0 (- digits)))
              (progn
                (setf pol 0)
                (loop for i from 0 to n do
                      (setf pol (mksum pol (mkprod (nth i (car res)) (mkexpt var i)))))
                (setf sol (maxima::meval (maxima::$solve (mkeq pol 0) var)))
                (if (not (null (cdr sol))) ; a solution has been found
                    (progn (setq *sol* sol)
                      (setf sol (mapcar 'third (cdr sol)))
                      (setf sol
                            (mapcar #'(lambda (u)
                                        (let ((r (maxima::$realpart u))
                                              (i (maxima::$float (maxima::$imagpart u))))
                                          (if (or (equal i 0) (equal i 0.0) (equal i 0.0d0))
                                              (list u (abs (- x (maxima::$float u))))
                                              (list u most-POSITIVE-DOUBLE-FLOAT)
                                              )))
                            sol))
                      (setf mini most-POSITIVE-DOUBLE-FLOAT)
                      (loop for p in sol do (if (< (second p) mini) (progn (setf opt (car p)) (setq mini (second p)))))
                ))))
          ); loop
    opt
    ) ; let
  ) ;defun

(defun test7 () ; fails
       (convert2AlgNum 1.414213562373d0))


(defun test8 () ; works             
       (convert2AlgNum (+ 2 (* 3 1.414213562373d0))))


(defun test9 () :fails
       (convert2AlgNum (+ 1d0 (sqrt 2d0) (sqrt 3d0)) ))
