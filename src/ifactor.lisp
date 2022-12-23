;;;;
;;;;                      ~*~  IFACTOR  ~*~
;;;;
;;;;  Maxima integer factorization package.
;;;;
;;;;  Copyright: 2005-2015 Andrej Vodopivec, Volker van Nek
;;;;  Licence  : GPL
;;;;
;;;;   - ifactors     : factorization of integers
;;;;   - primep       : probabilistic primality test
;;;;   - next_prime   : smallest prime > n
;;;;   - prev_prime   : greatest prime < n
;;;;   - primes       : list of primes
;;;;   - power_mod    : fast modular powers
;;;;   - inv_mod      : modular inverse


(in-package :maxima)
(macsyma-module ifactor)

(defmvar $save_primes nil "Save primes found." boolean)

(defmvar $primep_number_of_tests 25 "Number of Miller-Rabin tests." fixnum)

(defmvar $pollard_rho_limit 16000 "Limit for pollard-rho factorization depth." fixnum)
(defmvar $pollard_pm1_limit 25000 "Limit for pollard-p1 factorization depth." fixnum)

(defmvar $pollard_rho_tests 5 "Number of pollard-rho rounds." fixnum)
(defmvar $pollard_pm1_tests 3 "Number of pollard-p-1 rounds." fixnum)

(defmvar $pollard_rho_limit_step 1000 "Step for pollard-rho factorization limit." fixnum)
(defmvar $pollard_pm1_limit_step 5000 "Step for pollard-p-1 factorization limit." fixnum)

(defmvar $ecm_number_of_curves 50 "Number of curves tried in one round of ecm." fixnum)
(defmvar $ecm_limit       200  "Starting smootheness limit for ecm method." fixnum)
(defmvar $ecm_max_limit 51199  "Maximum smootheness for ecm method." fixnum)
(defmvar $ecm_limit_delta 200  "Increase smoothness limit for ecm method after each round." fixnum)

(defmvar $ifactor_verbose nil "Display factorization steps." boolean)
(defmvar $factors_only nil  "Return a list of factors only." boolean)

(defun number-of-digits (n)
  (length (format nil "~d" n)))

;;; List of primes up to *largest-small-prime*

(defvar *largest-small-prime*
  (car (last *small-primes*)))

;;; List of numbers which have already been tested and are
;;; primes > *largest-small-prime* (only used if $save_primes is true!).

(defvar *large-primes* ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;;
;;;        ~*~  IMPLEMENTATION OF FACTORIZATION METHODS   ~*~             ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-factor-list (n)
  (when $ifactor_verbose
    (format t "~%Starting factorization of n = ~d~%" n))
  (init-prime-diffs 640000)
  (multiple-value-bind (large-part factor-list) (get-small-factors n)
    (if (> large-part 1)
        (append (convert-list (get-large-factors large-part)) factor-list)
        factor-list)))

(defmfun $ifactors (n)
  (unless (and (or (integerp n)
                   (and ($integerp n)
                        (setq n ($fix n)) ))
               (plusp n) )
    (merror (intl:gettext "ifactors: argument must be a positive integer; found: ~M") n))
  (let* (($intfaclim)
     (factor-list (get-factor-list n))
     (factor-list (if $factors_only
              (mapcar #'car factor-list)
              (mapcar #'(lambda (u) `((mlist simp) ,(car u) ,(cadr u))) factor-list))))
    ($sort `((mlist simp) ,@factor-list))))

;; cfactor is the function used by maxima to factor integers
;; (used form outside ifactor package)

(defun cfactor (x)
  (cond ((null $factorflag) (return-from cfactor (list x 1)))
    ((floatp x) (rat-error "`factor' given floating arg"))
    ((pzerop x) (return-from cfactor (list (pzero) 1)))
    ((equal x -1) (return-from cfactor (list -1 1)))
    ((minusp x) (return-from cfactor (cons -1 (cons 1 (cfactor (- x))))))
    ((< x 2) (return-from cfactor (list x 1)))
    (t (let* ((factor-list (get-factor-list x))
          (factor-list (sort factor-list #'< :key #'car))
          (ans ()))
         (dolist (fac factor-list ans)
           (setq ans (cons (car fac) (cons (cadr fac) ans))))))))

;;; we need to keep a list of differences between consecutive primes
;;; for trial division, for stage 2 of ecm and the big prime variation of Pollard p-1

(defvar *prime-diffs* (make-array 100000 :element-type 'fixnum :adjustable t :initial-element 0)
  "array of differences between consecutive primes")

(defvar *prime-diffs-limit* 1
  "biggest prime in *prime-diffs")

(defvar *prime-diffs-maxindex* 1
  "index of biggest valid prime difference")

(defvar *prime-diffs-maxdiff* 2
  "maximum difference between consecutive primes in *prime-diffs*")

;;; factor out primes < *prime-diffs-limit* by trial division
;;; the array gets filled by a call to init-prime-diffs in get-factor-list

(defun get-small-factors (n)
  (when (= n 1)
    (return-from get-small-factors (values 1 nil)))
  (when (< n 4)			;n = 2 or 3
    (return-from get-small-factors (values 1 `((,n 1)))))
  (let (factors)
    ;; first divide off the even part
    (loop with deg = 0
       while (and (> n 1) (evenp n)) do
     (setq n (ash n -1))		; divide n by 2
     (incf deg)			; and increment the exponent
       finally
     (when (plusp deg)
       (push `(2 ,deg) factors)
       (when $ifactor_verbose (format t "Factoring out 2: 2 (degree:~A)~%" deg))))
    (when (= n 1)
      (return-from get-small-factors (values 1 factors))) ; n was a power of 2
    ;; now use the *prime-diffs* array for trial-factoring
    (loop for i from 0 to *prime-diffs-maxindex*
       and d = 3 then (+ d (aref *prime-diffs* i))
       do
     (when (> (* d d) n)
       ;;(push `(,n 1) factors) replaced by workaround next line,
      (push (list n 1) factors) ;; see bug report 3510983 (van_nek, 2012-03-27)
       (when $ifactor_verbose  (format t "small prime cofactor: ~A~%" n))
       (return-from get-small-factors (values 1 factors)))
     (loop with deg = 0
        while (and (> n 1) (zerop (mod n d))) do
          (setq n (truncate n d))
          (incf deg)
        finally
          (when (plusp deg)
        (push `(,d ,deg) factors)
        (when $ifactor_verbose (format t "Factoring out small prime: ~A (degree:~A)~%" d deg))))
     (when (= n 1)
       (return-from get-small-factors (values 1 factors))))
    (return-from get-small-factors (values n factors))))

;;; get-large-factors returns the list of factors of integer n (n has
;;; no small factor at this tage)

(defun get-large-factors (n)
  (if (primep n)
      (progn
    (when $ifactor_verbose (format t "========> Prime factor: ~d~%~%" n))
    (list n))
      (get-large-factors-1 n)))

(defun get-large-factors-1 (n)
  (let ((f (get-one-factor n)))
    (if (= f n)
    (progn
      (when $ifactor_verbose (format t "Warning: could not find factors of composite:~%~A~%" n))
      (list n))
    (append (get-large-factors f) (get-large-factors (/ n f))))))

(defun get-one-factor (n)
  (when $ifactor_verbose
    (format t "Factoring n = ~d~%" n))
  (let ((f nil)
    (lim_pollard $pollard_rho_limit)
    (lim_p-1 $pollard_pm1_limit)
    ($ecm_number_of_curves $ecm_number_of_curves))

    ;; If $intfaclim is not false then we don't want to spend too much
    ;; time factoring integers so we return n and leave it
    ;; unfactored. The default value for $intfaclim is true, but most
    ;; functions which use integer factorization set it to false.
    (when $intfaclim
      (return-from get-one-factor n))

    ;; first try known large primes
    (dolist (p *large-primes*)
      (when (zerop (mod n p))
        (return-from get-one-factor p)))

    ;; try factoring smaller factors with pollard-rho
    (dotimes (i $pollard_rho_tests)
      (when $ifactor_verbose
    (format t "Pollard rho: round #~d of ~d (lim=~d)~%" (1+ i) $pollard_rho_tests lim_pollard))
      (setq f (get-one-factor-pollard n lim_pollard))
      (when (< 1 f n)
    (when $ifactor_verbose
      (format t "Pollard rho: found factor ~A (~d digits)~%" f (number-of-digits f)))
    (return-from get-one-factor f))
      (if (> lim_pollard 0)
      (incf lim_pollard $pollard_rho_limit_step)))

    ;; now try factoring with pollards p-1 method
    (dotimes (i $pollard_pm1_tests)
      (when $ifactor_verbose
    (format t "Pollard p-1: round #~d of ~d (lim=~d)~%" (1+ i) $pollard_pm1_tests lim_p-1))
      (setq f (get-one-factor-p-1 n lim_p-1))
      (when (< 1 f n)
    (when $ifactor_verbose
      (format t "Pollard p-1: found factor ~A (~d digits)~%" f (number-of-digits f)))
    (return-from get-one-factor f))
      (when (plusp lim_pollard)
    (incf lim_p-1 $pollard_pm1_limit_step)))

    ;; continue with ecm
    (do ()
    (nil)
      (setq f (get-one-factor-ecm n))
      (unless (null f)
    (return-from get-one-factor f))
      (setq $ecm_number_of_curves (+ $ecm_number_of_curves 50)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;;
;;;   ~*~  IMPLEMENTATION OF POLLARDS-RHO FACTORIZATION METHOD   ~*~      ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is the Brent's variant of Pollard's rho method.

(defun get-one-factor-pollard (n lim)
  (let* ((x (+ (random (- n 3)) 2))
     (a (+ (random (- n 2)) 1))
     (b (+ (random (- n 5)) 1))
     (y x) (d 1) (r 2) (j 1) (k)
          (terms (integer-length n)) )
    (setq b (/ b (gcd a b)))
    (loop while (= d 1) do
     (setq y x)
     (incf j r)
     (dotimes (i r)
       (setq x (mod (+ (* a (mod (* x x) n)) b) n)))
     (setq k 0)
     (loop while (and (< k r) (equal d 1)) do
          (dotimes (i (min terms (- r k)))
        (setq x (mod (+ (* a (mod (* x x) n)) b) n))
        (setq d (mod (* d (- x y)) n)))
          (setq d (gcd d n))
          (incf k terms))
     (setq r (* 2 r))
     (when (< 0 lim j)
         (return-from get-one-factor-pollard d)))
    d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;;
;;;   ~*~  IMPLEMENTATION OF POLLARDS P-1 FACTORIZATION METHOD   ~*~      ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; product of all primes low < p < high and all integers
;;; isqrt(low) < n < isqrt(high)

(defun ppexpo (low high)
  (declare (integer low high))
  (let ((x 1))
    (loop for i from (max 2 (1+ (isqrt low))) to (isqrt high) do
     (setq x (* i x)))
    (when (oddp low) (incf low))
    (loop for i from (1+ low) to high
       when (primep i) do (setq x (* i x)))
    x))

(defun big-prime-var (y n bound)
  (let ((x (make-array (1+ (ash *prime-diffs-maxdiff* -1)) :element-type 'integer :initial-element 1))
    (y2 (mod (* y y) n))
    (q 3)
    (count 1)
    (d 0)
    (z 0)
    (k 0))
    (loop for i from 1 to (ash *prime-diffs-maxdiff* -1) do
     (setf (aref x i) (mod (* y2 (aref x (1- i))) n)))
    (setq y (power-mod y q n))
    (setq z (1- y))
    (setq bound (min bound *prime-diffs-limit*))
    (loop for i from 0
     while (< q bound) do
     (setq k (aref *prime-diffs* i))
     (incf q k)
     (setq y (mod (* y (aref x (ash k -1))) n))
     (setq z (mod (* z (1- y)) n))
     (when (> (incf count) 1000)
       (setq count 0)
       (setq d (gcd z n))
       (when (> d 1)
         (return-from big-prime-var d))))
    d))

;;; Pollard's p-1 factoring algorithm
;;; in general a prime factor p of x is found, if p-1 is
;;; a product of prime powers q^k <= lim

(defun get-one-factor-p-1 (n &optional (lim 16000))
  (declare (integer n lim))
  (let* ((base (+ 2 (random (- n 2))))
     (anz 256)
     (d (gcd base n)))
    (declare (fixnum anz)
         (integer base d))
    (when (< 1 d n) (return-from get-one-factor-p-1 d))
    (loop for n0 from 0 to (1- lim) by anz
       and ex = (ppexpo n0 (min lim (+ n0 anz))) do
       (setq base (power-mod base ex n))
       (when (<= base 1) (return-from get-one-factor-p-1 1))
       (setq d (gcd (1- base) n))
       (when (> d 1)
     (return-from get-one-factor-p-1 d)))
    (when (= d 1)
      (return-from get-one-factor-p-1 (big-prime-var base n *prime-diffs-limit*))))
  1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;;
;;;  ~*~  IMPLEMENTATION OF ELLIPTIC CURVE FACTORIZATION METHOD   ~*~     ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Elliptic curve factorization method as described in the paper
;;; R. Brent: "Factorization of the tenth and eleventh Fermat Number".
;;; The paper is in file rpb161tr.dvi.gz from
;;; ftp://ftp.comlab.ox.ac.uk/pub/Documents/techpapers/Richard.Brent/
;;;
;;; Based in part on the implementation from GAP4 FacInt package.

(defun init-prime-diffs (n)
  (when (> n *prime-diffs-limit*)
    (setq n (* 2 n))
    (when $ifactor_verbose (format t "Initializing prime diffs up to n=~d~%" n))
    (let ((sieve (make-array (1+ n) :element-type 'bit :initial-element 1)))
      (do ((p 3 ($next_prime p)))
      ((> p (isqrt n)))
    (do ((d (* 2 p) (+ d p)))
        ((> d n))
      (setf (sbit sieve d) 0)))
      (do ((q1 3)
       (i 0)
       (q2 5 (+ q2 2)))
      ((> q2 n) (setq *prime-diffs-maxindex* (1- i)))
    (when (= 1 (sbit sieve q2))
      (when (>= i (length *prime-diffs*))
        (setq *prime-diffs* (adjust-array *prime-diffs* (* 2 (length *prime-diffs*)) :element-type 'fixnum :initial-element 0))
        (when $ifactor_verbose
          (format t "init-prime-diffs: adjusting *prime-diffs* to size ~d~%" (* 2 (length *prime-diffs*)))))
      (setq *prime-diffs-limit* q2)

      (let ((diff (- q2 q1)))
        (setf (aref *prime-diffs* i) diff)
        (when (> diff *prime-diffs-maxdiff*) (setq *prime-diffs-maxdiff* diff)))
      (setq q1 q2)
      (incf i))))))

;;; modular inverse of a (modulus m)
;;;
;;; this implementation of the modular inverse is different to `invmod' in src/rat3a.lisp
;;; inv-mod returns a positive modulo or `nil' in case of a zero divisor

(defun inv-mod (a m)
  (let ((u1 0)(u2 m)(v1 1)(v2 (mod a m)) q r)
    (do ()((zerop v2)
            (if (= 1 u2) (mod u1 m) nil) )
      (multiple-value-setq (q r) (truncate u2 v2))
      (psetq u1 v1 v1 (- u1 (* q v1)))
      (setq u2 v2 v2 r) )))

(defmfun $inv_mod (a m)
  (unless (and (integerp a) (integerp m))
      (merror (intl:gettext "inv_mod: arguments must be integers; found: ~M, ~M") a m))
  (unless (= 0 a) (inv-mod a m)) )

;;; computations on the elliptic curve:
;;; we use the elliptic curve in projective coordinates (x,y,z), but only
;;; use x and z

(defun ecm-product (q p1 p2 n)
  (let ((x1 (car p1)) (x2 (car p2))
    (z1 (cadr p1)) (z2 (cadr p2))
    (pr1) (pr2) (sq1) (sq2) (x3) (z3))
    (setq pr1 (mod (* (- x1 z1) (+ x2 z2)) n))
    (setq pr2 (mod (* (+ x1 z1) (- x2 z2)) n))
    (setq sq1 (mod (expt (+ pr1 pr2) 2) n))
    (setq sq2 (mod (expt (- pr1 pr2) 2) n))
    (setq x3 (mod (* (cadr q) sq1) n))
    (setq z3 (mod (* (car q) sq2) n))
    `(,x3 ,z3)))

(defun ecm-square (p n a)
  (let ((x1 (car p)) (z1 (cadr p))
    (x2) (z2) (sq1) (sq2) (f1) (f2))
    (setq sq1 (mod (* (+ x1 z1) (+ x1 z1)) n))
    (setq sq2 (mod (* (- x1 z1) (- x1 z1)) n))
    (setq f1 (- sq1 sq2))
    (setq f2 (mod (* a f1) n))
    (setq x2 (mod (* sq1 sq2) n))
    (setq z2 (mod (* f1 (+ sq2 f2)) n))
    `(,x2 ,z2)))

(defun ecm-power (base e n a)
  (let ((p base) (ptb (ecm-square base n a)) (l (integer-length e)))
    (do ((i (- l 2) (1- i)))
    ((< i 0))
      (if (logbitp i e)
      (progn
        (setq p (ecm-product base p ptb n))
        (setq ptb (ecm-square ptb n a)))
      (progn
        (setq ptb (ecm-product base p ptb n))
        (setq p (ecm-square p n a)))))
    p))

(defun ecm-factor-with-curve (n x z a lim1)
  (let ((g (gcd (- (* a a) 4) n)))
    (unless (= g 1) (return-from ecm-factor-with-curve g)))
  (setq a (mod (floor (+ a 2) 4) n))
  ;;
  ;; stage 1: compute p^M where M=p1^e1*...*pk^ek where
  ;;          p1,...,pk are primes < lim1 and ei=log[pi](n)
  ;;
  (let ((q 1)
    (last_q ($prev_prime lim1))
    (p `(,x ,z))
    (ex) (next_gcd)	(gcd_interval))
    (setq gcd_interval (floor lim1 4))
    (setq next_gcd gcd_interval)
    (do ()
    ((> q lim1))
      (setq q ($next_prime q))
      (setq ex (floor (log lim1) (log q)))
      (cond ((= q 2) (incf ex 2))
        ((= q 3) (incf ex)))
      (setq p (ecm-power p (expt q ex) n a))
      (when (>= q next_gcd)
    (let ((g (gcd (cadr p) n)))
      (when (< 1 g n)
        (when $ifactor_verbose
          (format t "ECM: found factor in stage 1: ~d (~d digits)~%" g (number-of-digits g)))
        (return-from ecm-factor-with-curve g))
      (setq next_gcd (min (+ next_gcd gcd_interval) last_q)))))
    ;;
    ;; stage 2: compute (p^M)^pi for each prime lim1<pi<lim2 (and some
    ;;          other exponents)
    ;;          Uses "Improved standard cotinuation".
    ;;
    (let* ((lim2 (* lim1 100))
       (power-after-1 p)
       (step-size (min (/ lim1 2) (isqrt (/ lim2 2))))
       (d-step-size (* 2 step-size))
       (power-table (make-array (+ 2 step-size)))
       (d-step-size-power (ecm-power power-after-1 d-step-size n a))
       (step-power (ecm-power power-after-1 (1+ d-step-size) n a))
       (last-step-power power-after-1)
       (step-pos 1)
       (q1 3)
       (prime-diffs-pos 0)
       (step-power-buff))
      (init-prime-diffs lim2)
      (setf (aref power-table 1) (ecm-square power-after-1 n a))
      (setf (aref power-table 2) (ecm-square (aref power-table 1) n a))
      (do ((i 3 (1+ i)))
      ((> i step-size))
    (setf (aref power-table i)
          (ecm-product (aref power-table (- i 2)) (aref power-table 1) (aref power-table (1- i)) n)))
      (do ()
      ((> step-pos (- lim2 d-step-size)))
    (let ((buff-prod 1)
          (q-limit (+ step-pos d-step-size))
          (power-table-pos (/ (- q1 step-pos) 2)))
      (when (zerop power-table-pos) ($error q1 step-pos))
      (do ()
          ((> q1 q-limit))
        (let* ((sp1 (car step-power))
           (sp2 (cadr step-power))
           (pp1 (car (aref power-table power-table-pos)))
           (pp2 (cadr (aref power-table power-table-pos)))
           (coord-diffs (mod (- (* sp1 pp2) (* sp2 pp1)) n)))
          (setq buff-prod (mod (* coord-diffs buff-prod) n)))
        (incf q1 (aref *prime-diffs* prime-diffs-pos))
        (incf power-table-pos (/ (aref *prime-diffs* prime-diffs-pos) 2))
        (incf prime-diffs-pos))

      (let ((g (gcd n buff-prod)))
        (when (> g 1)
          (when $ifactor_verbose
        (format t "ECM: found factor in stage 2: ~d (~d digits)~%" g (number-of-digits g)))
          (return-from ecm-factor-with-curve g)))

      (setq step-power-buff step-power)
      (setq step-power (ecm-product last-step-power	d-step-size-power step-power n))
      (setq last-step-power step-power-buff)
      (incf step-pos d-step-size))))
    nil))

(defun get-one-factor-ecm (n)
  (when (primep n) (return-from get-one-factor-ecm n))
  (let ((sigma (+ 6 (random (ash 1 20))))
    (x 0) (z 0) (u 0) (v 0) (a 0) (a1 0) (a2 0)
    (fact) (lim1 $ecm_limit) (a2_inv 0))
    (dotimes (i $ecm_number_of_curves)
      (setq u (mod (- (* sigma sigma) 5) n))
      (setq v (mod (* 4 sigma) n))
      (setq x (mod (expt u 3) n))
      (setq z (mod (expt v 3) n))
      (setq a1 (mod (* (expt (- v u) 3) (+ (* 3 u) v)) n))
      (setq a2 (mod (* 4 x v) n))
      (setq a2_inv (inv-mod a2 n))
      (when (null a2_inv)
    (return-from get-one-factor-ecm (gcd a2 n)))
      (setq a (mod (* a1 a2_inv) n))
      (setq sigma (max 6 (mod (+ (* sigma sigma) 1) n)))
      (when $ifactor_verbose
    (format t "ECM: trying with curve #~d of ~d (lim=~d)~%" (1+ i) $ecm_number_of_curves lim1))
      (setq fact (ecm-factor-with-curve n x z a lim1))
      (when (and fact (< fact n))
    (return-from get-one-factor-ecm fact))
      (setq lim1 (min (+ lim1 $ecm_limit_delta) $ecm_max_limit)))
    nil))


;;; convert (3 5 3 5 3 7) to ((3 3) (5 2) (7 1))

(defun convert-list (l)
  (labels ((convert-list-sub (e n l1 l2)
         (cond ((null l1)
            (cons (list e n) l2))
           ((= e (car l1))
            (convert-list-sub e (1+ n) (cdr l1) l2))
           (t (convert-list-sub (car l1) 1 (cdr l1) (cons `(,e ,n) l2))))))
    (let ((l1 (sort l #'>)))
      (convert-list-sub (car l1) 1 (rest l1) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                       ;;;
;;;           ~*~  IMPLEMENTATION OF PRIMALITY TESTS  ~*~                 ;;;
;;;                                                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmfun $primep (n)
  (if (integerp n)
      (primep (abs n))
      (merror (intl:gettext "primep: argument must be an integer; found: ~M") n)))

(defun primep (n)
  (cond
    ((= n 1) nil)
    ((evenp n) (= n 2))
    ((<= n *largest-small-prime*) (when (member n *small-primes*) t))
    ((< n 9080191) (primep-small n '(31 73)))
    ((< n 4759123141) (primep-small n '(2 7 61)))
    ((< n 2152302898747) (primep-small n '(2 3 5 7 11)))
    ((< n 3474749660383) (primep-small n '(2 3 5 7 11 13)))
    ((< n 341550071728321) (primep-small n '(2 3 5 7 11 13 17)))
    ((< n 3825123056546413051) (primep-small n '(2 3 5 7 11 13 17 19 23)))
    ((< n 318665857834031151167461) (primep-small n '(2 3 5 7 11 13 17 19 23 29 31 37)))
    ((< n 3317044064679887385961981) (primep-small n '(2 3 5 7 11 13 17 19 23 29 31 37 41)))
    ((member n *large-primes*) t)
    (t (primep-prob n)) ))

;;; A Miller-Rabin test is deterministic for small n if we test for small bases.
;;; Reference:
;;;  [1] G. Jaeschke, On Strong Pseudoprimes to Several Bases,
;;;         Math. Comp., 61 (1993), 915-926.
;;;  [2] http://primes.utm.edu/prove/prove2_3.html
;;;  [3] Jiang, Deng - Strong pseudoprimes to the first eight prime bases (2014)
;;;         Mathematics of Computation, Vol 83, Nr 290, Pages 2915--2924
;;;  [3] Sorenson, Webster - Strong Pseudoprimes to Twelve Prime Bases (2015)
;;;         arXiv:1509.00864v1 [math.NT]

(defun primep-small (n bases)
  (multiple-value-bind (q k) (miller-rabin-decomposition n)
    (dolist (x bases t)
      (unless (miller-rabin-kernel n q k x)
        (return-from primep-small nil) ))))

;;; strong primality test:
;;;  - run $primep_number_of_tests times a Miller-Rabin test
;;;  - run one Lucas test

(defun primep-prob (n)
  ;; Miller-Rabin tests:
  (multiple-value-bind (q k) (miller-rabin-decomposition n)
    (dotimes (i $primep_number_of_tests)
      (unless (miller-rabin-kernel n q k)
        (return-from primep-prob nil) )))
  ;; Lucas test:
  (primep-lucas n) )


;;; Miller-Rabin (algorithm P from D. Knuth, TAOCP, 4.5.4)
;;;
;;;   - write n-1 = q*2^k (n,q odd, n > 2)
;;;   - x is a random number 1 < x < n
;;;   - n passes the test if
;;;         x^q = 1 (mod n)
;;;         or x^(q*2^j) = -1 (mod n) for some j = 0..k-1
;;;
;;; A prime number must pass this test.
;;; The probability of passing one test and not being a prime is less than 1/4.

;; return values q,k with n-1 = q*2^k
(defun miller-rabin-decomposition (n) ;; assume n > 2 (n-1 is even)
  (do ((k 1 (1+ k))
       (q (ash n -1) (ash q -1)) )
      ((logbitp 0 q) (values q k)) ))
;;
;; now assume n-1 = q*2^k, k >= 1
(defun miller-rabin-kernel (n q k &optional x)
  (unless x
    (setq x (+ (random (- n 2)) 2)) )
  (let ((y (power-mod x q n)) ;; j = 0
        (minus1 (1- n)) )
    (if (or (= y 1) (= y minus1))
      t
      (do ((j 1 (1+ j)))
          ((= j k))
        (setq y (power-mod y 2 n))
        (when (= y minus1) (return t))
        (when (= y 1) (return)) )))) ;; n prime => last y must have been 1 or -1


(defmfun $power_mod (b e m)
  (unless (and (integerp b) (integerp e) (integerp m))
    (merror (intl:gettext "power_mod: arguments must be integers; found: ~M, ~M, ~M") b e m) )
  (if (>= e 0)
    (power-mod b e m)
    (let ((inv (inv-mod b m)))
      (when inv
        (power-mod inv (- e) m) ))))
;;
(defun power-mod (b e m)
  (declare (optimize (speed 3) (safety 0)))
  (cond
    ((zerop e)
      (mod 1 m) )
    ((typep e 'fixnum)
      (do ((res 1)) (())
        (when (logbitp 0 e)
          (setq res (mod (* res b) m))
          (when (= 1 e) (return res)) )
        (setq e (ash e -1)
              b (mod (* b b) m)) ))
    (t ;; sliding window variant:
      (let* ((l (integer-length e))
             (k (cond ((< l  65) 3)
                      ((< l 161) 4)
                      ((< l 385) 5)
                      ((< l 897) 6)
                      (t         7) ))
             (tab (power-mod-tab b k m))
             (res 1) s u tmp )
        (do ((i (1- l)))
            ((< i 0) res)
          (cond
            ((logbitp i e)
              (setq s (max (1+ (- i k)) 0))
              (do () ((logbitp s e)) (incf s))
              (setq tmp (1+ (- i s)))
              (dotimes (h tmp) (setq res (mod (* res res) m)))
              (setq u (ldb (byte tmp s) e))
              (unless (= u 0) (setq res (mod (* res (svref tab (ash u -1))) m)))
              (setq i (1- s)) )
            (t
              (setq res (mod (* res res) m))
              (decf i) )))))))
;;
(defun power-mod-tab (b k m)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((l (ash 1 (1- k)))
         (tab (make-array l :element-type 'integer :initial-element 1))
         (bi b)
         (bb (mod (* b b) m)) )
    (setf (svref tab 0) b)
    (do ((i 1 (1+ i)))
        ((= i l) tab)
      (setq bi (mod (* bi bb) m))
      (setf (svref tab i) bi) )))

;;; primep-lucas:
;;;
;;;  Define: x^2-a*x+b, D=a^2-4*b; x1, x2 roots of x^2+a*x+b;
;;;  U[k]=(x1^k-x2^k)/(x1-x2), V[k]=x1^k+x2^k.
;;;
;;;  Lucas theorem: If p is an odd prime, gcd(p,b)=1 and jacobi(D,p)=-1,
;;;                 then p divides U[p+1].
;;;
;;;  We calculate U[p+1] for x^2-b*x+1 where jacobi(b^2-4,n)=-1
;;;  and test if p divides U[p+1].

(defun primep-lucas (n)
  (let (prmp (b 3))
    (loop while (not (= ($jacobi (- (* b b) 4) n) -1)) do
     (incf b))
    (setq prmp (zerop (lucas-sequence (1+ n) b n)))
    (when (and prmp $save_primes)
      (push n *large-primes*))
    prmp))

;;; Get element U[p+1] of Lucas sequence for x^2-p*x+1.
;;;
;;; Uses algorithm from M. Joye and J.-J. Quisquater,
;;;                     Efficient computation of full Lucas sequences, 1996

(defun lucas-sequence (k p n)
  (let ((uh 1) (vl 2) (vh p) (s 0) l)
    (do ()
    ((logbitp 0 k))
      (setq k (ash k -1))
      (setq s (1+ s)))

    (setq l (integer-length k))

    (do ((j (1- l) (1- j)))
    ((= 0 j))
      (if (logbitp j k)
      (progn
        (setq uh (mod (* uh vh) n))
        (setq vl (mod (- (* vh vl) p) n))
        (setq vh (mod (- (* vh vh) 2) n)))
      (progn
        (setq uh (mod (1- (* uh vl)) n))
        (setq vh (mod (- (* vh vl) p) n))
        (setq vl (mod (- (* vl vl) 2) n)))))

    (setq uh (mod (1- (* uh vl)) n))
    (setq vl (mod (- (* vh vl) p) n))

    (dotimes (j s)
      (setq uh (mod (* uh vl) n))
      (setq vl (mod (- (* vl vl) 2) n)))
    uh))

;;; first values of next_prime
(defvar *next_prime_ar* #(0 2 3 5 5 7 7))

;;; first values of prev_prime
(defvar *prev_prime_ar* #(0 0 0 2 3 3 5 5 7 7 7 7))

;;; gaps between numbers that are not multiples of 2,3,5,7
(defvar deltaprimes_next
  '(1 10 9 8 7 6 5 4 3 2 1 2 1 4 3 2 1 2 1 4 3 2 1 6 5 4 3 2 1 2
    1 6 5 4 3 2 1 4 3 2 1 2 1 4 3 2 1 6 5 4 3 2 1 6 5 4 3 2 1 2 1
    6 5 4 3 2 1 4 3 2 1 2 1 6 5 4 3 2 1 4 3 2 1 6 5 4 3 2 1 8 7 6
    5 4 3 2 1 4 3 2 1 2 1 4 3 2 1 2 1 4 3 2 1 8 7 6 5 4 3 2 1 6 5
    4 3 2 1 4 3 2 1 6 5 4 3 2 1 2 1 4 3 2 1 6 5 4 3 2 1 2 1 6 5 4
    3 2 1 6 5 4 3 2 1 4 3 2 1 2 1 4 3 2 1 6 5 4 3 2 1 2 1 6 5 4 3
    2 1 4 3 2 1 2 1 4 3 2 1 2 1 10 9 8 7 6 5 4 3 2 1 2))

(defvar deltaprimes_prev
  '(-1 -2 -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -1 -2 -1 -2 -3 -4 -1 -2
    -1 -2 -3 -4 -1 -2 -3 -4 -5 -6 -1 -2 -1 -2 -3 -4 -5 -6 -1 -2 -3
    -4 -1 -2 -1 -2 -3 -4 -1 -2 -3 -4 -5 -6 -1 -2 -3 -4 -5 -6 -1 -2
    -1 -2 -3 -4 -5 -6 -1 -2 -3 -4 -1 -2 -1 -2 -3 -4 -5 -6 -1 -2 -3
    -4 -1 -2 -3 -4 -5 -6 -1 -2 -3 -4 -5 -6 -7 -8 -1 -2 -3 -4 -1 -2
    -1 -2 -3 -4 -1 -2 -1 -2 -3 -4 -1 -2 -3 -4 -5 -6 -7 -8 -1 -2 -3
    -4 -5 -6 -1 -2 -3 -4 -1 -2 -3 -4 -5 -6 -1 -2 -1 -2 -3 -4 -1 -2
    -3 -4 -5 -6 -1 -2 -1 -2 -3 -4 -5 -6 -1 -2 -3 -4 -5 -6 -1 -2 -3
    -4 -1 -2 -1 -2 -3 -4 -1 -2 -3 -4 -5 -6 -1 -2 -1 -2 -3 -4 -5 -6
    -1 -2 -3 -4 -1 -2 -1 -2 -3 -4 -1 -2 -1 -2 -3 -4 -5 -6 -7 -8 -9
    -10))

;;; product of primes in [59..2897]
(defvar bigprimemultiple 6805598092615180737440235028147472981586738014295015027644884201753964648883910180850814465749532893719128055374719237806417537893593625321589379773764981786235326314555704406245399180879758341371676681401881451390195684863765326592983982964414393796690715805513465774520452671995927595391575142047776807977863591126244782181086547150369260177339043045082132788709080989495477932949788444703905327686499493503904132269141007955089790798876488207574072278769735865653223865994494346936718462923487228576140267887355548289736131557613540186975875834980017431190021254898173201223012171417763388931502928376549397638685218312217808199405294916194758171476025904777185780125034583816795375331627264462778001498062163759312245245590800878057927864359433868165604228946307536835897173733369926842890411102870160854438921809703357774373318146115616129588245083207631664167515206143659538759733110973189757163548882116479710800109577584318611988710048552969742803870964125788279451564113232340649434743105271873797620278073136369295820926294656549976175331880139356684249842712956493849288710258349886914201056170180503844749859595207139766052196982574437241716274871254310342540993006427120762049161745282399431514257565489)

(defmfun $next_prime (n)
  (unless (and (integerp n))
    (merror (intl:gettext "next_prime: argument must be an integer; found: ~M") n))
  (cond ((< n 2) 2)
    ((<= n 6) (aref *next_prime_ar* n))
    ((< n 100000) (return-from $next_prime (next-prime-det n deltaprimes_next)))
    (t (next-prime-prob n deltaprimes_next))))

(defmfun $prev_prime (n)
  (unless (and (integerp n) (> n 2))
    (merror (intl:gettext "prev_prime: argument must be an integer greater than 2; found: ~M") n))
  (if (<= n 11) (return-from $prev_prime (aref *prev_prime_ar* n)))
  (if (< n 100000) (return-from $prev_prime (next-prime-det n deltaprimes_prev)))
  (next-prime-prob n deltaprimes_prev))


;;; Find next/prev prime using deterministic test that checks all
;;; divisors < sqrt(n) and skipping all multiples of 2,3,5,7
;;; preconditions: 11 < n < 9973*9973
(defun next-prime-det (n deltaprimes)
  (incf n (nth (mmod n 210) deltaprimes))
  (loop while 1 do
       (dolist (p *small-primes*)
     (if (= (mmod n p) 0) (return))
     (if (>= (* p p) n) (return-from next-prime-det n)))
       (incf n (nth (mmod n 210) deltaprimes))))

;;; Find next/prev prime using probabilistic test and skipping al multiples of
;;; 2,3,5,7 using deltaprimes list and calculating gcd's with product of
;;; prime numbers
(defun next-prime-prob (n deltaprimes)
  ;; skip all multiples of 2,3,5,7
  (incf n (nth (mmod n 210) deltaprimes))
  (loop
     (and
      ;; gcd agaist product of primes in [11..31]
      (= (gcd n 955049953) 1)
      ;; gcd agaist product of primes in [37..53]
      (= (gcd n 162490421) 1)
      ;; gcd agaist product of primes in [59..2897]
      (= (gcd n bigprimemultiple) 1)
      (primep n)
      (return-from next-prime-prob n))
     ;; skip all multiples of 2,3,5,7"
     (incf n (nth (mmod n 210) deltaprimes))))


(defun next-prime (n c)
  (when (evenp n) (incf n c))
  (loop
     (when (primep n)
       (return-from next-prime n))
     (incf n (* 2 c))))

;;; return a list of all primes between start and end

(defmfun $primes (start end)
  (unless (and (integerp start) (integerp end))
    (merror (intl:gettext "primes: arguments must be integers; found: ~M, ~M") start end))
  (let ((primes nil))
    (cond
      ;; take primes from *small-primes* if possible
      ((<= start *largest-small-prime*)
        (dolist (n *small-primes*)
          (when (<= start n end)
            (push n primes) ))
        (setq start *largest-small-prime*) )
      (t
        (decf start) )) ; $next_prime returns a value >= argument + 1
    ;; search for the rest of primes
    (do ((n ($next_prime start) ($next_prime (1+ n))))
        ((> n end) (cons '(mlist) (reverse primes)))
      (push n primes) )))
