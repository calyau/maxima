;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2020-04 (21D Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format double-float))

(in-package "HOMPACK")


(defun initp (iflg1 n numt kdeg coef nn mmaxt par ipar ideg facv cl pdg qdg r)
  (declare (type (array double-float (*)) r qdg pdg cl facv par coef)
           (type (array f2cl-lib:integer4 (*)) ideg ipar kdeg numt)
           (type (f2cl-lib:integer4) mmaxt nn n iflg1))
  (f2cl-lib:with-multi-array-data
      ((numt f2cl-lib:integer4 numt-%data% numt-%offset%)
       (kdeg f2cl-lib:integer4 kdeg-%data% kdeg-%offset%)
       (ipar f2cl-lib:integer4 ipar-%data% ipar-%offset%)
       (ideg f2cl-lib:integer4 ideg-%data% ideg-%offset%)
       (coef double-float coef-%data% coef-%offset%)
       (par double-float par-%data% par-%offset%)
       (facv double-float facv-%data% facv-%offset%)
       (cl double-float cl-%data% cl-%offset%)
       (pdg double-float pdg-%data% pdg-%offset%)
       (qdg double-float qdg-%data% qdg-%offset%)
       (r double-float r-%data% r-%offset%))
    (prog ((p (make-array 20 :element-type 'double-float))
           (q (make-array 20 :element-type 'double-float))
           (ccl (make-array 22 :element-type 'double-float)) (zero 0.0) (i 0)
           (ierr 0) (iideg 0) (j 0) (jj 0) (k 0) (l 0) (n2 0) (np1 0))
      (declare (type (f2cl-lib:integer4) np1 n2 l k jj j iideg ierr i)
               (type (double-float) zero)
               (type (array double-float (22)) ccl)
               (type (array double-float (20)) q p))
      (setf zero (coerce 0.0f0 'double-float))
      (setf n2 (f2cl-lib:int-mul 2 n))
      (setf np1 (f2cl-lib:int-add n 1))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (setf (f2cl-lib:fref ideg-%data% (j) ((1 n)) ideg-%offset%) 0)
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k (f2cl-lib:fref numt (j) ((1 nn)))) nil)
            (tagbody
              (setf iideg 0)
              (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                            ((> l n) nil)
                (tagbody
                  (setf iideg
                          (f2cl-lib:int-add iideg
                                            (f2cl-lib:fref kdeg-%data%
                                                           (j l k)
                                                           ((1 nn)
                                                            (1
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                                            (1 mmaxt))
                                                           kdeg-%offset%)))
                 label12))
              (if
               (> iideg (f2cl-lib:fref ideg-%data% (j) ((1 n)) ideg-%offset%))
               (setf (f2cl-lib:fref ideg-%data% (j) ((1 n)) ideg-%offset%)
                       iideg))
             label15))))
     label15
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                        ((> k (f2cl-lib:fref numt (j) ((1 nn)))) nil)
            (tagbody
              (setf iideg 0)
              (f2cl-lib:fdo (l 1 (f2cl-lib:int-add l 1))
                            ((> l n) nil)
                (tagbody
                  (setf iideg
                          (f2cl-lib:int-add iideg
                                            (f2cl-lib:fref kdeg-%data%
                                                           (j l k)
                                                           ((1 nn)
                                                            (1
                                                             (f2cl-lib:int-add
                                                              nn
                                                              1))
                                                            (1 mmaxt))
                                                           kdeg-%offset%)))
                 label22))
              (setf (f2cl-lib:fref kdeg-%data%
                                   (j np1 k)
                                   ((1 nn) (1 (f2cl-lib:int-add nn 1))
                                    (1 mmaxt))
                                   kdeg-%offset%)
                      (f2cl-lib:int-sub
                       (f2cl-lib:fref ideg-%data% (j) ((1 n)) ideg-%offset%)
                       iideg))
             label25))))
     label25
      (cond
        ((or (= iflg1 10) (= iflg1 0))
         (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                       ((> i n) nil)
           (tagbody
             (setf (f2cl-lib:fref facv-%data% (i) ((1 n)) facv-%offset%)
                     (coerce 0.0f0 'double-float))
            label30)))
        (t
         (multiple-value-bind
               (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                var-10 var-11 var-12 var-13 var-14 var-15 var-16 var-17 var-18)
             (sclgnp n nn mmaxt numt kdeg 0 zero coef
              (f2cl-lib:array-slice ipar-%data%
                                    f2cl-lib:integer4
                                    (1)
                                    ((1
                                      (f2cl-lib:int-add 42
                                                        (f2cl-lib:int-mul 2 n)
                                                        (f2cl-lib:int-mul n
                                                                          (f2cl-lib:int-add
                                                                           n
                                                                           1)
                                                                          mmaxt))))
                                    ipar-%offset%)
              (f2cl-lib:array-slice ipar-%data%
                                    f2cl-lib:integer4
                                    ((+ 1 n))
                                    ((1
                                      (f2cl-lib:int-add 42
                                                        (f2cl-lib:int-mul 2 n)
                                                        (f2cl-lib:int-mul n
                                                                          (f2cl-lib:int-add
                                                                           n
                                                                           1)
                                                                          mmaxt))))
                                    ipar-%offset%)
              (f2cl-lib:array-slice par-%data%
                                    double-float
                                    (1)
                                    ((1
                                      (f2cl-lib:int-add 2
                                                        (f2cl-lib:int-mul 28 n)
                                                        (f2cl-lib:int-mul 6
                                                                          (expt
                                                                           n
                                                                           2))
                                                        (f2cl-lib:int-mul 7
                                                                          n
                                                                          mmaxt)
                                                        (f2cl-lib:int-mul 4
                                                                          (expt
                                                                           n
                                                                           2)
                                                                          mmaxt))))
                                    par-%offset%)
              (f2cl-lib:array-slice par-%data%
                                    double-float
                                    ((+ 1 (f2cl-lib:int-mul n mmaxt)))
                                    ((1
                                      (f2cl-lib:int-add 2
                                                        (f2cl-lib:int-mul 28 n)
                                                        (f2cl-lib:int-mul 6
                                                                          (expt
                                                                           n
                                                                           2))
                                                        (f2cl-lib:int-mul 7
                                                                          n
                                                                          mmaxt)
                                                        (f2cl-lib:int-mul 4
                                                                          (expt
                                                                           n
                                                                           2)
                                                                          mmaxt))))
                                    par-%offset%)
              (f2cl-lib:array-slice par-%data%
                                    double-float
                                    ((+ 1
                                        (f2cl-lib:int-mul n mmaxt)
                                        (f2cl-lib:int-mul 4 (expt n 2))))
                                    ((1
                                      (f2cl-lib:int-add 2
                                                        (f2cl-lib:int-mul 28 n)
                                                        (f2cl-lib:int-mul 6
                                                                          (expt
                                                                           n
                                                                           2))
                                                        (f2cl-lib:int-mul 7
                                                                          n
                                                                          mmaxt)
                                                        (f2cl-lib:int-mul 4
                                                                          (expt
                                                                           n
                                                                           2)
                                                                          mmaxt))))
                                    par-%offset%)
              (f2cl-lib:array-slice par-%data%
                                    double-float
                                    ((+ 1
                                        (f2cl-lib:int-mul n mmaxt)
                                        (f2cl-lib:int-mul 4 (expt n 2))
                                        (f2cl-lib:int-mul 2 n)))
                                    ((1
                                      (f2cl-lib:int-add 2
                                                        (f2cl-lib:int-mul 28 n)
                                                        (f2cl-lib:int-mul 6
                                                                          (expt
                                                                           n
                                                                           2))
                                                        (f2cl-lib:int-mul 7
                                                                          n
                                                                          mmaxt)
                                                        (f2cl-lib:int-mul 4
                                                                          (expt
                                                                           n
                                                                           2)
                                                                          mmaxt))))
                                    par-%offset%)
              (f2cl-lib:array-slice par-%data%
                                    double-float
                                    ((+ 1
                                        (f2cl-lib:int-mul n mmaxt)
                                        (f2cl-lib:int-mul 4 (expt n 2))
                                        (f2cl-lib:int-mul 2 n)
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add
                                                           (f2cl-lib:int-mul 2
                                                                             n)
                                                           1))))
                                    ((1
                                      (f2cl-lib:int-add 2
                                                        (f2cl-lib:int-mul 28 n)
                                                        (f2cl-lib:int-mul 6
                                                                          (expt
                                                                           n
                                                                           2))
                                                        (f2cl-lib:int-mul 7
                                                                          n
                                                                          mmaxt)
                                                        (f2cl-lib:int-mul 4
                                                                          (expt
                                                                           n
                                                                           2)
                                                                          mmaxt))))
                                    par-%offset%)
              facv
              (f2cl-lib:array-slice par-%data%
                                    double-float
                                    ((+ 1
                                        (f2cl-lib:int-mul n mmaxt)
                                        (f2cl-lib:int-mul 4 (expt n 2))
                                        (f2cl-lib:int-mul 4 n)
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add
                                                           (f2cl-lib:int-mul 2
                                                                             n)
                                                           1))))
                                    ((1
                                      (f2cl-lib:int-add 2
                                                        (f2cl-lib:int-mul 28 n)
                                                        (f2cl-lib:int-mul 6
                                                                          (expt
                                                                           n
                                                                           2))
                                                        (f2cl-lib:int-mul 7
                                                                          n
                                                                          mmaxt)
                                                        (f2cl-lib:int-mul 4
                                                                          (expt
                                                                           n
                                                                           2)
                                                                          mmaxt))))
                                    par-%offset%)
              (f2cl-lib:array-slice par-%data%
                                    double-float
                                    ((+ 1
                                        (f2cl-lib:int-mul n mmaxt)
                                        (f2cl-lib:int-mul 4 (expt n 2))
                                        (f2cl-lib:int-mul 5 n)
                                        (f2cl-lib:int-mul n
                                                          (f2cl-lib:int-add
                                                           (f2cl-lib:int-mul 2
                                                                             n)
                                                           1))))
                                    ((1
                                      (f2cl-lib:int-add 2
                                                        (f2cl-lib:int-mul 28 n)
                                                        (f2cl-lib:int-mul 6
                                                                          (expt
                                                                           n
                                                                           2))
                                                        (f2cl-lib:int-mul 7
                                                                          n
                                                                          mmaxt)
                                                        (f2cl-lib:int-mul 4
                                                                          (expt
                                                                           n
                                                                           2)
                                                                          mmaxt))))
                                    par-%offset%)
              ierr)
           (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                            var-8 var-9 var-10 var-11 var-12 var-13 var-14
                            var-15 var-16 var-17))
           (setf ierr var-18))
         (cond
           ((= ierr 0)
            (tagbody
              (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                            ((> j n) nil)
                (tagbody
                  (f2cl-lib:fdo (k 1 (f2cl-lib:int-add k 1))
                                ((> k (f2cl-lib:fref numt (j) ((1 nn)))) nil)
                    (tagbody
                      (setf (f2cl-lib:fref coef-%data%
                                           (j k)
                                           ((1 nn) (1 mmaxt))
                                           coef-%offset%)
                              (f2cl-lib:fref par-%data%
                                             ((f2cl-lib:int-add
                                               (f2cl-lib:int-mul n mmaxt)
                                               (f2cl-lib:int-mul 4 (expt n 2))
                                               (f2cl-lib:int-mul 5 n)
                                               (f2cl-lib:int-mul n
                                                                 (f2cl-lib:int-add
                                                                  (f2cl-lib:int-mul
                                                                   2
                                                                   n)
                                                                  1))
                                               j
                                               (f2cl-lib:int-mul n
                                                                 (f2cl-lib:int-sub
                                                                  k
                                                                  1))))
                                             ((1
                                               (f2cl-lib:int-add 2
                                                                 (f2cl-lib:int-mul
                                                                  28
                                                                  n)
                                                                 (f2cl-lib:int-mul
                                                                  6
                                                                  (expt n 2))
                                                                 (f2cl-lib:int-mul
                                                                  7
                                                                  n
                                                                  mmaxt)
                                                                 (f2cl-lib:int-mul
                                                                  4
                                                                  (expt n 2)
                                                                  mmaxt))))
                                             par-%offset%))
                     label40))))
             label40)))))
      (setf (f2cl-lib:fref p (1 1) ((1 2) (1 10))) 0.12324754231)
      (setf (f2cl-lib:fref p (2 1) ((1 2) (1 10))) 0.76253746298)
      (setf (f2cl-lib:fref p (1 2) ((1 2) (1 10))) 0.9385783895)
      (setf (f2cl-lib:fref p (2 2) ((1 2) (1 10))) -0.9937589281)
      (setf (f2cl-lib:fref p (1 3) ((1 2) (1 10))) -0.23467908356)
      (setf (f2cl-lib:fref p (2 3) ((1 2) (1 10))) 0.39383930009)
      (setf (f2cl-lib:fref p (1 4) ((1 2) (1 10))) 0.83542556622)
      (setf (f2cl-lib:fref p (2 4) ((1 2) (1 10))) -0.10192888288)
      (setf (f2cl-lib:fref p (1 5) ((1 2) (1 10))) -0.55763522521)
      (setf (f2cl-lib:fref p (2 5) ((1 2) (1 10))) -0.83729899911)
      (setf (f2cl-lib:fref p (1 6) ((1 2) (1 10))) -0.78348738738)
      (setf (f2cl-lib:fref p (2 6) ((1 2) (1 10))) -0.10578234903)
      (setf (f2cl-lib:fref p (1 7) ((1 2) (1 10))) 0.03938347346)
      (setf (f2cl-lib:fref p (2 7) ((1 2) (1 10))) 0.04825184716)
      (setf (f2cl-lib:fref p (1 8) ((1 2) (1 10))) -0.43428734331)
      (setf (f2cl-lib:fref p (2 8) ((1 2) (1 10))) 0.93836289418)
      (setf (f2cl-lib:fref p (1 9) ((1 2) (1 10))) -0.99383729993)
      (setf (f2cl-lib:fref p (2 9) ((1 2) (1 10))) -0.40947822291)
      (setf (f2cl-lib:fref p (1 10) ((1 2) (1 10))) 0.09383736736)
      (setf (f2cl-lib:fref p (2 10) ((1 2) (1 10))) 0.26459172298)
      (setf (f2cl-lib:fref q (1 1) ((1 2) (1 10))) 0.58720452864)
      (setf (f2cl-lib:fref q (2 1) ((1 2) (1 10))) 0.01321964722)
      (setf (f2cl-lib:fref q (1 2) ((1 2) (1 10))) 0.978841347)
      (setf (f2cl-lib:fref q (2 2) ((1 2) (1 10))) -0.14433009712)
      (setf (f2cl-lib:fref q (1 3) ((1 2) (1 10))) 0.39383737289)
      (setf (f2cl-lib:fref q (2 3) ((1 2) (1 10))) 0.41543223411)
      (setf (f2cl-lib:fref q (1 4) ((1 2) (1 10))) -0.03938376373)
      (setf (f2cl-lib:fref q (2 4) ((1 2) (1 10))) -0.61253112318)
      (setf (f2cl-lib:fref q (1 5) ((1 2) (1 10))) 0.39383737388)
      (setf (f2cl-lib:fref q (2 5) ((1 2) (1 10))) -0.26454678861)
      (setf (f2cl-lib:fref q (1 6) ((1 2) (1 10))) -0.00938376766)
      (setf (f2cl-lib:fref q (2 6) ((1 2) (1 10))) 0.34447867861)
      (setf (f2cl-lib:fref q (1 7) ((1 2) (1 10))) -0.04837366632)
      (setf (f2cl-lib:fref q (2 7) ((1 2) (1 10))) 0.4825273679)
      (setf (f2cl-lib:fref q (1 8) ((1 2) (1 10))) 0.93725237347)
      (setf (f2cl-lib:fref q (2 8) ((1 2) (1 10))) -0.54356527623)
      (setf (f2cl-lib:fref q (1 9) ((1 2) (1 10))) 0.39373957747)
      (setf (f2cl-lib:fref q (2 9) ((1 2) (1 10))) 0.65573434564)
      (setf (f2cl-lib:fref q (1 10) ((1 2) (1 10))) -0.39380038371)
      (setf (f2cl-lib:fref q (2 10) ((1 2) (1 10))) 0.98903450052)
      (setf (f2cl-lib:fref ccl (1 1) ((1 2) (1 11))) -0.03485644332)
      (setf (f2cl-lib:fref ccl (2 1) ((1 2) (1 11))) 0.28554634336)
      (setf (f2cl-lib:fref ccl (1 2) ((1 2) (1 11))) 0.91453454766)
      (setf (f2cl-lib:fref ccl (2 2) ((1 2) (1 11))) 0.35354566613)
      (setf (f2cl-lib:fref ccl (1 3) ((1 2) (1 11))) -0.36568737635)
      (setf (f2cl-lib:fref ccl (2 3) ((1 2) (1 11))) 0.45634642477)
      (setf (f2cl-lib:fref ccl (1 4) ((1 2) (1 11))) -0.89089767544)
      (setf (f2cl-lib:fref ccl (2 4) ((1 2) (1 11))) 0.34524523544)
      (setf (f2cl-lib:fref ccl (1 5) ((1 2) (1 11))) 0.13523462465)
      (setf (f2cl-lib:fref ccl (2 5) ((1 2) (1 11))) 0.43534535555)
      (setf (f2cl-lib:fref ccl (1 6) ((1 2) (1 11))) -0.34523544445)
      (setf (f2cl-lib:fref ccl (2 6) ((1 2) (1 11))) 0.00734522256)
      (setf (f2cl-lib:fref ccl (1 7) ((1 2) (1 11))) -0.80004678763)
      (setf (f2cl-lib:fref ccl (2 7) ((1 2) (1 11))) -0.009387123644)
      (setf (f2cl-lib:fref ccl (1 8) ((1 2) (1 11))) -0.875432124245)
      (setf (f2cl-lib:fref ccl (2 8) ((1 2) (1 11))) 4.5687651e-4)
      (setf (f2cl-lib:fref ccl (1 9) ((1 2) (1 11))) 0.65256352333)
      (setf (f2cl-lib:fref ccl (2 9) ((1 2) (1 11))) -0.12356777452)
      (setf (f2cl-lib:fref ccl (1 10) ((1 2) (1 11))) 0.09986798321548)
      (setf (f2cl-lib:fref ccl (2 10) ((1 2) (1 11))) -0.56753456577)
      (setf (f2cl-lib:fref ccl (1 11) ((1 2) (1 11))) 0.29674947394739)
      (setf (f2cl-lib:fref ccl (2 11) ((1 2) (1 11))) 0.93274302173)
      (cond
        ((or (= iflg1 1) (= iflg1 0))
         (tagbody
           (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                         ((> i 2) nil)
             (tagbody
               (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                             ((> j n) nil)
                 (tagbody
                   (setf (f2cl-lib:fref cl-%data%
                                        (i j)
                                        ((1 2) (1 (f2cl-lib:int-add n 1)))
                                        cl-%offset%)
                           (coerce 0.0f0 'double-float))
                  label50))))
          label50
           (setf (f2cl-lib:fref cl-%data%
                                (1 np1)
                                ((1 2) (1 (f2cl-lib:int-add n 1)))
                                cl-%offset%)
                   (coerce 1.0f0 'double-float))
           (setf (f2cl-lib:fref cl-%data%
                                (2 np1)
                                ((1 2) (1 (f2cl-lib:int-add n 1)))
                                cl-%offset%)
                   (coerce 0.0f0 'double-float))))
        (t
         (tagbody
           (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                         ((> j np1) nil)
             (tagbody
               (setf jj (f2cl-lib:int-add (mod (f2cl-lib:int-sub j 1) 11) 1))
               (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                             ((> i 2) nil)
                 (tagbody
                   (setf (f2cl-lib:fref cl-%data%
                                        (i j)
                                        ((1 2) (1 (f2cl-lib:int-add n 1)))
                                        cl-%offset%)
                           (f2cl-lib:fref ccl (i jj) ((1 2) (1 11))))
                  label60))))
          label60)))
      (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                    ((> j n) nil)
        (tagbody
          (setf jj (f2cl-lib:int-add (mod (f2cl-lib:int-sub j 1) 10) 1))
          (powp (f2cl-lib:fref ideg-%data% (j) ((1 n)) ideg-%offset%)
           (f2cl-lib:array-slice p double-float (1 jj) ((1 2) (1 10)))
           (f2cl-lib:array-slice pdg-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 pdg-%offset%))
          (powp (f2cl-lib:fref ideg-%data% (j) ((1 n)) ideg-%offset%)
           (f2cl-lib:array-slice q double-float (1 jj) ((1 2) (1 10)))
           (f2cl-lib:array-slice qdg-%data%
                                 double-float
                                 (1 j)
                                 ((1 2) (1 n))
                                 qdg-%offset%))
          (multiple-value-bind (var-0 var-1 var-2 var-3)
              (divp (f2cl-lib:array-slice q double-float (1 jj) ((1 2) (1 10)))
               (f2cl-lib:array-slice p double-float (1 jj) ((1 2) (1 10)))
               (f2cl-lib:array-slice r-%data%
                                     double-float
                                     (1 j)
                                     ((1 2) (1 n))
                                     r-%offset%)
               ierr)
            (declare (ignore var-0 var-1 var-2))
            (setf ierr var-3))
         label70))
      (go end_label)
     end_label
      (return
       (values nil nil nil nil nil nil nil nil nil nil nil nil nil nil nil)))))

(in-package #:cl-user)
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::initp fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((fortran-to-lisp::integer4) (fortran-to-lisp::integer4)
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (fortran-to-lisp::integer4)
                        (fortran-to-lisp::integer4) (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)))
           :return-values '(nil nil nil nil nil nil nil nil nil nil nil nil nil
                            nil nil)
           :calls '(fortran-to-lisp::divp fortran-to-lisp::powp
                    fortran-to-lisp::sclgnp))))

