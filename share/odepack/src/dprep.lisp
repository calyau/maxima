;;; Compiled by f2cl version:
;;; ("f2cl1.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl2.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl3.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl4.l,v 96616d88fb7e 2008/02/22 22:19:34 rtoy $"
;;;  "f2cl5.l,v 95098eb54f13 2013/04/01 00:45:16 toy $"
;;;  "f2cl6.l,v 1d5cbacbb977 2008/08/24 00:56:27 rtoy $"
;;;  "macros.l,v 1409c1352feb 2013/03/24 20:44:50 toy $")

;;; Using Lisp CMU Common Lisp snapshot-2013-11 (20E Unicode)
;;; 
;;; Options: ((:prune-labels nil) (:auto-save t) (:relaxed-array-decls t)
;;;           (:coerce-assigns :as-needed) (:array-type ':array)
;;;           (:array-slicing t) (:declare-common nil)
;;;           (:float-format single-float))

(in-package "ODEPACK")


(defun dprep (neq y yh savf ewt ftem ia ja wk iwk ipper f jac)
  (declare (type (f2cl-lib:integer4) ipper)
           (type (array double-float (*)) wk ftem ewt savf yh y)
           (type (array f2cl-lib:integer4 (*)) iwk ja ia neq))
  (let ()
    (symbol-macrolet ((tn (aref (dls001-part-0 *dls001-common-block*) 216))
                      (miter (aref (dls001-part-1 *dls001-common-block*) 26))
                      (n (aref (dls001-part-1 *dls001-common-block*) 31))
                      (seth (aref (dlss01-part-0 *dlss01-common-block*) 5))
                      (iesp (aref (dlss01-part-1 *dlss01-common-block*) 1))
                      (istatc (aref (dlss01-part-1 *dlss01-common-block*) 2))
                      (iys (aref (dlss01-part-1 *dlss01-common-block*) 3))
                      (iba (aref (dlss01-part-1 *dlss01-common-block*) 4))
                      (ibian (aref (dlss01-part-1 *dlss01-common-block*) 5))
                      (ibjan (aref (dlss01-part-1 *dlss01-common-block*) 6))
                      (ibjgp (aref (dlss01-part-1 *dlss01-common-block*) 7))
                      (ipian (aref (dlss01-part-1 *dlss01-common-block*) 8))
                      (ipjan (aref (dlss01-part-1 *dlss01-common-block*) 9))
                      (ipjgp (aref (dlss01-part-1 *dlss01-common-block*) 10))
                      (ipigp (aref (dlss01-part-1 *dlss01-common-block*) 11))
                      (ipr (aref (dlss01-part-1 *dlss01-common-block*) 12))
                      (ipc (aref (dlss01-part-1 *dlss01-common-block*) 13))
                      (ipic (aref (dlss01-part-1 *dlss01-common-block*) 14))
                      (ipisp (aref (dlss01-part-1 *dlss01-common-block*) 15))
                      (iprsp (aref (dlss01-part-1 *dlss01-common-block*) 16))
                      (ipa (aref (dlss01-part-1 *dlss01-common-block*) 17))
                      (lenwk (aref (dlss01-part-1 *dlss01-common-block*) 20))
                      (lreq (aref (dlss01-part-1 *dlss01-common-block*) 21))
                      (lrat (aref (dlss01-part-1 *dlss01-common-block*) 22))
                      (moss (aref (dlss01-part-1 *dlss01-common-block*) 25))
                      (ngp (aref (dlss01-part-1 *dlss01-common-block*) 28))
                      (nnz (aref (dlss01-part-1 *dlss01-common-block*) 30))
                      (nsp (aref (dlss01-part-1 *dlss01-common-block*) 31))
                      (nzl (aref (dlss01-part-1 *dlss01-common-block*) 32))
                      (nzu (aref (dlss01-part-1 *dlss01-common-block*) 33)))
      (f2cl-lib:with-multi-array-data
          ((neq f2cl-lib:integer4 neq-%data% neq-%offset%)
           (ia f2cl-lib:integer4 ia-%data% ia-%offset%)
           (ja f2cl-lib:integer4 ja-%data% ja-%offset%)
           (iwk f2cl-lib:integer4 iwk-%data% iwk-%offset%)
           (y double-float y-%data% y-%offset%)
           (yh double-float yh-%data% yh-%offset%)
           (savf double-float savf-%data% savf-%offset%)
           (ewt double-float ewt-%data% ewt-%offset%)
           (ftem double-float ftem-%data% ftem-%offset%)
           (wk double-float wk-%data% wk-%offset%))
        (prog ((nzsut 0) (np1 0) (maxg 0) (liwk 0) (lenigp 0) (ldif 0) (kmin 0)
               (kmax 0) (knew 0) (k 0) (jfound 0) (j 0) (iptt2 0) (iptt1 0)
               (ipiu 0) (ipil 0) (ier 0) (ibr 0) (i 0) (yj 0.0d0) (fac 0.0d0)
               (erwt 0.0d0) (dyj 0.0d0) (dq 0.0d0))
          (declare (type (double-float) dq dyj erwt fac yj)
                   (type (f2cl-lib:integer4) i ibr ier ipil ipiu iptt1 iptt2 j
                                             jfound k knew kmax kmin ldif
                                             lenigp liwk maxg np1 nzsut))
          (setf ibian (f2cl-lib:int-mul lrat 2))
          (setf ipian (f2cl-lib:int-add ibian 1))
          (setf np1 (f2cl-lib:int-add n 1))
          (setf ipjan (f2cl-lib:int-add ipian np1))
          (setf ibjan (f2cl-lib:int-sub ipjan 1))
          (setf liwk (f2cl-lib:int-mul lenwk lrat))
          (if (> (f2cl-lib:int-sub (f2cl-lib:int-add ipjan n) 1) liwk)
              (go label210))
          (if (= moss 0) (go label30))
          (if (= istatc 3) (go label20))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
              (setf erwt
                      (/ 1.0d0
                         (f2cl-lib:fref ewt-%data% (i) ((1 *)) ewt-%offset%)))
              (setf fac (+ 1.0d0 (/ 1.0d0 (+ i 1.0d0))))
              (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                      (+ (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                         (* fac
                            (f2cl-lib:sign erwt
                                           (f2cl-lib:fref y-%data%
                                                          (i)
                                                          ((1 *))
                                                          y-%offset%)))))
             label10))
          (f2cl-lib:computed-goto (label70 label100) moss)
         label20
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label25
              (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                      (f2cl-lib:fref yh-%data% (i) ((1 *)) yh-%offset%))))
          (f2cl-lib:computed-goto (label70 label100) moss)
         label30
          (setf knew ipjan)
          (setf kmin (f2cl-lib:fref ia-%data% (1) ((1 *)) ia-%offset%))
          (setf (f2cl-lib:fref iwk-%data% (ipian) ((1 *)) iwk-%offset%) 1)
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (setf jfound 0)
              (setf kmax
                      (f2cl-lib:int-sub
                       (f2cl-lib:fref ia-%data%
                                      ((f2cl-lib:int-add j 1))
                                      ((1 *))
                                      ia-%offset%)
                       1))
              (if (> kmin kmax) (go label45))
              (f2cl-lib:fdo (k kmin (f2cl-lib:int-add k 1))
                            ((> k kmax) nil)
                (tagbody
                  (setf i (f2cl-lib:fref ja-%data% (k) ((1 *)) ja-%offset%))
                  (if (= i j) (setf jfound 1))
                  (if (> knew liwk) (go label210))
                  (setf (f2cl-lib:fref iwk-%data% (knew) ((1 *)) iwk-%offset%)
                          i)
                  (setf knew (f2cl-lib:int-add knew 1))
                 label40))
              (if (= jfound 1) (go label50))
             label45
              (if (> knew liwk) (go label210))
              (setf (f2cl-lib:fref iwk-%data% (knew) ((1 *)) iwk-%offset%) j)
              (setf knew (f2cl-lib:int-add knew 1))
             label50
              (setf (f2cl-lib:fref iwk-%data%
                                   ((f2cl-lib:int-add ipian j))
                                   ((1 *))
                                   iwk-%offset%)
                      (f2cl-lib:int-sub (f2cl-lib:int-add knew 1) ipjan))
              (setf kmin (f2cl-lib:int-add kmax 1))
             label60))
          (go label140)
         label70
          (multiple-value-bind (var-0 var-1 var-2 var-3)
              (funcall f neq tn y savf)
            (declare (ignore var-0 var-2 var-3))
            (when var-1
              (setf tn var-1)))
          (setf k ipjan)
          (setf (f2cl-lib:fref iwk-%data% (ipian) ((1 *)) iwk-%offset%) 1)
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (if (> k liwk) (go label210))
              (setf (f2cl-lib:fref iwk-%data% (k) ((1 *)) iwk-%offset%) j)
              (setf k (f2cl-lib:int-add k 1))
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                 label75
                  (setf (f2cl-lib:fref savf-%data% (i) ((1 *)) savf-%offset%)
                          0.0d0)))
              (multiple-value-bind (var-0 var-1 var-2 var-3 var-4 var-5 var-6)
                  (funcall jac
                           neq
                           tn
                           y
                           j
                           (f2cl-lib:array-slice iwk-%data%
                                                 f2cl-lib:integer4
                                                 (ipian)
                                                 ((1 *))
                                                 iwk-%offset%)
                           (f2cl-lib:array-slice iwk-%data%
                                                 f2cl-lib:integer4
                                                 (ipjan)
                                                 ((1 *))
                                                 iwk-%offset%)
                           savf)
                (declare (ignore var-0 var-2 var-4 var-5 var-6))
                (when var-1
                  (setf tn var-1))
                (when var-3
                  (setf j var-3)))
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                  (if
                   (<=
                    (abs (f2cl-lib:fref savf-%data% (i) ((1 *)) savf-%offset%))
                    seth)
                   (go label80))
                  (if (= i j) (go label80))
                  (if (> k liwk) (go label210))
                  (setf (f2cl-lib:fref iwk-%data% (k) ((1 *)) iwk-%offset%) i)
                  (setf k (f2cl-lib:int-add k 1))
                 label80))
              (setf (f2cl-lib:fref iwk-%data%
                                   ((f2cl-lib:int-add ipian j))
                                   ((1 *))
                                   iwk-%offset%)
                      (f2cl-lib:int-sub (f2cl-lib:int-add k 1) ipjan))
             label90))
          (go label140)
         label100
          (setf k ipjan)
          (setf (f2cl-lib:fref iwk-%data% (ipian) ((1 *)) iwk-%offset%) 1)
          (multiple-value-bind (var-0 var-1 var-2 var-3)
              (funcall f neq tn y savf)
            (declare (ignore var-0 var-2 var-3))
            (when var-1
              (setf tn var-1)))
          (f2cl-lib:fdo (j 1 (f2cl-lib:int-add j 1))
                        ((> j n) nil)
            (tagbody
              (if (> k liwk) (go label210))
              (setf (f2cl-lib:fref iwk-%data% (k) ((1 *)) iwk-%offset%) j)
              (setf k (f2cl-lib:int-add k 1))
              (setf yj (f2cl-lib:fref y-%data% (j) ((1 *)) y-%offset%))
              (setf erwt
                      (/ 1.0d0
                         (f2cl-lib:fref ewt-%data% (j) ((1 *)) ewt-%offset%)))
              (setf dyj (f2cl-lib:sign erwt yj))
              (setf (f2cl-lib:fref y-%data% (j) ((1 *)) y-%offset%) (+ yj dyj))
              (multiple-value-bind (var-0 var-1 var-2 var-3)
                  (funcall f neq tn y ftem)
                (declare (ignore var-0 var-2 var-3))
                (when var-1
                  (setf tn var-1)))
              (setf (f2cl-lib:fref y-%data% (j) ((1 *)) y-%offset%) yj)
              (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                            ((> i n) nil)
                (tagbody
                  (setf dq
                          (/
                           (-
                            (f2cl-lib:fref ftem-%data%
                                           (i)
                                           ((1 *))
                                           ftem-%offset%)
                            (f2cl-lib:fref savf-%data%
                                           (i)
                                           ((1 *))
                                           savf-%offset%))
                           dyj))
                  (if (<= (abs dq) seth) (go label110))
                  (if (= i j) (go label110))
                  (if (> k liwk) (go label210))
                  (setf (f2cl-lib:fref iwk-%data% (k) ((1 *)) iwk-%offset%) i)
                  (setf k (f2cl-lib:int-add k 1))
                 label110))
              (setf (f2cl-lib:fref iwk-%data%
                                   ((f2cl-lib:int-add ipian j))
                                   ((1 *))
                                   iwk-%offset%)
                      (f2cl-lib:int-sub (f2cl-lib:int-add k 1) ipjan))
             label120))
         label140
          (if (or (= moss 0) (/= istatc 1)) (go label150))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label145
              (setf (f2cl-lib:fref y-%data% (i) ((1 *)) y-%offset%)
                      (f2cl-lib:fref yh-%data% (i) ((1 *)) yh-%offset%))))
         label150
          (setf nnz
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref iwk-%data%
                                  ((f2cl-lib:int-add ipian n))
                                  ((1 *))
                                  iwk-%offset%)
                   1))
          (setf lenigp 0)
          (setf ipigp (f2cl-lib:int-add ipjan nnz))
          (if (/= miter 2) (go label160))
          (setf maxg np1)
          (setf ipjgp (f2cl-lib:int-add ipjan nnz))
          (setf ibjgp (f2cl-lib:int-sub ipjgp 1))
          (setf ipigp (f2cl-lib:int-add ipjgp n))
          (setf iptt1 (f2cl-lib:int-add ipigp np1))
          (setf iptt2 (f2cl-lib:int-add iptt1 n))
          (setf lreq (f2cl-lib:int-sub (f2cl-lib:int-add iptt2 n) 1))
          (if (> lreq liwk) (go label220))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (jgroup n
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipian)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipjan)
                                     ((1 *))
                                     iwk-%offset%)
               maxg ngp
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipigp)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipjgp)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (iptt1)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (iptt2)
                                     ((1 *))
                                     iwk-%offset%)
               ier)
            (declare (ignore var-0 var-1 var-2 var-3 var-5 var-6 var-7 var-8))
            (setf ngp var-4)
            (setf ier var-9))
          (if (/= ier 0) (go label220))
          (setf lenigp (f2cl-lib:int-add ngp 1))
         label160
          (setf ipr (f2cl-lib:int-add ipigp lenigp))
          (setf ipc ipr)
          (setf ipic (f2cl-lib:int-add ipc n))
          (setf ipisp (f2cl-lib:int-add ipic n))
          (setf iprsp
                  (+ (the f2cl-lib:integer4 (truncate (- ipisp 2) lrat)) 2))
          (setf iesp (f2cl-lib:int-sub (f2cl-lib:int-add lenwk 1) iprsp))
          (if (< iesp 0) (go label230))
          (setf ibr (f2cl-lib:int-sub ipr 1))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i n) nil)
            (tagbody
             label170
              (setf (f2cl-lib:fref iwk-%data%
                                   ((f2cl-lib:int-add ibr i))
                                   ((1 *))
                                   iwk-%offset%)
                      i)))
          (setf nsp (f2cl-lib:int-sub (f2cl-lib:int-add liwk 1) ipisp))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9)
              (odrv n
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipian)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipjan)
                                     ((1 *))
                                     iwk-%offset%)
               wk
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipr)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipic)
                                     ((1 *))
                                     iwk-%offset%)
               nsp
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipisp)
                                     ((1 *))
                                     iwk-%offset%)
               1 iys)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8))
            (setf iys var-9))
          (if (= iys (f2cl-lib:int-add (f2cl-lib:int-mul 11 n) 1))
              (go label240))
          (if (/= iys 0) (go label230))
          (setf ipa (f2cl-lib:int-sub (f2cl-lib:int-add lenwk 1) nnz))
          (setf nsp (f2cl-lib:int-sub ipa iprsp))
          (setf lreq
                  (+
                   (max (the f2cl-lib:integer4 (truncate (* 12 n) lrat))
                        (+ (the f2cl-lib:integer4 (truncate (* 6 n) lrat))
                           (f2cl-lib:int-mul 2 n)
                           nnz))
                   3))
          (setf lreq
                  (f2cl-lib:int-add
                   (f2cl-lib:int-sub (f2cl-lib:int-add lreq iprsp) 1)
                   nnz))
          (if (> lreq lenwk) (go label250))
          (setf iba (f2cl-lib:int-sub ipa 1))
          (f2cl-lib:fdo (i 1 (f2cl-lib:int-add i 1))
                        ((> i nnz) nil)
            (tagbody
             label180
              (setf (f2cl-lib:fref wk-%data%
                                   ((f2cl-lib:int-add iba i))
                                   ((1 *))
                                   wk-%offset%)
                      0.0d0)))
          (setf ipisp
                  (f2cl-lib:int-add
                   (f2cl-lib:int-mul lrat (f2cl-lib:int-sub iprsp 1))
                   1))
          (multiple-value-bind
                (var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7 var-8 var-9
                 var-10 var-11 var-12 var-13 var-14)
              (cdrv n
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipr)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipc)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipic)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipian)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipjan)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice wk-%data%
                                     double-float
                                     (ipa)
                                     ((1 *))
                                     wk-%offset%)
               (f2cl-lib:array-slice wk-%data%
                                     double-float
                                     (ipa)
                                     ((1 *))
                                     wk-%offset%)
               (f2cl-lib:array-slice wk-%data%
                                     double-float
                                     (ipa)
                                     ((1 *))
                                     wk-%offset%)
               nsp
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipisp)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice wk-%data%
                                     double-float
                                     (iprsp)
                                     ((1 *))
                                     wk-%offset%)
               iesp 5 iys)
            (declare (ignore var-0 var-1 var-2 var-3 var-4 var-5 var-6 var-7
                             var-8 var-9 var-10 var-11 var-13))
            (setf iesp var-12)
            (setf iys var-14))
          (setf lreq (f2cl-lib:int-sub lenwk iesp))
          (if (= iys (f2cl-lib:int-add (f2cl-lib:int-mul 10 n) 1))
              (go label250))
          (if (/= iys 0) (go label260))
          (setf ipil ipisp)
          (setf ipiu (f2cl-lib:int-add ipil (f2cl-lib:int-mul 2 n) 1))
          (setf nzu
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref iwk-%data%
                                  ((f2cl-lib:int-add ipil n))
                                  ((1 *))
                                  iwk-%offset%)
                   (f2cl-lib:fref iwk-%data% (ipil) ((1 *)) iwk-%offset%)))
          (setf nzl
                  (f2cl-lib:int-sub
                   (f2cl-lib:fref iwk-%data%
                                  ((f2cl-lib:int-add ipiu n))
                                  ((1 *))
                                  iwk-%offset%)
                   (f2cl-lib:fref iwk-%data% (ipiu) ((1 *)) iwk-%offset%)))
          (if (> lrat 1) (go label190))
          (multiple-value-bind (var-0 var-1 var-2)
              (adjlr n
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipisp)
                                     ((1 *))
                                     iwk-%offset%)
               ldif)
            (declare (ignore var-0 var-1))
            (setf ldif var-2))
          (setf lreq (f2cl-lib:int-add lreq ldif))
         label190
          (if (and (= lrat 2) (= nnz n)) (setf lreq (f2cl-lib:int-add lreq 1)))
          (setf nsp (f2cl-lib:int-sub (f2cl-lib:int-add nsp lreq) lenwk))
          (setf ipa (f2cl-lib:int-sub (f2cl-lib:int-add lreq 1) nnz))
          (setf iba (f2cl-lib:int-sub ipa 1))
          (setf ipper 0)
          (go end_label)
         label210
          (setf ipper -1)
          (setf lreq
                  (+ 2 (the f2cl-lib:integer4 (truncate (+ (* 2 n) 1) lrat))))
          (setf lreq
                  (max (the f2cl-lib:integer4 (f2cl-lib:int-add lenwk 1))
                       (the f2cl-lib:integer4 lreq)))
          (go end_label)
         label220
          (setf ipper -2)
          (setf lreq (+ (the f2cl-lib:integer4 (truncate (- lreq 1) lrat)) 1))
          (go end_label)
         label230
          (setf ipper -3)
          (multiple-value-bind (var-0 var-1 var-2 var-3)
              (cntnzu n
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipian)
                                     ((1 *))
                                     iwk-%offset%)
               (f2cl-lib:array-slice iwk-%data%
                                     f2cl-lib:integer4
                                     (ipjan)
                                     ((1 *))
                                     iwk-%offset%)
               nzsut)
            (declare (ignore var-0 var-1 var-2))
            (setf nzsut var-3))
          (setf lreq
                  (+ (f2cl-lib:int-sub lenwk iesp)
                     (the f2cl-lib:integer4
                          (truncate (- (+ (* 3 n) (* 4 nzsut)) 1) lrat))
                     1))
          (go end_label)
         label240
          (setf ipper -4)
          (go end_label)
         label250
          (setf ipper -5)
          (go end_label)
         label260
          (setf ipper -6)
          (setf lreq lenwk)
          (go end_label)
         end_label
          (return
           (values nil nil nil nil nil nil nil nil nil nil ipper nil nil)))))))

(in-package #-gcl #:cl-user #+gcl "CL-USER")
#+#.(cl:if (cl:find-package '#:f2cl) '(and) '(or))
(eval-when (:load-toplevel :compile-toplevel :execute)
  (setf (gethash 'fortran-to-lisp::dprep fortran-to-lisp::*f2cl-function-info*)
          (fortran-to-lisp::make-f2cl-finfo
           :arg-types '((array fortran-to-lisp::integer4 (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*)) (array double-float (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array fortran-to-lisp::integer4 (*))
                        (array double-float (*))
                        (array fortran-to-lisp::integer4 (*))
                        (fortran-to-lisp::integer4) t t)
           :return-values '(nil nil nil nil nil nil nil nil nil nil
                            fortran-to-lisp::ipper nil nil)
           :calls '(fortran-to-lisp::cntnzu fortran-to-lisp::adjlr
                    fortran-to-lisp::cdrv fortran-to-lisp::odrv
                    fortran-to-lisp::jgroup))))

