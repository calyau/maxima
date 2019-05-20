;;********************************************************
;; file:        testsuite.lisp
;; description: Initialize the list of testsuite files
;; date:        Tue Dec 18 2018 - 14:38
;; author:      Various
;;********************************************************

(in-package :maxima)

;;; The Maxima test suite is defined to be the sum of all the tests in
;;; the files listed in the Maxima option variable $testsuite_files.
;;; Each entry can either be a plain file name (minus extension), e.g.,
;;; "testfile.mac", or a Maxima list containing a file name followed by
;;; another Maxima list of the numbers of the test problems that are
;;; expected to fail, e.g.  ((mlist simp) "testfile.mac" ((mlist) 7 9 13)).
(defvar $testsuite_files
      `((mlist simp)
	((mlist simp) "rtest_rules"
	 #+allegro ((mlist simp) 11 13))
        "rtestnset" 
        ((mlist simp) "rtest1"
	 ;; 115 sometimes fails on ECL 16.1.2 on AMD 64/Ubuntu.
	 #+ecl ((mlist simp) 115 183 185 186)
	 #-ecl ((mlist simp) 183 185 186))
        ((mlist simp) "rtest1a" ((mlist simp) 33))
        ((mlist simp) "rtest2" ((mlist simp) 86 95))
	"rtest4"
        ;; Mark tests that require the documentation as known failures
        ;; if this was a lisp-only build
        ((mlist simp) "rtest5"
                 ,@(and (boundp '*autoconf-lisp-only-build*)
                        (symbol-value '*autoconf-lisp-only-build*)
                        (list* '(mlist simp) (list 78 80))))
	;; 46 = ECL bug #437: mishandling float format
	;; 45 sporadically fails in all tested ECL versions (15.3.7-16.1.3)
	;; 43 fails in ECL up to version 15.3.7
        ((mlist simp) "rtest6"
	 #+ecl ((mlist simp) 43 45 46))
        "rtest6a"
	"rtest6b"
	"rtest7"
        "rtest9" 
	((mlist simp) "rtest9a"
	 #+allegro ((mlist simp) 24 27 30 31 35 36 47 48 51 52 55 56 59 60 63 64 67 68 71 72))
        ((mlist simp) "rtest10" ((mlist simp) 24 25))
        ((mlist simp) "rtest11"
	 #+allegro ((mlist simp) 136 137 158 174)
	 #+ccl64 ((mlist simp) 158 174)
	 #+gcl ((mlist simp) 158 174 175))
        "rtest13"
	"rtest13s"
	;; ECL 16.1.2 still reliably fails in #307 + #310
	;; and sporadically in 201, 234, 249, 250, 251, 252, 267, 297, 298, 312, 315
	;; and 319
	;; ECL 13.5.1 sporadically fails in 233
	((mlist simp) "rtest14"
	 #+ecl ((mlist simp) 145 201 233 234 249 250 251 252 267 297 298 307 310 312 315 319))
        "rtest15"
	;; ccl versions 1.11 and earlier fail test 50.  Mark it as a
	;; known failure.  Presumably 1.12 will have this fixed.
	;; Test 561 tickles bug in ECL if signed zero enabled; see ECL
	;; bug #329. Fixed post-16.1.3.
	;; Test 50 still sometimes fails in ecl 16.1.2
        ((mlist simp) "rtest16"
	 #-(or ecl allegro) ((mlist simp) 524 525)
	 #+ecl ((mlist simp) 50 524 525 561)
	 #+allegro ((mlist simp) 50 241 524 525))
        "rtestode"
	"rtestode_zp"
        "rtest3"
	;; ECL 16.1.2 still fails in #104
	((mlist simp) "rtest8"
	 #+ecl ((mlist simp) 104))
        ((mlist simp) "rtest12"
	 ((mlist simp) 76 78))
        "rexamples"
        ((mlist simp) "rtesthyp"
	 ((mlist simp) 105 112 113 123 124 128))
        ((mlist simp) "rtest_hypgeo"
	 ((mlist simp) 143))
        "rtestmt19937"
        "rtest_allnummod"
        "rtestconjugate"
        ((mlist simp) "rtestsum"
	 ((mlist simp) 3 4 18 75))
	;; Tested with acl 10.1
	((mlist simp) "rtest_trig"
	 #+allegro ((mlist simp) 58))
        "rtest_zeta"
        "rtest_diff_invtrig"
        "rtest_scalarp"
        "rtest_everysome"
        ((mlist simp) "rtestint"
	 ((mlist simp) 232))
        "rtest_numth"
        "rtestifactor"
        ((mlist simp) "rtest_equal"
	 ((mlist simp) 157 160))
        "rtest_abs"
        ((mlist simp) "rtest_taylor"
	 ((mlist simp) 88 91 97 104 128 129))
        ((mlist simp) "rtest_dot")
        "rtest_mset"
        "rtest_boolean"
        "rtest_round"
        ((mlist simp) "rtest_map"
	 ((mlist simp) 2 3 4))
        ((mlist simp) "rtest_sign"
	 ((mlist simp) 21 25 30 40 65 72 79))
        "rtest_algebraic"
	;; Using the gcl version that is shipped with Ubuntu 16.04 (Long term Support)
	;; test 307 of rtest_gamma results in an error-catch on i386, but not in
	;; i64.
	;; Unfortunately many gcl versions have the same version number so we cannot
	;; test for the buggy version in order to get full support for the lts version.
	;;
	;; On ECL 15.3.7 (but not on ECL versions from 2014 or 2016) rtest_gamma
	;; most of the times crashes on ia32 and sometimes crashes on x64.
	((mlist simp) "rtest_gamma"
	 #+gcl ((mlist simp) 307)
	 #+allegro   ((mlist simp) 48 198 663 745))
        "rtest_expintegral"
        "rtest_signum"
        "rtest_lambert_w"
        ((mlist simp) "rtest_elliptic"
	 #-allegro ((mlist simp) 129 143)
	 #+allegro ((mlist simp) 92 129 143))
        "rtest_integrate"
        "rtest_integrate_special"
        ((mlist simp) "rtest_sqrt"
	 ((mlist simp) 89))
        ((mlist simp) "rtest_carg"
	 ((mlist simp) 40 41))
        ((mlist simp) "rtest_log")
        ((mlist simp) "rtest_power"
	 ((mlist simp) 19 20 26 58 65))
        "rtestdefstruct"
	;; Tested with acl 10.1
	((mlist simp) "rtest_limit"
	 #+allegro ((mlist simp) 184 185))
        "rtest_powerseries"
        ((mlist simp) "rtest_laplace"
	 ((mlist simp) 29 49 50 51 54 59 60 61 62 78 80))
        "rtest_plotoptions"
	"rtest_algsys"
        "rtest_trace"
	))

;; The list of share testsuite files. As they are given withut a path
;; this assumes that file_search_tests is set appropriately so that maxima
;; can actually find these files. (file_search_maxima is a good choice.)
(defvar $share_testsuite_files
  '((mlist simp)
    "rtestflatten"
    "rtest_z_transform"
    "rtest_zeilberger_extreme"
    "rtest_zeilberger"
    "rtest_boolsimp"
    "rtest_eigen"
    "rtest_lsquares"

    ;; These tests of diff eq code mostly fail.
    ;; Probably it's just a matter of loading the
    ;; appropriate package or packages for each test.

    ;; "rtest_ode1_abel"
    ;; "rtest_ode1_riccati"
    ;; "rtest_sym"
    ;; "rtest_sym2"
    ;; "rtestode_kamke_1_1"
    ;; "rtestode_kamke_1_2"
    ;; "rtestode_kamke_1_3"
    ;; "rtestode_kamke_1_4"
    ;; "rtestode_kamke_1_5"
    ;; "rtestode_kamke_1_6"
    ;; "rtestode_murphy_1_1"
    ;; "rtestode_murphy_1_2"
    ;; "rtestode_murphy_1_3"
    ;; "rtestode_murphy_1_4"
    ;; "rtestode_murphy_1_5"
    ;; "rtestode_murphy_1_6"
    ;; "rtestode_murphy_2_1"
    ;; "rtestode_murphy_2_2"
    ;; "rtestode_murphy_2_3"
    ;; "rtestode_murphy_2_4"
    ;; "rtestode_murphy_2_5"
    ;; "rtestode_kamke_1_1.mac"
    ;; "rtestode_kamke_1_2.mac"
    ;; "rtestode_kamke_1_3.mac"
    ;; "rtestode_kamke_1_4.mac" 
    ;; "rtestode_kamke_1_5.mac"
    ;; ((mlist simp) "rtestode_kamke_1_6.mac"
    ;;  ((mlist simp)  20))   ; OK - testsuite issues
    ;; "rtestode_kamke_2_1.mac"
    ;; ((mlist simp) "rtestode_kamke_2_2.mac"
    ;;  ((mlist simp) 132 )) ; OK - testsuite issues
    ;; ((mlist simp) "rtestode_kamke_2_3.mac"
    ;;  ((mlist simp) 83 107)) ; OK - testsuite issues
    ;; "rtestode_kamke_2_4.mac"
    ;; "rtestode_kamke_2_5.mac"
    ;; "rtest_sym.mac" 
    ;; "rtest_sym2.mac"
    ;; ((mlist simp) "rtest_ode1_riccati.mac"
    ;;  (mlist simp 138)) ; OK - testsuite issues
    ;; ((mlist simp) "rtest_ode1_abel.mac"
    ;;  (mlist simp 45)) ; OK - testsuite issues
    ;; "rtestode_odelin.mac"
    ;; "rtestode_utils.mac"

    "rtest_odelin"
    "rtestezunits"
    ;; ACL 10.1 cannot load stringproc as it has no (get-encoding) function.
    #-allegro
    "rtest_numericalio"
    ((mlist simp) "rtest_simplify_sum"
     ((mlist simp) 57))
    "rtest_solve_rec"
    ;; ACL 10.1 cannot load stringproc as it has no (get-encoding) function.
    #-allegro
    ((mlist simp) "rtest_stringproc"
     ;; 12 fails in ECL 15.3.7, 69 in ECL 13.5.1
     #+ecl ((mlist simp) 12 69)
     #+gcl ((mlist simp) 14))
    "rtest_opproperties"
    "rtest_stats"
    "rtest_distrib"
    ((mlist simp) "rtest_descriptive"
     ;; 86 and 97 fail in ECL 15.3.7
     #+(or gcl ecl)
     ((mlist simp) 86 97)
     ;; Tests that failed for ACL 10.1
     #+allegro
     ((mlist simp) 86 87 97 98))
    "rtest_interpol"
    ((mlist simp) "rtest_levin"
     ;; Tested with allegro 10.1
     #+allegro ((mlist simp) 71 75 77))
    "rtest_fractals"
    "rtest_bernstein"
    "rtest_atensor"
    "rtest_ctensor"
    "rtest_itensor"
    ;; On sbcl 1.4.10 we still get out-of-memory errors on many
    ;; computers on loading lapack => commented these tests out
    ;; for SBCL.    
    ;;
    ;;  The following functions were used but not defined: ODEPACK::DUMACH in gcl 2.6.12
    ;;  and abcl 1.5.0

    ;; Floating-point exception with ECL 16.1.2 in 4 7 8 9 10 12
    ;; On a different computer with ECL 16.1.2 test 5 failed, too. No idea why.
    ;; On another computer that used the same ECL version test step 15 resulted in an
    ;; error-catch without making further information visible in the log.
    ;; ECL 13.5.1 errored out in 7-12,14+15 and ECL 13.5.1 to 16.1.2
    ;; are known to sporadically error out in steps 6 and 13.
    ;;
    ;; But ecl 16.1.3 passes all of these tests
    #-sbcl
    ((mlist simp) "rtest_dgeqrf"
     #+ecl ((mlist simp) 1 4 5 6 7 8 9 10 11 12 13 14 15))
    #-sbcl
    ((mlist simp) "rtest_dgesv"
     #+ecl ((mlist simp) 1 3 4 5 6))
    ;;  The following functions were used but not defined: ODEPACK::DUMACH in gcl 2.6.12
    ;;  and abcl 1.5.0
    #-(or gcl abcl gcl)
    "rtest_dlsode"
    ((mlist simp) "rtest_fourier_elim"
     ((mlist simp) 146 147 148 149))
    ((mlist simp) "rtest_sequence"
     ((mlist simp) 55))
    "rtest_cholesky"
    "rtest_eigens_by_jacobi"
    "rtest_lu"
    "rtest_linalg"
    "rtest_polynomialp"
    "rtest_matrixexp"
    ((mlist simp) "rtest_romberg"
     ((mlist simp) 18 20))
    "rtest_wilcoxon"
    "rtest_bitwise"
    "rtest_gf"
    ((mlist simp) "rtest_namespaces"
     #+(or clisp sbcl ccl cmucl ecl)
     ((mlist simp) 7))
    "rtest_arag"
    ((mlist simp) "rtest_pdiff"
     #-(or ccl cmucl ecl sbcl gcl clisp)
     ((mlist simp) 62))
    ((mlist simp) "rtest_to_poly"
     #-(or ccl sbcl gcl ecl)
     ((mlist simp) 13 14 15 16 17 18 19 20 25))
;; Tested with acl 10.1
    ((mlist simp) "rtestprintf"
     #+allegro
     ((mlist simp) 1 2 3 4 5 6 7 8 9 10 11 12 13 14
      15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34
      35 37 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 57
      58 59 66 67 68)
     #+ecl
     ;; ECL 15.3.7 and earlier fails in 54, 66 and 70
     ((mlist simp) 38 54 61 63 65 66 69 70)
     #+clisp
     ((mlist simp) 27 38 61 63 65 69)
     #+gcl
     ((mlist simp) 7 29 38 39 40 47 48 61 69 70)
     ;; The tests that failed with abcl 1.5.0
     #+abcl
     ((mlist simp) 38 40 61 63 65 69)
     #+ccl
     ((mlist simp) 27 61)
     #+cmucl
     ((mlist simp) 61)
     #-(or clisp cmucl gcl ecl abcl ccl allegro)
     ((mlist simp) 38 61 63 65 69)
     )
    "rtest_simplex"
    ((mlist simp) "rtest_graphs"
     ;; Tested with acl 10.1
     #+allegro ((mlist simp) 3 4 5))
    ((mlist simp) "rtest_abs_integrate"
     #-(or cmucl ccl ecl sbcl gcl)
     ((mlist) 66 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 123 125 126 127 164 178)
     #+gcl
     ((mlist simp) 66 164)
     #+cmucl
     ((mlist simp) 66 164)
     #+sbcl
     ((mlist simp) 66 164)
     #+ecl
     ((mlist simp) 66 164)
     #+ccl
     ((mlist simp) 66 164))
    "rtest_pochhammer"
    ((mlist simp) "rtest_to_poly_solve"
     #-(or cmucl ccl ecl gcl sbcl sbcl)
     ((mlist simp) 64 74 80 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 55 70 72 73 76 77 78 83 86 87 88 89 90 96 97 102 116 120 124 125 126 127 128 129 130 131 133 134 135 136 137 138 139 140 141 142 143 144 146 147 148 149 157 158 159 162 163 164 165 166 167 168 169 173 179 180 181 183 184 196 197 198 199 200 201 202 203 204 207 208 210 214 215 216 217 222 233 234 240 241 242 243 244 245 253 262 263 265 268 273 274 277 278 282 283 284 285 286 287 288 289 290 291 292 293 295 296 299 300 311 312 318 319 320 322)
     #+sbcl
     ((mlist simp) 48 55 64 74 80 83 102 116 131 133 137 140 141 147 166 167 168 183 184 216 240 242 245 265 293 312 319 320 322)
     #+clisp
     ((mlist simp) 55 102 116 131 133 137 140 141 147 166 167 168 183 184 216
      240 242 245 265 293 312 319 320 322)
     #+gcl
     ((mlist simp) 64 74 80 48 55 83 102 116 131 133 137 140 141 147 149 166 167 168 183 184 212 216 240 242 245 265 293 312 319 320 322)
     #+cmucl
     ((mlist simp)
      48 55 64 74 80 83 102 116 131 133 137 140 141 147 166 167 168 183 184 199
      200 201 202 203 204 214 216 222 240 242 245 265 268 277 278 293 295 311
      312 319 320 322)
     #+ecl
     ((mlist simp)
      48 55 64 74 80 83 102 116 131 133 137 140 141 147 166 167 168 183 184 199
      200 201 202 203 204 214 216 222 240 242 245 265 268 277 278 293 295 311
      312 319 320 322)
     #+ccl
     ((mlist simp)
      48 55 64 74 80 83 102 116 131 133 137 140 141 147 166 167 168 183 184 216 240 242 245 265 293 312 319 320 322))
    ;; The tests that failed with abcl 1.5.0
    ((mlist simp) "rtest_hg"
     #+(or gcl abcl) ((mlist simp) 87 120)
     #-(or gcl abcl) ((mlist simp) 87))
    ((mlist simp) "rtest_sym"
     #-(or sbcl gcl clisp) ((mlist simp) 12 15 58 64)
     #+(or clisp gcl)  ((mlist simp) 15 64)
     #+sbcl ((mlist simp) 15))
    ((mlist simp) "rtest_nfloat"
     #-(or ecl gcl)
     ((mlist simp) 25))
    "rtest_mnewton"
    "rtest_solve_rat_ineq"
    ((mlist simp) "rtest_vect"
     ((mlist simp) 4 9 10 13 16 19 20 21 24 25))
    "rtest_antid"
    "rtest_bffac"
    "rtest_diff_form"
    "rtest_grobner"
    ((mlist simp) "rtest_finance"
     ;; Tested with acl 10.1
     ;; ECL newer than 15.3.7 doesn't fail any more
     #+(or gcl allegro ecl)
     ((mlist simp) 9 10 11))
    "rtest_fft"
    "rtest_rfft"
    ((mlist simp) "rtest_decfp"
     #+gcl ((mlist simp) 1 2 3 4))
    "rtest_wrstcse"
    ;; ACL 10.1 cannot load stringproc as it has no (get-encoding) function.
    #-(or ecl abcl)
    "rtest_draw"
    ((mlist simp) "rtest_engineering_format"
     #+sbcl
     ((mlist simp) 6)
     #+abcl
     ((mlist simp) 6)
     ;; ECL > 15.3.7 doesn't fail here any more
     #+ecl
     ((mlist simp) 6 8 10 12 14)
     #+ccl
     ((mlist simp) 6 8 10 12)
     ;; Tested with acl 10.1
     #+allegro
     ((mlist simp) 1 6 8 10 12 14))
    )
  )
