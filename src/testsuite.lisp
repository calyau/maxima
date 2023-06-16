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
(defparameter $testsuite_files
      `((mlist simp)
        "rtest_sqdnst"
        "rtest_extensions"
	((mlist simp) "rtest_rules"
	 #+allegro ((mlist simp) 11 13))
        "rtestnset" 
        ;; ACL 10.1 cannot load stringproc as it has no (get-encoding) function.
        #-allegro
        ((mlist simp) "rtest1"
	 ((mlist simp) 183 185 186))
        ((mlist simp) "rtest1a" ((mlist simp) 33))
        ((mlist simp) "rtest2" ((mlist simp) 86 95))
	"rtest4"
        ;; Mark tests that require the documentation as known failures
        ;; if this was a lisp-only build
        ((mlist simp) "rtest5"
                 ,@(and (boundp '*autoconf-lisp-only-build*)
                        (symbol-value '*autoconf-lisp-only-build*)
                        (list (list '(mlist simp) 80))))
        "rtest6"
        "rtest6a"
	"rtest6b"
	"rtest7"
        "rtest9" 
	((mlist simp) "rtest9a"
	 #+allegro ((mlist simp) 24 27 30 31 35 36 47 48 51 52 55 56 59 60 63 64 67 68 71 72))
        ((mlist simp) "rtest10" ((mlist simp) 24 25))
        ((mlist simp) "rtest11"
	 #+allegro ((mlist simp) 136 137 158 174)
	 #+ccl64 ((mlist simp) 158 174))
        "rtest13"
	"rtest13s"
	;; ECL 16.1.2 still reliably fails in #307 + #310
	;; and sporadically in 201, 234, 249, 250, 251, 252, 267, 297, 298, 312, 315
	;; and 319
	;; ECL 13.5.1 sporadically fails in 233
	((mlist simp) "rtest14")
        "rtest15"
	;; ccl versions 1.11 and earlier fail test 50.  Mark it as a
	;; known failure.  Presumably 1.12 will have this fixed.
	;; Test 561 tickles bug in ECL if signed zero enabled; see ECL
	;; bug #329. Fixed post-16.1.3.
	;; Test 50 still sometimes fails in ecl 16.1.2
        ((mlist simp) "rtest16"
	 #-(or ecl allegro) ((mlist simp) 525 526)
	 #+ecl ((mlist simp) 525 526)
	 #+allegro ((mlist simp) 50 242 525 526))
        "rtestode"
	"rtestode_zp"
        ((mlist simp) "rtest3" ((mlist simp) 146))
	;; ECL 16.1.2 still fails in #104
	((mlist simp) "rtest8")
        "rtest12"
        "rexamples"
        ((mlist simp) "rtesthyp"
	 ((mlist simp) 105 112 113 123 124 128))
        ((mlist simp) "rtest_hypgeo"
	 ((mlist simp) 143))
        "rtestmt19937"
        "rtest_allnummod"
        ((mlist simp) "rtest_maxmin" 
                ((mlist simp) 16 17 40 52 53 57 97 109))
        "rtestconjugate"
        ((mlist simp) "rtestsum"
	 ((mlist simp) 23 24 38 95))
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
	;; Using the gcl version 2.6.14 the tests pass.
	;;
	;; On ECL 15.3.7 (but not on ECL versions from 2014 or 2016) rtest_gamma
	;; most of the times crashes on ia32 and sometimes crashes on x64.
	((mlist simp) "rtest_gamma"
	 #+allegro   ((mlist simp) 48 198 663 745))
        "rtest_expintegral"
        "rtest_signum"
        "rtest_lambert_w"
        ((mlist simp) "rtest_elliptic"
	 #-allegro ((mlist simp) 135 149)
	 #+allegro ((mlist simp) 92 135 149))
        "rtest_integrate"
        "rtest_integrate_special"
        ((mlist simp) "rtest_sqrt"
	 ((mlist simp) 89))
        ((mlist simp) "rtest_carg"
	 ((mlist simp) 40 41))
        ((mlist simp) "rtest_log")
        ((mlist simp) "rtest_power"
	 ((mlist simp) 19 20 26))
        "rtestdefstruct"
	;; Tested with acl 10.1
	((mlist simp) "rtest_limit"
	 #+allegro ((mlist simp) 184 185))
        "rtest_powerseries"
        ((mlist simp) "rtest_laplace"
	 ((mlist simp) 29 49 50 51 59 60 61 62 78 80))
        "rtest_plotoptions"
	"rtest_algsys"
        "rtest_trace"
	"rtest_polynomialp"
        ((mlist simp) "rtest_limit_extra" 
          ((mlist simp)  42 52 53 59 61 64 67 80 82 83 84 89 
                         94 96 98 102 104 106 108 111
                         124 125 126 127 132 133 135 136 137
                         224 226 227 228 230 231 232 234 238 
                         239 240 241 242 243 244 245 246 249
                         256 259 261 262 263 267 268 269 270 271 272 277))
        "rtest_gcd"
	;; The tests that failed with abcl 1.5.0
	((mlist simp) "rtest_hg"
	 #+(or gcl abcl) ((mlist simp) 87 120)
	 #-(or gcl abcl) ((mlist simp) 87))
	((mlist simp) "rtest_nfloat"
	 #-gcl((mlist simp) 25))
	((mlist simp) "rtest_ilt")
	((mlist simp) "ulp_tests"
	 ;; Clisp doesn't have denormals
	 #+clisp
	 ((mlist simp) 10 42 49))))

;; The list of share testsuite files. As they are given without a path
;; this assumes that file_search_tests is set appropriately so that maxima
;; can actually find these files. (file_search_maxima is a good choice.)
(defparameter $share_testsuite_files
  '((mlist simp)
    "rtest_facexp"
    "rtest_orthopoly"
    "rtest_pslq"
    "rtestflatten"
    "rtest_z_transform"
    "rtest_zeilberger_extreme"
    "rtest_zeilberger"
    "rtest_boolsimp"
    "rtest_eigen"
    "rtest_lsquares"
    "rtest_pytranslate"

    ;; Omit tests in share/contrib/diffequations/tests.
    ;; To run those tests: load ("setup_tests.mac");
    ;; and then: run_testsuite ();
    ;; (setup_tests assigns the list of ODE tests to testsuite_files)

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
    ((mlist simp) "rtest_stringproc")
    #-allegro
    ((mlist simp) "rtest_md5sum")
    "rtest_opproperties"
    "rtest_stats"
    "rtest_distrib"
    ((mlist simp) "rtest_descriptive"
     ;; Tests that failed for ACL 10.1
     #+allegro
     ((mlist simp) 98 99 109 110))
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

    #-sbcl
    ((mlist simp) "rtest_dgeqrf")
    #-sbcl
    ((mlist simp) "rtest_dgesv")
    ;;  The following functions were used but not defined: ODEPACK::DUMACH in gcl 2.6.12
    ;;  and abcl 1.5.0
    #-abcl
    "rtest_dlsode"
    ((mlist simp) "rtest_fourier_elim"
     ((mlist simp) 146 147 148 149))
    ((mlist simp) "rtest_sequence"
     ((mlist simp) 55))
    "rtest_cholesky"
    "rtest_eigens_by_jacobi"
    "rtest_lu"
    "rtest_linalg"
    "rtest_matrixexp"
    ((mlist simp) "rtest_romberg"
     ((mlist simp) 18 20))
    "rtest_wilcoxon"
    "rtest_bitwise"
    "rtest_gf"
    "rtest_arag"
    ((mlist simp) "rtest_pdiff"
     #-(or ccl cmucl ecl sbcl gcl clisp)
     ((mlist simp) 62))
    ((mlist simp) "rtest_to_poly"
     #-(or abcl ccl cmucl sbcl gcl ecl clisp)
     ((mlist simp) 13 14 15 16 17 18 19 20 25))
;; Tested with acl 10.1
    ((mlist simp) "rtestprintf"
     #+allegro
     ((mlist simp) 1 2 3 4 5 6 7 8 9 10 11 12 13 14
      15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34
      35 37 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 57
      58 59 66 67 68)
     #+ecl
     ;; ECL 22.2.1 results
     ((mlist simp) 61 63 65 69)
     #+clisp
     ((mlist simp) 27 38 61 63 65 69)
     #+gcl
     ((mlist simp) 7 29 38 39 40 48 61)
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
    ((mlist simp) "rtest_abs_integrate" ((mlist) 177 253))
    "rtest_pochhammer"
    ((mlist simp) "rtest_to_poly_solve"
     #+gcl ((mlist simp) 64 74 80 116 140 141 168 184 212 242 245 322)
     #-(or gcl) ((mlist simp) 64 74 80 116 140 141 168 184 242 245 322)
     )
    ((mlist simp) "rtest_sym"
     #-(or sbcl gcl clisp cmucl ecl) ((mlist simp) 15 64)
     #+clisp  ((mlist simp) 15 64)
     #+sbcl ((mlist simp) 15 64))
    "rtest_mnewton"
    "rtest_solve_rat_ineq"
    ((mlist simp) "rtest_vect"
     #-(or cmucl gcl ecl)
     ((mlist simp) 4 9 10 13 16 19 20 21 24 25)
     #+(or cmucl ecl gcl)
     ((mlist simp) 4 9 10 13 16 20 21 24 25))
     "rtest_antid"
     "rtest_bffac"
     "rtest_diff_form"
     "rtest_grobner"
     ((mlist simp) "rtest_finance"
      ;; Tested with acl 10.1
      #+allegro
      ((mlist simp) 9 10 11))
     "rtest_fft"
     "rtest_rfft"
     "rtest_decfp"
     "rtest_wrstcse"
     ;; ACL 10.1 cannot load stringproc as it has no (get-encoding) function.
     #-(or ecl abcl)
     "rtest_draw"
     ((mlist simp) "rtest_engineering_format"
      #+abcl
      ((mlist simp) 6)
      #+ccl
      ((mlist simp) 6 8 10 12)
      ;; Tested with acl 10.1
      #+allegro
      ((mlist simp) 1 6 8 10 12 14))
     )
    )
