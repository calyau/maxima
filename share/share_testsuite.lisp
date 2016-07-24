;; Like tests/testsuite.lisp, $share_test_files is the list of tests
;; from the share directory.  This assumes that file_search_tests is
;; set appropriately so that maxima can actually find these files.
;; (file_search_maxima is a good choice.)

(setf $share_testsuite_files
  '((mlist simp)
    "rtestflatten"
    "rtest_z_transform"
    "rtest_zeilberger_extreme"
    "rtest_zeilberger"
    "rtest_boolsimp"
    "rtest_eigen"

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
    ;; "rtestode_kamke_2_1"
    ;; "rtestode_kamke_2_2"
    ;; "rtestode_kamke_2_3"
    ;; "rtestode_kamke_2_4"
    ;; "rtestode_kamke_2_5"

    "rtest_odelin"
    "rtestezunits"
    "rtest_numericalio"
    ((mlist simp) "rtest_simplify_sum" 57)
    "rtest_solve_rec"
    ((mlist simp) "rteststringproc" 12 14)
    "rtest_opproperties"
    "rtest_stats"
    "rtest_distrib"
    "rtest_descriptive"
    "rtest_interpol"
    "rtest_levin"
    "rtest_fractals"
    "rtest_bernstein"
    "rtest_atensor"
    "rtest_ctensor"
    "rtest_itensor"
    ((mlist simp) "rtest_fourier_elim" 146 147 148 149)
    ((mlist simp) "rtest_sequence" 55)
    "test-cholesky"
    "test-eigens-by-jacobi"
    "test-lu"
    "test-linalg"
    "test-polynomialp"
    "test-matrixexp"
    ((mlist simp) "rtest_romberg" 18 20)
    "rtest_wilcoxon"
    "rtest_bitwise"
    "gf_test"
    "rtest_namespaces"
    "arag_test"
    "rtest_pdiff"
    "rtest_to_poly"
    ((mlist simp) "rtestprintf" 27 29 38 39 40 47 48 54)
    "rtest_simplex"
    "rtest_graphs"
    "rtest_abs_integrate"
    "rtest_pochhammer"
    ((mlist simp) "rtest_to_poly_solve" 64 74 80)
    "rtest_hg"
    "rtest_nfloat"
    "rtest_mnewton"
    "rtest_solve_rat_ineq"
    ((mlist simp) "rtest_vect" 4 9 10 13 16 19 20 21 24 25)
    "rtest_antid"
    "rtest_bffac"
    "rtest_grobner"
    "rtest_finance"
    "rtest_fft"
    "rtest_rfft"
    "rtest_decfp")
  )
