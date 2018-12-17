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
     #-(or ccl cmucl ecl sbcl)
     ((mlist simp) 62))
    ((mlist simp) "rtest_to_poly"
     #-ccl
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
     ((mlist simp) 7 29 38 39 40 47 48 61 63 65 69 70)
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
     #-(or cmucl ccl ecl)
     ((mlist) 66 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 123 125 126 127 164 178)
     #+cmucl
     ((mlist simp) 66 164)
     #+ecl
     ((mlist simp) 66 164)
     #+ccl
     ((mlist simp) 66 164))
    "rtest_pochhammer"
    ((mlist simp) "rtest_to_poly_solve"
     #-(or cmucl ccl ecl gcl)
     ((mlist simp) 64 74 80 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 55 70 72 73 76 77 78 83 86 87 88 89 90 96 97 102 116 120 124 125 126 127 128 129 130 131 133 134 135 136 137 138 139 140 141 142 143 144 146 147 148 149 157 158 159 162 163 164 165 166 167 168 169 173 179 180 181 183 184 196 197 198 199 200 201 202 203 204 207 208 210 214 215 216 217 222 233 234 240 241 242 243 244 245 253 262 263 265 268 273 274 277 278 282 283 284 285 286 287 288 289 290 291 292 293 295 296 299 300 311 312 318 319 320 322)
     #+gcl
     ((mlist simp) 64 74 80 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 55 70 72 73 76 77 78 83 86 87 88 89 90 96 97 102 116 120 124 125 126 127 128 129 130 131 133 134 135 136 137 138 139 140 141 142 143 144 146 147 148 149 157 158 159 162 163 164 165 166 167 168 169 173 179 180 181 183 184 196 197 198 199 200 201 202 203 204 207 208 210 212 214 215 216 217 222 233 234 240 241 242 243 244 245 253 262 263 265 268 273 274 277 278 282 283 284 285 286 287 288 289 290 291 292 293 295 296 299 300 311 312 318 319 320 322)
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
    ((mlist simp) "symtest.mac"
     ((mlist simp) 12 58 64))

    ((mlist simp) "rtest_nfloat"
     #-ecl
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
