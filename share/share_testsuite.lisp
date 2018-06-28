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
#-(or gcl ecl)    "rtest_stringproc"
#+gcl    ((mlist simp) "rtest_stringproc" 14)
#+ecl    ((mlist simp) "rtest_stringproc" 12 69)
    "rtest_opproperties"
    "rtest_stats"
    "rtest_distrib"
#-(or gcl ecl)   "rtest_descriptive"
#+gcl    ((mlist simp) "rtest_descriptive" 86 97)
#+ecl    ((mlist simp) "rtest_descriptive" 86 97)
    "rtest_interpol"
    "rtest_levin"
    "rtest_fractals"
    "rtest_bernstein"
    "rtest_atensor"
    "rtest_ctensor"
    "rtest_itensor"
    ((mlist simp) "rtest_fourier_elim" 146 147 148 149)
    ((mlist simp) "rtest_sequence" 55)
    "rtest_cholesky"
    "rtest_eigens_by_jacobi"
    "rtest_lu"
    "rtest_linalg"
    "rtest_polynomialp"
    "rtest_matrixexp"
    ((mlist simp) "rtest_romberg" 18 20)
    "rtest_wilcoxon"
    "rtest_bitwise"
    "rtest_gf"
#+(or clisp sbcl ccl cmucl)    ((mlist simp) "rtest_namespaces" 7)
#-(or clisp sbcl ccl cmucl)    "rtest_namespaces"
    "rtest_arag"
#-(or ccl ecl) ((mlist simp) "rtest_pdiff" 62)
#+ccl "rtest_pdiff"
#+ecl ((mlist simp) "rtest_pdiff" 38 61 62 63 65 69)
    ((mlist simp) "rtest_to_poly" 13 14 15 16 17 18 19 20 25)
#+ecl               ((mlist simp) "rtestprintf" 38 54 61 63 65 66 69 70)
#+clisp             ((mlist simp) "rtestprintf" 27 38 61 63 65 69)
#+gcl               ((mlist simp) "rtestprintf" 7 29 38 39 40 47 48 61 63 65 69 70)
#+abcl              ((mlist simp) "rtestprintf" 40)
#+ccl              ((mlist simp) "rtestprintf" 27 61)
#-(or clisp gcl ecl abcl ccl)    ((mlist simp) "rtestprintf" 38 61 63 65 69)
    "rtest_simplex"
    "rtest_graphs"
    ((mlist simp) "rtest_abs_integrate" 66 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 123 125 126 127 164 178)
    "rtest_pochhammer"
#-gcl    ((mlist simp) "rtest_to_poly_solve" 64 74 80 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 55 70 72 73 76 77 78 83 86 87 88 89 90 96 97 102 116 120 124 125 126 127 128 129 130 131 133 134 135 136 137 138 139 140 141 142 143 144 146 147 148 149 157 158 159 162 163 164 165 166 167 168 169 173 179 180 181 183 184 196 197 198 199 200 201 202 203 204 207 208 210 214 215 216 217 222 233 234 240 241 242 243 244 245 253 262 263 265 268 273 274 277 278 282 283 284 285 286 287 288 289 290 291 292 293 295 296 299 300 311 312 318 319 320 322)
#+gcl    ((mlist simp) "rtest_to_poly_solve" 64 74 80 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 55 70 72 73 76 77 78 83 86 87 88 89 90 96 97 102 116 120 124 125 126 127 128 129 130 131 133 134 135 136 137 138 139 140 141 142 143 144 146 147 148 149 157 158 159 162 163 164 165 166 167 168 169 173 179 180 181 183 184 196 197 198 199 200 201 202 203 204 207 208 210 212 214 215 216 217 222 233 234 240 241 242 243 244 245 253 262 263 265 268 273 274 277 278 282 283 284 285 286 287 288 289 290 291 292 293 295 296 299 300 311 312 318 319 320 322)
#-(or gcl abcl)    ((mlist simp) "rtest_hg" 87)
#+gcl              ((mlist simp) "rtest_hg" 87 120)
#+abcl             ((mlist simp) "rtest_hg" 120)
    ((mlist simp) "rtest_nfloat" 25)
    "rtest_mnewton"
    "rtest_solve_rat_ineq"
    ((mlist simp) "rtest_vect" 4 9 10 13 16 19 20 21 24 25)
    "rtest_antid"
    "rtest_bffac"
    "rtest_grobner"
#-(or gcl ecl)    "rtest_finance"
#+gcl    ((mlist simp) "rtest_finance" 9 10 11)
#+ecl    ((mlist simp) "rtest_finance" 9 10 11)
    "rtest_fft"
    "rtest_rfft"
#-gcl    "rtest_decfp"
#+gcl    ((mlist simp) "rtest_decfp" 1 2 3 4)
    "rtest_wrstcse"
#-ecl    "rtest_draw"
#-(or sbcl ccl ecl abcl)    "rtest_engineering_format"
#+sbcl    ((mlist simp) "rtest_engineering_format" 6)
#+abcl    ((mlist simp) "rtest_engineering_format" 6)
#+ccl    ((mlist simp) "rtest_engineering_format" 6 8 10 12)
#+ecl    ((mlist simp) "rtest_engineering_format" 6 8 10 12 14)
    )
  )
