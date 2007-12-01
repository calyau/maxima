;;; The Maxima test suite is defined to be the sum of all the tests
;;; in the files listed in *testsuite-files*.
;;; Each entry can either be a plain file name (minus extension), e.g.,
;;; "testfile.mac", or a list containing a file name followed by the numbers
;;; of the test problems that are expected to fail, e.g.
;;; ("testfile.mac" 7 9 13).

;; The expected failures are due to testsuite issues.  The "wrong" answers 
;; are correct but I can't get the testsuite to agree.
(setf $testsuite_files
      '( (mlist simp)
	 (mlist "rtestode_murphy_1_1.mac" 100)  ; OK - testsuite issues
         "rtestode_murphy_1_2.mac" 
         "rtestode_murphy_1_3.mac"
         "rtestode_murphy_1_4.mac"
         "rtestode_murphy_1_5.mac"
         "rtestode_murphy_1_6.mac"
         (mlist "rtestode_murphy_2_1.mac" 141 143) ; OK - testsuite issues
         "rtestode_murphy_2_2.mac"
         "rtestode_murphy_2_3.mac"
         "rtestode_murphy_2_4.mac"
         "rtestode_murphy_2_5.mac"
         "rtestode_kamke_1_1.mac"
         (mlist "rtestode_kamke_1_2.mac" 9)  ; OK - testsuite issues
         (mlist "rtestode_kamke_1_3.mac" 2)  ; OK - testsuite issues
         "rtestode_kamke_1_4.mac" 
         "rtestode_kamke_1_5.mac"
         (mlist "rtestode_kamke_1_6.mac" 20) ; OK - testsuite issues
         (mlist "rtestode_kamke_2_1.mac" 80 107) ; OK - testsuite issues
         (mlist "rtestode_kamke_2_2.mac" 8 10 34 38 40 87 134 )
         (mlist "rtestode_kamke_2_3.mac" 8 11 21)
         "rtestode_kamke_2_4.mac"
         "rtestode_kamke_2_5.mac"
	 "rtest_sym.mac" 
	 "rtest_sym2.mac"
	 "rtest_ode1_riccati.mac"
         "rtest_ode1_abel.mac"
         "rtestode_odelin.mac"
	)
)
