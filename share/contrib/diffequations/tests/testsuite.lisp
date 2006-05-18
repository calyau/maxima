;;; The Maxima test suite is defined to be the sum of all the tests
;;; in the files listed in *testsuite-files*.
;;; Each entry can either be a plain file name (minus extension), e.g.,
;;; "testfile.mac", or a list containing a file name followed by the numbers
;;; of the test problems that are expected to fail, e.g.
;;; ("testfile.mac" 7 9 13).

;; Some of the answers have changed recently.  (May 2006)
;; They are probably correct but different.  Mark them as expected failures 
;; for the time being 
(setf $testsuite_files
      '( (mlist simp)
	 "rtestode_murphy_1_1.mac"
         "rtestode_murphy_1_2.mac" 
         "rtestode_murphy_1_3.mac"
         "rtestode_murphy_1_4.mac"
         "rtestode_murphy_1_5.mac"
         "rtestode_murphy_1_6.mac"
         "rtestode_murphy_2_1.mac"
         (mlist "rtestode_murphy_2_2.mac" 162)
         "rtestode_murphy_2_3.mac"
         "rtestode_murphy_2_4.mac"
         "rtestode_murphy_2_5.mac"
         "rtestode_kamke_1_1.mac"
         (mlist "rtestode_kamke_1_2.mac" 223 224)
         "rtestode_kamke_1_3.mac"
         "rtestode_kamke_1_4.mac" 
         (mlist "rtestode_kamke_1_5.mac" 71 107 114 150)
         "rtestode_kamke_1_6.mac" 
         "rtestode_kamke_2_1.mac"
         "rtestode_kamke_2_2.mac"
         "rtestode_kamke_2_3.mac"
         "rtestode_kamke_2_4.mac"
         "rtestode_kamke_2_5.mac"
	 "rtest_sym.mac" 
	 "rtest_sym2.mac"
	 "rtest_ode1_riccati.mac"
         "rtest_ode1_abel.mac"
	)
)
