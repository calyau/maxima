;;; The Maxima test suite is defined to be the sum of all the tests
;;; in the files listed in *testsuite-files*.
;;; Each entry can either be a plain file name (minus extension), e.g.,
;;; "testfile.mac", or a list containing a file name followed by the numbers
;;; of the test problems that are expected to fail, e.g.
;;; ("testfile.mac" 7 9 13).

(setf *testsuite-files*
      '( "rtestode_murphy1.mac"
	 "rtestode_murphy2.mac"
         "rtestode_kamke_1_1.mac"
         "rtestode_kamke_1_2.mac"
         "rtestode_kamke_1_3.mac"
         "rtestode_kamke_1_4.mac" 
         "rtestode_kamke_1_5.mac"
         "rtestode_kamke_1_6.mac" 
	 "rtest_sym.mac" 
	 "rtest_sym2.mac"
	 "rtest_ode1_riccati.mac"
         "rtest_ode1_abel.mac"
	)
)
