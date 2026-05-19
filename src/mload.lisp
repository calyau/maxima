;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancements.                   ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       (c) Copyright 1982 Massachusetts Institute of Technology       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :maxima)
(macsyma-module mload)

(defun load-and-tell (filename)
  (loadfile filename t ;; means this is a lisp-level call, not user-level.
	    $loadprint))
       
(defun errset-namestring (x)
  (let ((errset nil))
    (errset (pathname x))))

(defun get-dirs (path &aux (ns (namestring path)))
  "Find all the directories in the directory given by PATH.  We do not
  descend into the subdirectories."
  ;; Basically get a directory listing from "<path>/*/".  The last
  ;; slash tells DIRECTORY to return just the directories in PATH.
  (directory (concatenate 'string
                          ns
                          (if (eql #\/ (char ns (1- (length ns)))) "" "/")
                          "*"
                          #+(or :clisp :sbcl :ecl :openmcl :gcl :cmucl) "/")
             #+openmcl :directories #+openmcl t))

(defun unix-like-basename (path)
  (let* ((pathstring (namestring path))
	 (len (length pathstring)))
    (when (equal (subseq pathstring (- len 1) len) "/")
      (decf len)
      (setf pathstring (subseq pathstring 0 len)))
    (subseq pathstring (1+ (or (position #\/ pathstring :from-end t)
			       (position #\\ pathstring :from-end t) -1)) len)))

(defun unix-like-dirname (path)
  "A Lisp equivalent of the Unix program dirname.  This strips the last
  component from the given PATH.  The last non-slash component and
  trailing slashes are removed.  In particular for \"a/b/c/\", this
  returns \"a/b\", not \"a/b/c/\"."
  (let* ((pathstring (namestring path))
	 (len (length pathstring)))
    (when (equal (subseq pathstring (- len 1) len) "/")
      (decf len)
      (setf pathstring (subseq pathstring 0 len)))
    (let ((last-slash (or (position #\/ pathstring :from-end t)
			  (position #\\ pathstring :from-end t))))
      (if last-slash
          (subseq pathstring 0 last-slash)
          "."))))

(defmfun $filename_merge (&rest file-specs)
  (when (or (null file-specs) (cddr file-specs))
    (wna-err '$filename_merge))
  (setq file-specs (mapcar #'macsyma-namestring-sub file-specs))
  (pathname (if (null (cdr file-specs))
                (car file-specs)
                (merge-pathnames (cadr file-specs) (car file-specs)))))

(defun macsyma-namestring-sub (user-object)
  (if (pathnamep user-object) user-object
      (let* ((system-object
	      (cond ((and (atom user-object) (not (symbolp user-object)))
		     user-object)
		    ((atom user-object)	;hence a symbol in view of the
		     (print-invert-case (fullstrip1 user-object))) ; first clause
		    (($listp user-object)
		     (fullstrip (cdr user-object)))
		    (t
		     (merror (intl:gettext "filename_merge: unexpected argument: ~M") user-object))))
	     (namestring-try (errset-namestring system-object)))
	(if namestring-try (car namestring-try)
	    ;; know its small now, so print on same line.
	    (merror (intl:gettext "filename_merge: unexpected argument: ~:M") user-object)))))

;; Returns the truename corresponding to a stream, or nil (for non-file streams).
;; Previously we used (subtypep (type-of stream) 'file-stream) to determine whether
;; a stream is a file stream, but this doesn't work on GCL.
(defun get-stream-truename (stream)
  (handler-case
    (probe-file stream)
    (error () nil)))

(defmfun $batchload (filename-or-stream &aux (*mread-prompt* ""))
  (declare (special *mread-prompt*))
  (if (streamp filename-or-stream)
      (batchload-stream filename-or-stream)
    (let
      ((filename ($file_search1 filename-or-stream '((mlist) $file_search_maxima))))
      (with-open-file (in-stream filename)
        (batchload-stream in-stream)))))

(defun batchload-stream (in-stream &key autoloading-p)
  (let ($load_pathname)
    (let*
      ((noevalargs nil)
       (*read-base* 10.)
       (stream-truename (get-stream-truename in-stream))
       (in-stream-string-rep
        (if stream-truename
          (setq $load_pathname (cl:namestring stream-truename))
          (format nil "~A" in-stream)))
       ;; If we arrived here from autoloading, call MEVAL instead of MEVAL*
       ;; since MEVAL* is intended to be called from the interpreter top level;
       ;; MEVAL* modifies global state, resetting VARLIST and calling CLEARSIGN.
       (meval-fcn (symbol-function (if autoloading-p 'meval 'meval*)))
       (expr nil))
      (declare (special *prompt-on-read-hang*))
      (when (and $loadprint (not *maxima-quiet*))
        (format t (intl:gettext "~&read and interpret ~A~&") in-stream-string-rep))
      (cleanup)
      (newline in-stream)
      (loop while (and
                    (setq  expr (let (*prompt-on-read-hang*) (mread in-stream nil)))
                    (consp expr))
            do (funcall meval-fcn (third expr)))
      in-stream-string-rep)))

;;returns appropriate error or existing pathname.
;; the second argument is a maxima list of variables
;; each of which contains a list of paths.   This is
;; so users can correct the variable..
(defmfun $file_search1 (name search-lists &aux lis)
  (if (pathnamep name)
      (setq name (namestring name)))
  (setq lis (apply '$append (mapcar 'symbol-value (cdr search-lists))))
  (let ((res ($file_search name lis)))
    (or res
	(merror (intl:gettext "file_search1: ~M not found in ~A.")
		name
		(string-trim "[]" ($sconcat search-lists))))))

(defmfun $load (filename)
  "This is the generic file loading function.
  LOAD(filename) will either BATCHLOAD or LOADFILE the file,
  depending on whether the file contains Macsyma, Lisp, or Compiled
  code. The file specifications default such that a compiled file
  is searched for first, then a lisp file, and finally a macsyma batch
  file. This command is designed to provide maximum utility and
  convenience for writers of packages and users of the macsyma->lisp
  translator."

  (if (or (stringp filename) (symbolp filename) (pathnamep filename))
    (let ((searched-for
  	 ($file_search1 filename
  			'((mlist) $file_search_maxima $file_search_lisp  )))
  	type)
      (setq type ($file_type searched-for))
      (case type
        (($maxima)
         ($batchload searched-for))
        (($lisp $object)
         ;; do something about handling errors
         ;; during loading. Foobar fail act errors.
         (load-and-tell searched-for))
        (t
         ;; UNREACHABLE MESSAGE: DEFAULT TYPE IS '$OBJECT (SEE $FILE_TYPE BELOW)
         (merror "Maxima bug: Unknown file type ~M" type)))
      searched-for)
    (merror "load: argument must be a string, symbol, or pathname; found: ~M" filename)))

(defmvar $file_type_lisp
    (list '(mlist) "l" "lsp" "lisp"))

(defmvar $file_type_maxima
    (list '(mlist) "mac" "mc" "demo" "dem" "dm1" "dm2" "dm3" "dmt" "wxm"))

(defmfun $file_type (fil)
  (let ((typ ($pathname_type fil)))
    (cond
      ((member typ (cdr $file_type_lisp) :test #'string=)
       '$lisp)
      ((member typ (cdr $file_type_maxima) :test #'string=)
       '$maxima)
      (t
       '$object))))

(defmfun $pathname_directory (path)
  (let ((pathname (pathname path)))
    (namestring (make-pathname :directory (pathname-directory pathname)))))

(defmfun $pathname_name (path)
  (let ((pathname (pathname path)))
    (pathname-name pathname)))

(defmfun $pathname_type (path)
  (let ((pathname (pathname path)))
    (pathname-type pathname)))
  

;; Following GENERIC-AUTOLOAD is copied from orthopoly/orthopoly-init.lisp.
;; Previous version didn't take Clisp, CMUCL, or SBCL into account.
;; Moved here from suprv1 (along with $setup_autoload and $directory)
;; so that mload no longer has a back-edge from suprv1.  The defvar
;; *autoloaded-files* stays in suprv1 because suprv1's kill code
;; mutates it; suprv1 also still owns (defvar autoload
;; 'generic-autoload) since its load-function calls (funcall autoload
;; ...).

(defun generic-autoload (file &aux type)
  (unless (member file *autoloaded-files* :test #'equal)
    (push file *autoloaded-files*)
    (setq file (pathname (cdr file)))
    (setq type (pathname-type file))
    (let ((bin-ext #+gcl "o"
		   #+cmu (c::backend-fasl-file-type c::*target-backend*)
		   #+clisp "fas"
		   #+allegro "fasl"
		   #+openmcl (pathname-type ccl::*.fasl-pathname*)
		   #+lispworks (pathname-type (compile-file-pathname "foo.lisp"))
		   #-(or gcl cmu clisp allegro openmcl lispworks) ""))
      (if (member type (list bin-ext "lisp" "lsp")  :test 'equalp)
	  (let ((*read-base* 10.) (*print-base* 10.)) #-sbcl (load file) #+sbcl (with-compilation-unit nil (load file)))
	  ($load file)))))

(defmfun $directory (path)
  (cons '(mlist) (mapcar 'namestring (directory ($filename_merge path)))))

(defmfun $setup_autoload (filename &rest functions)
  (let ((file ($file_search filename)))
    (dolist (func functions)
      (nonsymchk func '$setup_autoload)
      (putprop (setq func ($verbify func)) file 'autoload)
      (add2lnc func $props)))
  '$done)


(defvar *macsyma-startup-queue* nil)

(declaim (special *mread-prompt*))

;;;; batch & demo search hacks

(defmfun $batch (filename-or-stream &optional (demo :batch)
               &aux tem   (possible '(:demo :batch :test)))
  "giving a second argument makes it use demo mode, ie pause after evaluation
   of each command line"
  ;; Try to get rid of testsuite failures on machines that are low on RAM.
  ($garbage_collect)
  (cond
    ((setq tem (member ($mkey demo) possible :test #'eq))
     (setq demo (car tem)))
    (t (format t (intl:gettext "batch: second argument must be 'demo', 'batch' or 'test'; found: ~A, assumed 'batch'~%") demo)))
  (if (streamp filename-or-stream)
    (batch-stream filename-or-stream demo)
    (let
      ((filename ($file_search1 filename-or-stream
                                (if (eql demo :demo)
                                  '((mlist) $file_search_demo )
                                  '((mlist) $file_search_maxima)))))
      (cond
        ((eq demo :test)
         (test-batch filename nil :show-all t))
        (t
          (with-open-file (in-stream filename)
            (batch-stream in-stream demo)))))))

(defmfun $demo (filename)
  (let ((tem ($file_search filename $file_search_demo)))
    (or tem (merror (intl:gettext "demo: could not find ~M in ~M.")
		    filename '$file_search_demo))
    ($batch tem	'$demo)))

(defun batch-stream (in-stream demo)
  (declare (special $batch_answers_from_file))
  (let ($load_pathname)
    (let*
      ((*read-base* 10.)
      (stream-truename (get-stream-truename in-stream))
       (in-stream-string-rep
        (if stream-truename
          (setq $load_pathname (cl:namestring stream-truename))
          (format nil "~A" in-stream)))
       (*query-io* (if $batch_answers_from_file
		       (make-two-way-stream in-stream (make-string-output-stream)) *query-io*))
       (*standard-input* (if $batch_answers_from_file
			     in-stream *standard-input*)))
      (when (not *maxima-quiet*)
        (format t (intl:gettext "~%read and interpret ~A~%") in-stream-string-rep))
      (catch 'macsyma-quit (continue :stream in-stream :batch-or-demo-flag demo))
      (incf $linenum)
      in-stream-string-rep)))

;; When either a or b are special float values (NaN or +/-Inf), return true iff
;; a and b are the both NaN or both +Inf or both -Inf.
;; Note that float_approx_equal(NaN, NaN) returns true.
;;
;; When a and b to finite, nonzero floats, return true iff
;;
;;   |a - b| <= float_approx_equal_tolerance * min(2^n, 2^m)
;;
;; where a = af * 2^m, |af| < 1, and m is an integer (similarly for b). 
;; See Knuth, "The Art of Computer Programming" (3rd ed.), Vol. 2, Sec. 4.2.2, Eq. 24, page 233.
;;
;; Note that Eq. 24 isn't well-defined for a or b equal to zero.
;; To make progress, let's consider the limit as b --> 0,
;; therefore n --> -inf. For any finite a, that means that min(2^n, 2^m) = 2^n
;; and therefore |a - b| --> |a|, but to pass the test,
;; |a| must be less than or equal to float_approx_equal_tolerance * 2^n --> 0.
;; Therefore the test fails for all nonzero, finite a, when b = 0.
;; i.e. when either a or b is zero but not both, return false.
;;
;; Note also that if a < 0 and b > 0 or vice versa, the test must fail:
;; without loss of generality assume |a| > |b|. Then n <= m and min(2^n, 2^m) = 2^n.
;; Now |a - b| = |a| + |b| since a and b are different signs.
;; Then |a - b| / min(2^n, 2^m) = |a/min(2^n, 2^m)| + |b/min(2^n, 2^m)| = |af|*2^m/2^n + |bf| >= |af| + |bf| > 1.
;; So unless float_approx_equal_tolerance is unusually large, the test must fail.
;;
;; Otherwise, either a or b is not a float, so return false.

(defmvar $float_approx_equal_tolerance (* 8 +flonum-epsilon+))

(defmfun $float_approx_equal (a b)
  (setq a (if (floatp a) a ($float a)))
  (setq b (if (floatp b) b ($float b)))
  (and
   (floatp a)
   (floatp b)
   (cond
     ;; look for NaN
     ((/= a a) (/= b b))
     ((/= b b) nil)
     ;; look for Inf
     ((> (abs a) most-positive-double-float) (= a b))
     ((> (abs b) most-positive-double-float) nil)
     ;; look for zero
     ((= a 0d0) (= b 0d0))
     ((= b 0d0) nil)
     ;; look for A = B
     ((= a b))
     (t
      ;; Implement test without involving floating-point arithmetic,
      ;; to avoid errors which could occur with extreme values.
      (multiple-value-bind (a-significand a-expt a-sign)
          (decode-float a)
        (declare (ignore a-significand))
        (multiple-value-bind (b-significand b-expt b-sign)
            (decode-float b)
          (declare (ignore b-significand))
          (when (or (= a-sign b-sign)
                    (>= $float_approx_equal_tolerance 1d0))
            (multiple-value-bind (a-b-significand a-b-expt)
                (integer-decode-float (abs (- a b)))
              (multiple-value-bind (tol-significand tol-expt)
                  (integer-decode-float $float_approx_equal_tolerance)
                (or (< a-b-expt (+ tol-expt (min a-expt b-expt)))
                    (and (= a-b-expt (+ tol-expt (min a-expt b-expt)))
                         (<= a-b-significand tol-significand))))))))))))

;; Big float version of float_approx_equal. But for bfloat_approx_equal, the tolerance isn't
;; user settable; instead, it is 32 / 2^fpprec. The factor of 32 is too large, I suppose. But
;; the test suite gives a few errors with a factor of 16. These errors might be due to 
;; float / big float comparisons.

(defmfun $bfloat_approx_equal (a b)
  (setq a (if ($bfloatp a) a ($bfloat a)))
  (setq b (if ($bfloatp b) b ($bfloat b)))
  (let ((m) (bits))
    (and
     ($bfloatp a)
     ($bfloatp b)
     (setq bits (min (third (first a)) (third (first b))))
     (setq m (* 32 (expt 2 (- bits)) (min (expt 2 (- (car (last a)) 1)) (expt 2 (- (car (last b)) 1)))))
     (setq m (if (rationalp m) (div (numerator m) (denominator m)) m))
     (eq t (mgqp m (take '(mabs) (sub a b)))))))


;; The first argument 'f' is the expected result; the second argument 
;; 'g' is the output of the test. By explicit evaluation, the expected 
;; result *can* be a CL array, CL hashtable, or a taylor polynomial. Such
;; a test would look something like (yes, it's a silly test)

;;    taylor(x,x,0,2);
;;    ''(taylor(x,x,0,2)
 
(defun approx-alike (f g)
 
  (cond ((floatp f) (and (floatp g) ($float_approx_equal f g)))
	
	(($bfloatp f) (and ($bfloatp g) ($bfloat_approx_equal f g)))
	
	(($taylorp g)
	 (approx-alike 0 (sub (ratdisrep f) (ratdisrep g))))
	
	((stringp f)
	 (and (stringp g) (string= f g)))

	((arrayp f)
	 (and (arrayp g)
          (equal (array-dimensions f) (array-dimensions g))
          (approx-alike ($listarray f) ($listarray g))))

	((hash-table-p f)
	 (and (hash-table-p g) (approx-alike ($listarray f) ($listarray g))))

	((atom f)
	 (and (atom g) (equal f g)))
		     
	((op-equalp f 'lambda)
	 (and (op-equalp g 'lambda)
	      (approx-alike-list (mapcar #'(lambda (s) (simplifya s nil)) (margs f))
				 (mapcar #'(lambda (s) (simplifya s nil)) (margs g)))))
	
	(($ratp f)
	 (and ($ratp g) (approx-alike (ratdisrep f) (ratdisrep g))))
	
	;; maybe we don't want this.
	((op-equalp f 'mquote)
	 (approx-alike (second f) g))
	 
	;; I'm pretty sure that (mop f) and (mop g) won't signal errors, but
	;; let's be extra careful.

	((and (consp f) (consp (car f)) (consp g) (consp (car g))
	      (or (approx-alike (mop f) (mop g)) 
		  (and (symbolp (mop f)) (symbolp (mop g))
		       (approx-alike ($nounify (mop f)) ($nounify (mop g)))))
	      (eq ($subvarp f) ($subvarp g))
	      (approx-alike-list (margs f) (margs g))))
	
	(t nil)))

(defun approx-alike-list (p q)
  (cond ((null p) (null q))
	((null q) (null p))
	(t (and (approx-alike (first p) (first q)) (approx-alike-list (rest p) (rest q))))))

(defun simple-equal-p (f g)
  (approx-alike (simplifya f nil) (simplifya g nil)))

(defun batch-equal-check (expected result)
  (let ((answer (catch 'macsyma-quit (simple-equal-p expected result))))
    (if (eql answer 'maxima-error) nil answer)))

(defvar *collect-errors* t)

(defun print-test-batch-problem (out filename problem-no problem-lineno input)
  (let* ((*print-base* 10)
         (center
          (format nil (intl:gettext " ~A: Problem ~A~A ")
            filename problem-no
            (if problem-lineno (format nil (intl:gettext " (line ~S)") problem-lineno) "")))
         (stars-count (- $linel (length center)))
         (stars-left (make-string (max 10 (ceiling stars-count 2)) :initial-element #\*))
         (stars-right (make-string (max 10 (floor stars-count 2)) :initial-element #\*)))
    (format out "~%~A~A~A~%" stars-left center stars-right)
    (format out (intl:gettext "~%Input:~%"))
    (displa input)))

;; Execute the code in FILENAME as a batch file.  If EXPECTED-ERRORS
;; is non-NIL, it is a list of numbers denoting which tests in this
;; file are expected to fail.  OUT specifies the stream where any
;; output goes (defaulting to *standard-output*).  SHOW-EXPECTED is
;; non-NIL if the expected results should also be printed.  SHOW-ALL
;; is non-NIL if all tests (including expected failures) should be
;; shown.  Finally, SHOWTIME is non-NIL if the execution time should
;; be displayed.
;;
;; This function returns four values:
;;   1.  the filename
;;   2.  NIL or a Maxima list of test numbers that failed
;;   3.  NIL or a Maxima list of test numbers that were expected to
;;       fail but actually passed.
;;   4.  Total number of tests in the file
(defun test-batch (filename expected-errors
			    &key (out *standard-output*) (show-expected nil)
			    (show-all nil) (showtime nil))
  (declare (special $batch_answers_from_file))
  (let (result
	next-result
	next
	error-log
	all-differences
	unexpected-pass
	strm
	expr
	problem-lineinfo
	problem-lineno
	tmp-output
	save-output
	($ratprint nil)
	(*mread-prompt* "")
	(*read-base* 10.)
	(num-problems 0)
	(i 0)
	(start-run-time 0)
	(end-run-time 0)
	(start-real-time 0)
	(end-real-time 0)
	(test-start-run-time 0)
	(test-end-run-time 0)
	(test-start-real-time 0)
	(test-end-real-time 0)
	(*query-io* *query-io*)
	(*standard-input* *standard-input*)
    (filename-basename (unix-like-basename filename)))
    
    (cond (*collect-errors*
	   (setq error-log
		 (if (streamp *collect-errors*) *collect-errors*
		   (handler-case
		       (open (alter-pathname filename :type "ERR") :direction :output :if-exists :supersede)
		     (file-error () nil))))
	   (when error-log
	     (format t (intl:gettext "~%batch: write error log to ~a") error-log)
	     (format error-log (intl:gettext "~%/* Maxima error log from tests in ~A") filename)
	     (format error-log " */~2%"))))
 
    (unwind-protect 
	(progn
	  (setq strm (open filename :direction :input))
	  (when $batch_answers_from_file
	    (setq *query-io* (make-two-way-stream strm out)
		  *standard-input* strm))
	  (setq start-real-time (get-internal-real-time))
	  (setq start-run-time (get-internal-run-time))
	  (while (not (eq 'eof (setq expr (mread strm 'eof))))
	    (incf num-problems)
	    (setq problem-lineinfo (second (first expr)))
	    (setq problem-lineno (if (and (consp problem-lineinfo) (integerp (first problem-lineinfo)))
				     (1+ (first problem-lineinfo))))
	    (incf i)
        (when show-all
          (print-test-batch-problem out filename-basename i problem-lineno (third expr)))
	    (setf tmp-output (make-string-output-stream))
	    (setf save-output *standard-output*)
	    (setf *standard-output* tmp-output)
	  
	    (unwind-protect
		 (progn
		   (setq test-start-run-time (get-internal-run-time))
		   (setq test-start-real-time (get-internal-real-time))
		   (let (($errormsg t))
		     (setq result (meval* `(($errcatch) ,(third expr)))))
		   (setq result (if ($emptyp result) 'error-catch (second result)))
		   (setq test-end-run-time (get-internal-run-time))
		   (setq test-end-real-time (get-internal-real-time))
		   (setq $% result))
	      (setf *standard-output* save-output))

	    (setq next (mread strm 'eof))
	    (if (eq next 'eof) (merror (intl:gettext "batch: missing expected result in test script.")))
	  
	    (setq next-result (third next))
	    (let* ((correct (batch-equal-check next-result result))
		   (expected-error (member i expected-errors))
		   (pass (or correct expected-error)))
	      (when (or show-all (not pass) (and correct expected-error)
			(and expected-error show-expected))
        (unless show-all
          (print-test-batch-problem out filename-basename i problem-lineno (third expr)))
		(format out (intl:gettext "~%~%Result:~%"))
		(format out "~a" (get-output-stream-string tmp-output))
		(displa $%)
		(when (eq showtime '$all)
		  (format out (intl:gettext "~%Time:  ~,3F sec (~,3F elapsed)")
			  (float (/ (- test-end-run-time test-start-run-time)
				    internal-time-units-per-second))
			  (float (/ (- test-end-real-time test-start-real-time)
				    internal-time-units-per-second)))))
	      (cond ((and correct expected-error)
		     (push i unexpected-pass)
		     (format t
			     (intl:gettext "~%... Which was correct, but was expected ~
                              to be wrong due to a known bug in~% Maxima or ~A.~%")
			     (lisp-implementation-type)))
		    (correct
		     (if show-all (format t (intl:gettext "~%... Which was correct.~%"))))
		    ((and (not correct) expected-error)
		     (when (or show-all show-expected)
		       (format t
			       (intl:gettext "~%This is a known error in Maxima or in ~A. ~
                                    The correct result is:~%")
			       (lisp-implementation-type))
		       (displa next-result)))
		    (t (format t (intl:gettext "~%This differed from the expected result:~%"))
		     (push i all-differences)
		     (displa next-result)
		     (cond ((and *collect-errors* error-log)
               (let ((*print-base* 10))
			    (format error-log (intl:gettext "/* Problem ~A~A */~%")
                                    i (if problem-lineno (format nil (intl:gettext " (line ~S)") problem-lineno) "")))
			    (mgrind (third expr) error-log)
			    (list-variable-bindings (third expr) error-log)
			    (format error-log ";~%")
			    (format error-log (intl:gettext "/* Erroneous Result?:~%"))
			    (mgrind result error-log) (format error-log " */ ")
			    (terpri error-log)
			    (format error-log (intl:gettext "/* Expected result: */~%"))
			    (mgrind next-result error-log)
			    (format error-log ";~%~%"))))))))
      (close strm))
    (setq end-run-time (get-internal-run-time))
    (setq end-real-time (get-internal-real-time))
    (cond (error-log
	   (or (streamp *collect-errors*)
	       (close error-log))))
    (let*
      ((*print-base* 10)
       (n-expected-errors (length expected-errors))
       (expected-errors-trailer
	 (if (= n-expected-errors 0)
	    ""
	    (format nil (intl:gettext " (not counting ~a expected errors)") n-expected-errors)))
       (time (if showtime
		 (format nil (intl:gettext "   using ~,3F seconds (~,3F elapsed).~%")
			 (float (/ (- end-run-time start-run-time) internal-time-units-per-second))
			 (float (/ (- end-real-time start-real-time) internal-time-units-per-second)))
		 "")))
      (cond ((null all-differences)
	     (format t (intl:gettext "~a/~a tests passed~a~%~A")
		     (- num-problems n-expected-errors) (- num-problems n-expected-errors)
		     expected-errors-trailer
		     time)
	     (when unexpected-pass
	       (multiple-value-bind (plural was-were)
		 (if (> (length unexpected-pass) 1)
		     (values "s" "were")
		     (values "" "was"))
	       (format t (intl:gettext "~%The following ~A problem~A passed but ~A expected to fail: ~A~%")
		       (length unexpected-pass) plural was-were (reverse unexpected-pass))))
	     (values filename
		     nil
		     `((mlist) ,@(reverse unexpected-pass))
		     num-problems))
	    (t
	     (format t (intl:gettext "~%~a/~a tests passed~a~%~A")
		     (- num-problems n-expected-errors (length all-differences)) (- num-problems n-expected-errors) expected-errors-trailer
		     time)
	     (let ((s (if (> (length all-differences) 1) "s" "")))
	       (format t (intl:gettext "~%The following ~A problem~A failed: ~A~%")
		       (length all-differences) s (reverse all-differences)))
	     (when unexpected-pass
	       (multiple-value-bind (plural was-were)
		 (if (> (length unexpected-pass) 1)
		     (values "s" "were")
		     (values "" "was"))
	       (format t (intl:gettext "~%The following ~A problem~A passed but ~A expected to fail: ~A~%")
		       (length unexpected-pass) plural was-were (reverse unexpected-pass))))
	     (values filename
		     `((mlist) ,@(reverse all-differences))
		     `((mlist) ,@(reverse unexpected-pass))
		     num-problems))))))
       
;;to keep track of global values during the error:
(defun list-variable-bindings (expr &optional str &aux tem)
  (loop for v in(cdr ($listofvars  expr))
    when (member v $values :test #'equal)
    collecting (setq tem`((mequal) ,v ,(meval* v)))
    and
    do (cond (str (format str ",")(mgrind tem str)))))

;;in init_max
;; name = foo or foo.type or dir/foo.type or dir/foo 
;; the empty parts are filled successively from defaults in templates in
;; the path.   A template may use multiple {a,b,c} constructions to indicate
;; multiple possibilities.  eg foo.l{i,}sp or foo.{dem,dm1,dm2}
(defmfun $file_search (name &optional paths)
  (if (and (symbolp name)
	   (char= (char (symbol-name name) 0) #\$))
      (setq name (subseq (print-invert-case name) 1)))
  (if (symbolp name)  (setf name (string name)))
  (if (file-exists-p name) (return-from $file_search name))
  (or paths (setq paths ($append $file_search_lisp  $file_search_maxima
				 $file_search_demo)))
  (atomchk paths '$file_search t)
  (new-file-search (string name) (cdr paths)))

;; Returns T if NAME exists and it does not appear to be a directory.
;; Note that Clisp throws an error from PROBE-FILE if NAME exists
;; and is a directory; hence the use of IGNORE-ERRORS.

(defun file-exists-p (name)
  (let ((foo (ignore-errors (probe-file name))))
    (if foo (not (apparently-a-directory-p foo)))))

(defun pathname-component-present-p (value)
  "Return T iff VALUE represents a present, meaningful pathname component.
  NIL, :UNSPECIFIC and the empty string mean the component is absent."
  (not (member value '(nil :unspecific "") :test #'equal)))

(defun apparently-a-directory-p (path)
  "Return T iff PATH looks like a directory, i.e. has a non-NIL directory
  and an absent name and type component."
  (and (pathname-directory path)
       (not (pathname-component-present-p (pathname-name path)))
       (not (pathname-component-present-p (pathname-type path)))))

(defun directory-exists-p (path)
  "Check whether PATH is an existing directory using
  implementation-specific functions, since Common Lisp does not
  specify a way to check whether a path is a file or a directory.
  Falls back to PROBE-FILE and checking whether the returned pathname
  looks like a directory."
  (ignore-errors
    #+abcl
    (extensions:probe-directory (ensure-pathname-as-directory path))
    #+allegro
    (excl:file-directory-p path)
    #+clisp
    (ext:probe-directory (ensure-pathname-as-directory path))
    #+gcl
    (eq (si:stat (namestring path)) :directory)
    #+lispworks
    (lw:file-directory-p path)
    #-(or abcl allegro clisp gcl lispworks)
    (let ((p (probe-file path)))
      (and p (apparently-a-directory-p p)))))

(defun ensure-pathname-as-directory (path)
  "Ensure PATH is a directory pathname (name and type both absent).
  If PATH already looks like a directory pathname, return it unchanged.
  Otherwise push the name (and type) into the directory component list."
  (let ((p (pathname path)))
    (if (not (apparently-a-directory-p p))
      (make-pathname
       :directory (append (or (pathname-directory p) '(:relative))
                          (list (file-namestring p)))
       :name nil
       :type nil
       :defaults p)
      p)))

(defun immediate-subdirectories (dir-path)
  "Return a list of pathnames for the immediate subdirectories of DIR-PATH.
  The function uses implementation-specific mechanisms where available to avoid
  returning regular files."
  (let* ((dir (ensure-pathname-as-directory dir-path))
         (wild-dir (make-pathname :directory
                                  #+(or abcl allegro lispworks)
                                  (pathname-directory dir)
                                  #-(or abcl allegro lispworks)
                                  (append (pathname-directory dir) '(:wild))
                                  :name
                                  #+(or abcl allegro lispworks)
                                  :wild
                                  #-(or abcl allegro lispworks)
                                  nil
                                  :type nil
                                  :defaults dir)))
    #+(or clisp cmucl ecl gcl sbcl)
    (directory wild-dir)
    #+allegro
    (remove-if-not #'apparently-a-directory-p
                   (directory wild-dir :directories-are-files nil))
    #+ccl
    (directory wild-dir :directories t)
    #-(or allegro ccl clisp cmucl ecl gcl sbcl)
    (remove-if-not #'apparently-a-directory-p
                   (directory wild-dir))))

(defun pathname-versionless-match-p (p1 p2)
  ;; Work around an ECL bug where pathnames returned by DIRECTORY
  ;; carry :VERSION :NEWEST, while pathnames constructed via
  ;; MAKE-PATHNAME use default :VERSION NIL, causing PATHNAME-MATCH-P
  ;; to return NIL even though all visible components are identical.
  ;; Normalizing both sides to :VERSION NIL makes the comparison
  ;; correct on ECL without affecting other implementations.
  (pathname-match-p
   (make-pathname :version nil :defaults p1)
   (make-pathname :version nil :defaults p2)))

(defun collect-directories (pathname &key (descendp (constantly t)))
  "Return all directories matching the directory part of PATHNAME, which
  can include wild components (:WILD, :WILD-INFERIORS, or string patterns
  such as \"a*\" or \"a?\", if the implementation supports them).
  Before descending into any directory due to a wildcard expansion,
  DESCENDP is called with that directory as its argument; the directory is
  skipped unless DESCENDP returns T.
  Typical use: Pass a DESCENDP that returns NIL for directories that should
  be ignored, such as \".git\", avoiding needless traversal of large or
  irrelevant subtrees."
  (let ((p (merge-pathnames pathname)))
    (labels
      ((expand (parent remaining)
         ;; PARENT is a concrete directory pathname built so far.
         ;; REMAINING is the list of directory components yet to be matched.
         (if (null remaining)
           (list parent)
           (let ((component (car remaining))
                 (rest (cdr remaining)))
             (if (eq component :wild-inferiors)
               (let ((subdirs (remove-if-not descendp
                                             (immediate-subdirectories parent))))
                 (append
                  ;; Zero levels: Skip :wild-inferiors entirely.
                  (expand parent rest)
                  ;; One or more levels: Descend, keeping :wild-inferiors active.
                  (mapcan (lambda (subdir)
                            (expand subdir remaining))
                          subdirs)))
               (if (wild-pathname-p
                    (make-pathname :directory (list :relative component))
                    :directory)
                 ;; :WILD or a string pattern like "a*" or "a?": Filter matching
                 ;; subdirectories via the filesystem.
                 (let* ((pattern (make-pathname
                                  :directory (append (pathname-directory parent)
                                                     (list component))
                                  :name nil :type nil
                                  :defaults parent))
                        (subdirs (remove-if-not
                                  (lambda (subdir)
                                    (pathname-versionless-match-p subdir pattern))
                                  (remove-if-not
                                   descendp
                                   (immediate-subdirectories parent)))))
                   (mapcan (lambda (subdir)
                             (expand subdir rest))
                           subdirs))
                 ;; Concrete component: Check existence and step in directly
                 ;; without calling DESCENDP (the caller named it explicitly).
                 (let ((next (make-pathname
                              :directory (append (pathname-directory parent)
                                                 (list component))
                              :name nil :type nil
                              :defaults parent)))
                   (when (directory-exists-p next)
                     (expand next rest)))))))))
      (expand (make-pathname :directory (list (car (pathname-directory p)))
                             :name nil :type nil
                             :defaults p)
              (cdr (pathname-directory p))))))

;; We keep these here in case we want to optimize the search.  To
;; speed things up, we might want to support search lists like
;; "share/**/*.{mac,wxm,mc}" so that we only descend the directory
;; once.  Then we would take the list of paths and try to match the
;; one with the given extensions.
;;
;; Currently, the search list is ["share/**/*.mac", "share/**/*.wxm",
;; "share/**/*.mc"].  Thus to find "foo.mc", we end up doing a
;; directory 3 times.
#+nil
(defun new-file-search (name template)
  (cond ((file-exists-p name))
	((atom template)
	 (let ((lis (loop for w in (split-string template "{}")
			  when (null (position #\, w))
			  collect w
			  else
			  collect (split-string w ","))))
	   (new-file-search1 name "" lis)))
	(t
	 (let ((temp nil))
	   (loop for v in template
		 when (setq temp (new-file-search name v))
		   do (return temp))))))

#+nil
(defun new-file-search1 (name begin lis)
  (cond ((null lis)
	 (let ((file (namestring ($filename_merge begin name))))
	   (if (file-exists-p file) file nil)))
	((atom (car lis))
	 (new-file-search1 name
			   (if begin
			       ($sconcat begin (car lis)) (car lis))
			   (cdr lis)))
	(t (loop for v in (car lis) with tem
		  when (setq tem  (new-file-search1 name begin (cons v (cdr lis))))
		  do (return tem)))))

(defmvar $file_search_cache '$auto
  "Controls the use of a cache by $FILE_SEARCH (used by $LOAD and $BATCH) to
  speed up file searches.
  - If '$AUTO (default), use the cache if a one-time test shows that the
    file system in *MAXIMA-USERDIR* and the Lisp implementation's DIRECTORY
    function behave as expected.
  - If NIL, never use the cache.
  - If T or any other value, always use the cache."
  :setting-list (nil t $auto))

(defvar *directory-cache* (make-hash-table :test #'equal)
  "The hash table that holds the cache for the DIRECTORY-CACHED function.")

(defvar *directory-cache-mdelta* 3
  "If a directory within a search path has been modified within the last
  N seconds, where N is the value of this variable, then DIRECTORY-CACHED will
  not use its cache for that search path.
  This is to account for the limited resolution of modification timestamps
  in Common Lisp (1 second) and some file systems. For example, some FAT
  file systems may have a modification timestamp resolution of 2 seconds.
  Without this logic, successive directory modifications in short intervals
  could result in new files not being found by DIRECTORY-CACHED.")

(defvar *test-directory-cached-result* :unknown
  "Stores the result of the TEST-DIRECTORY-CACHED function.")

(defvar *debug-directory-cached* nil
  "If non-NIL, print debug information for the DIRECTORY-CACHED function.")

(defun file-mtime (path)
  "Returns the modification time of the file/directory PATH."
  #+clisp
  ;; CLISP's FILE-WRITE-DATE (on UNIX) only works for regular files,
  ;; not directories.
  (nth-value 2 (ext:probe-pathname path))
  #-clisp
  (file-write-date path))

(defun file-mtime-or-nil (path)
  "Like FILE-MTIME, but returns NIL in the case of an error, the most likely
  error being that the file/directory doesn't exist."
  (ignore-errors (file-mtime path)))

(defun create-empty-file (&rest path)
  "Creates an empty file whose path is combined from PATH using COMBINE-PATH."
  (with-open-file (stream
                   (apply #'combine-path path)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    (declare (ignorable stream))))

(defun test-directory-cached-dir-base ()
  "Returns the base path of the directory that will be used for tests,
  which does not depend on the Maxima and Lisp version."
  (combine-path *maxima-userdir* "test-directory-cached" ""))

(defun test-directory-cached-dir ()
  "Returns the path of the directory that will be used for tests."
  (combine-path (test-directory-cached-dir-base)
                (maxima-version1)
                *maxima-lispname*
                (lisp-implementation-version1)))

(defun test-directory-cached ()
  "Tests whether the directory cache can be used.
  It checks that creating a file/directory within a parent directory updates
  the parent directory's modification timestamp.
  It also checks that the helper function COLLECT-DIRECTORIES work as expected.
  The special return value :TRY-LATER means that the test cannot be performed
  reliably at the moment due to limited timestamp precision."
  (macrolet ((dbg (string &rest args)
               `(when *debug-directory-cached*
                  (format *debug-io*
                    (concatenate 'string "test-directory-cached: " ,string "~%")
                    ,@args))))
    (let* ((userdir-mtime (file-mtime-or-nil *maxima-userdir*))
           (now (get-universal-time))
           (mtime-threshold (- now *directory-cache-mdelta*))
           parent-dir
           file-to-create)
      (cond
        (userdir-mtime
          ;; The Maxima user directory exists already. We can create a test file
          ;; in there and observe whether the directory's modification timestamp
          ;; changes - unless it has already been changed too recently.
          (dbg "Maxima user directory \"~A\" exists" *maxima-userdir*)
          (setq parent-dir *maxima-userdir*
                file-to-create
                  (combine-path *maxima-userdir*
                                (format nil "test-directory-cached-~A-~A"
                                        now (get-internal-real-time)))))
        (t
          ;; The Maxima user directory doesn't exist yet, or there was an error.
          ;; We will try to create the directory and observe whether the parent
          ;; directory's modification timestamp changes.
          (dbg "could not get mtime of Maxima user directory \"~A\"" *maxima-userdir*)
          (setq parent-dir (combine-path *maxima-userdir* ".."))))
      (let ((parent-dir-mtime (file-mtime-or-nil parent-dir)))
        (cond
          ((not parent-dir-mtime)
            ;; The parent directory doesn't exist, or there was an error.
            (dbg "could not get mtime of parent directory \"~A\"" parent-dir)
            (return-from test-directory-cached nil))
          ((> parent-dir-mtime mtime-threshold)
            ;; The parent directory has been modified (or created) too recently.
            ;; We cannot reliably test right now, try again later.
            (dbg "parent directory modified too recently, retry later")
            (return-from test-directory-cached :try-later)))
        ;; The parent directory exists and hasn't been modified too recently.
        ;; Create the Maxima user directory or test file.
        (dbg "parent directory mtime before: ~A" parent-dir-mtime)
        (dbg "ensure Maxima user directory \"~A\" exists" *maxima-userdir*)
        (ensure-directories-exist (combine-path *maxima-userdir* ""))
        (when file-to-create
          ;; Create the test file and delete it afterwards.
          (dbg "create and delete test file \"~A\"" file-to-create)
          (create-empty-file file-to-create)
          (delete-file file-to-create))
        ;; Now the parent directory's modification timestamp should have changed.
        (setq parent-dir-mtime (file-mtime-or-nil parent-dir))
        (dbg "parent directory mtime after: ~A" parent-dir-mtime)
        (cond
          ((and parent-dir-mtime) (> parent-dir-mtime mtime-threshold)
            ;; It worked!
            (dbg "test succeeded"))
          (t
            ;; It didn't work!
            (dbg "test failed")
            (return-from test-directory-cached nil)))
        ;; Create nested subdirectories inside *MAXIMA-USERDIR* and files
        ;; in them. Make sure that our helper function COLLECT-DIRECTORIES works
        ;; as expected, as the directory cache relies on it.
        (let ((test-dir (test-directory-cached-dir)))
          (dbg "create test directories and files in \"~A\"" test-dir)
          (ensure-directories-exist (combine-path test-dir "dir" ""))
          (ensure-directories-exist (combine-path test-dir "dir" "dir" ""))
          (create-empty-file test-dir "file")
          (create-empty-file test-dir "dir" "file")
          (create-empty-file test-dir "dir" "dir" "file")
          (dbg "list test directories (immediate subdirectories)")
          (let ((items (collect-directories (combine-path test-dir "*" "*"))))
            (dbg "items listed (expect 1 directory): ~A" items)
            (cond
              ((and (= (length items) 1) (every #'directory-exists-p items))
                (dbg "test succeeded"))
              (t
                (dbg "test failed")
                (return-from test-directory-cached nil))))
          (dbg "list test directories (recursive)")
          (let ((items (collect-directories (combine-path test-dir "**" "*"))))
            (dbg "items listed (expect 3 directories): ~A" items)
            (cond
              ((and (= (length items) 3) (every #'directory-exists-p items))
                (dbg "test succeeded"))
              (t
                (dbg "test failed")
                (return-from test-directory-cached nil))))
          ;; If we made it till here, everything works as expected.
          ;; Unfortunately, there's no Common Lisp way to delete directories.
          (dbg "all tests succeeded")
          t)))))

(defun check-directory-cached ()
  "Checks for a previous result of TEST-DIRECTORY-CACHED stored as a file in
  (TEST-DIRECTORY-CACHED-DIR), which is specific to the Maxima version and Lisp
  implementation/version.
  If no stored result is found, call TEST-DIRECTORY-CACHED and store the result,
  if it is T or NIL."
  (macrolet ((dbg (string &rest args)
               `(when *debug-directory-cached*
                  (format *debug-io*
                          (concatenate 'string "check-directory-cached: " ,string "~%")
                          ,@args))))
    (let* ((test-dir (test-directory-cached-dir))
           (successful-file (combine-path test-dir "successful"))
           (unsuccessful-file (combine-path test-dir "unsuccessful"))
           (successful (file-exists-p successful-file))
           (unsuccessful (file-exists-p unsuccessful-file)))
      (dbg "checking for previous test result")
      (dbg "\"successful\" file exists? ~A" successful)
      (dbg "\"unsuccessful\" file exists? ~A" unsuccessful)
      (cond
        ((and successful (not unsuccessful))
          ;; The cache can be used.
          (dbg "previous test successful, using cache")
          t)
        ((not (or successful unsuccessful))
          ;; Test whether we can use the directory cache.
          ;; Use IGNORE-ERRORS to make sure that if the test itself produces
          ;; an error that isn't caught, it doesn't prevent Maxima from working.
          (dbg "no previous test result, testing whether cache can be used")
          (let ((result (ignore-errors (test-directory-cached))))
            (dbg "test successful? ~A" result)
            ;; If the test result was T or NIL (it could also be :TRY-LATER),
            ;; remember it by creating the appropriate file.
            (case result
              ((t)
                (dbg "create \"successful\" file")
                (ignore-errors (create-empty-file successful-file))
                (dbg "file created? ~A" (file-exists-p successful-file)))
              ((nil)
                (dbg "create \"unsuccessful\" file")
                (ignore-errors (create-empty-file unsuccessful-file))
                (dbg "file created? ~A" (file-exists-p unsuccessful-file))))
            result))
        (t
          ;; The cache cannot be used.
          (dbg "previous test unsuccessful, not using cache")
          nil)))))

(defmvar $file_search_ignore_dirs '((mlist) ".bzr" ".git" ".github" ".hg" ".svn"
                                            ".eclipse" ".idea" ".vs" ".vscode" ".zed")
  "List of directory names to ignore during file search, e.g. \".git\".
  These should only be simple directory names, not \"dir/subdir\" or \".*\"."
  :setting-predicate
  #'(lambda (val)
      (or (and (listp val)
               (eq (caar val) 'mlist)
               (every #'stringp (cdr val)))
          (values nil "Must be a list of strings or an empty list"))))

(defun pathname-without-file-components (path)
  "Returns a pathname for PATH with :NAME, :TYPE and :VERSION set to NIL."
  (make-pathname :name nil :type nil :version nil :defaults path))

(defun pathname-directory-prefix-p (p1 p2)
  "Returns T iff P1 is a prefix of P2, taking into account :HOST, :DEVICE
  and :DIRECTORY."
  (let* ((p1 (pathname p1))
         (p2 (pathname p2))
         (dir1 (pathname-directory p1))
         (dir2 (pathname-directory p2)))
    (and (equal (pathname-host p1) (pathname-host p2))
         (equal (pathname-device p1) (pathname-device p2))
         (<= (length dir1) (length dir2))
         (not (mismatch dir1 dir2
                        :end2 (length dir1)
                        :test #'equal)))))

(defun make-directory-cached-filterp ()
  "Returns a predicate function that can be used for filtering directories
  to be searched and files to be returned during file search in order to
  exclude certain files/directories."
  (let ((ignore-dirs (cdr $file_search_ignore_dirs))
        (test-dir-base (pathname-without-file-components (test-directory-cached-dir-base)))
        (obj-dir-base (pathname-without-file-components (maxima-objdir-base)))
        (obj-dir (pathname-without-file-components *maxima-objdir*)))
    #'(lambda (path)
        (cond
          ((let ((dir (pathname-without-file-components path)))
             ;; Ignore paths containing directory names
             ;; on the $file_search_ignore_dirs list.
             (and (not (some #'(lambda (part)
                                 (member part ignore-dirs :test #'string=))
                             (cdr (pathname-directory dir))))
                  ;; Ignore paths under the test directory for the directory cache.
                  (not (pathname-directory-prefix-p test-dir-base dir))
                  ;; Ignore paths containing foreign binaries
                  ;; belonging to other Maxima/Lisp versions.
                  (not (and (pathname-directory-prefix-p obj-dir-base dir)
                            (not (pathname-directory-prefix-p dir obj-dir))
                            (not (pathname-directory-prefix-p obj-dir dir)))))))
          (t
            (when *debug-directory-cached*
              (format *debug-io* "directory-cached: ignore \"~A\"~%" path))
            nil)))))

(defun directory-cached (path mtime-cache &key (filterp (constantly t)))
  "Behaves like DIRECTORY, but speeds up the search using a cache.
  For each unique directory component containing wildcards,
  such as \"/home/username/.maxima/**/\", a list of all (sub)directories,
  their modification timestamps and a hash table of previous searches inside
  these directories are cached.
  When querying the cache, the current modification timestamps are compared to
  the cached ones. Only if they match, the cache can be used.
  If any directory was modified within the last *DIRECTORY-CACHE-MDELTA*
  seconds, then the cache is not used (see documentation for
  *DIRECTORY-CACHE-MDELTA*).
  The actual file search is implemented by iterating over the list of
  directories and testing whether a file with the given name exists there.
  MTIME-CACHE must be a hash table. It will be used to cache modification
  timestamps of directories. NEW-FILE-SEARCH can call DIRECTORY-CACHED multiple
  times per search. This cache allows to minimize the number of file system
  queries, but the same cache should only be used during \"atomic\" operations,
  i.e. not persist across multiple searches. Otherwise, file system modifications
  could go unnoticed.
  FILTERP is an optional predicate that controls which paths are visited and
  returned. A path is included only if FILTERP returns non-NIL for it.
  FILTERP must be monotone with respect to the directory hierarchy: if it
  returns NIL for a directory, it must also return NIL for every file and
  subdirectory contained within it. This constraint allows the search to prune
  entire subtrees without descending into them, which is the primary mechanism
  by which FILTERP speeds up the search."
  (macrolet ((dbg (string &rest args)
               `(when *debug-directory-cached*
                  (format *debug-io*
                    (concatenate 'string "directory-cached @ \"~A\": " ,string "~%")
                    path
                    ,@args))))
    (labels ((file-mtime-or-nil-cached (path)
               "Cached version of FILE-MTIME-OR-NIL, uses the MTIME-CACHE hash table."
               (multiple-value-bind (entry cached) (gethash path mtime-cache)
                 (if cached
                   entry
                   (setf (gethash path mtime-cache) (file-mtime-or-nil path)))))
             (existing-path-mtime-pairs (paths)
               "Returns a list of (PATH . MTIME) items for the list of paths PATHS.
               Entries with an MTIME of NIL are removed. The assumption is that
               FILE-WRITE-DATE failing means that the file/directory doesn't exist.
               While that's not strictly true, it shouldn't make a practical difference."
               (remove-if-not #'cdr
                 (mapcar #'(lambda (path) (cons path (file-mtime-or-nil-cached path))) paths)))
             (some-mtime-gt (dirs mtime)
               (some #'(lambda (dir) (> (cdr dir) mtime)) dirs)))
      (when (and (eq $file_search_cache '$auto)
                 (not (member *test-directory-cached-result* '(t nil))))
        ;; Check whether the cache can be used.
        (setq *test-directory-cached-result* (check-directory-cached)))
      (remove-if-not
        filterp
        (cond
          ((or (not $file_search_cache) (eq *test-directory-cached-result* nil))
            ;; Cache disabled, use regular DIRECTORY.
            (directory path))
          ((wild-pathname-p path)
            ;; At least one wildcard appears in the path. Get the filename part.
            (let ((query-file (make-pathname :host nil :device nil :directory nil :defaults path))
                  (query-dir (pathname-without-file-components path))
                  (do-cache t)
                  (mtime-threshold (- (get-universal-time) *directory-cache-mdelta*))
                  dirs
                  rcache)
              ;; Look up the cache for the directory part of the path.
              (multiple-value-bind (entry cached) (gethash query-dir *directory-cache*)
                (cond
                  (cached
                    ;; Cache hit! The cache entry is of the form:
                    ;; ((DIRECTORY . MTIME)* . RCACHE)
                    ;; DIRECTORY is any directory to be searched for the file.
                    ;; MTIME is the directory's modification timestamp.
                    ;; RCACHE is a hash table for caching individual file search results.
                    (dbg "hit")
                    (destructuring-bind (cached-dirs . cached-rcache) entry
                      ;; Update the directory information, getting the current
                      ;; modification timestamps and removing directories that
                      ;; no longer exist.
                      (setq dirs (existing-path-mtime-pairs (mapcar #'car cached-dirs)))
                      (cond
                        ((null dirs)
                          ;; No directories from the cache entry exist any more.
                          ;; Possibly, the search root directory was deleted/renamed/moved.
                          ;; Don't use the cache entry (it will be removed).
                          (dbg "invalid (no directories exist any more)")
                          (setq cached nil))
                        ((some-mtime-gt dirs mtime-threshold)
                          ;; Some directory was modified too recently (within the
                          ;; last *DIRECTORY-CACHE-MDELTA* seconds). We cannot use
                          ;; the cache entry (it will be removed) and won't create
                          ;; a new cache entry at this time.
                          (dbg "invalid (some directory modified too recently)")
                          (setq cached nil
                                do-cache nil))
                        ((not (equal dirs cached-dirs))
                          ;; Some directory has had its modification time changed.
                          ;; A file or directory may have been added, renamed or
                          ;; deleted. We cannot use the cache entry (it will be removed).
                          (dbg "invalid (some directory modified)")
                          (setq cached nil))
                        (t
                          ;; All directories are still there, and they still have the
                          ;; same modification timestamps as in the cache entry.
                          ;; This means that there are no new, renamed or deleted
                          ;; files or directories inside. We can use the cache entry.
                          (dbg "valid")
                          (setq rcache cached-rcache)))
                      (unless cached
                        ;; The cache entry cannot be used - remove it.
                        (dbg "remove")
                        (remhash query-dir *directory-cache*))))
                  (t
                    ;; Cache miss!
                    (dbg "miss")))
                (unless cached
                  ;; We don't have a cache entry or cannot use it.
                  ;; Get the current list of directories using COLLECT-DIRECTORIES.
                  ;; This is the expensive operation that we're trying to minimize.
                  (setq dirs (existing-path-mtime-pairs
                               (collect-directories path
                                                    :descendp filterp)))
                  (cond
                    ((null dirs)
                      ;; There are no directories, so there can be no files.
                      ;; Return NIL immediately, don't cache this.
                      (dbg "no directories exist")
                      (return-from directory-cached nil))
                    ((some-mtime-gt dirs mtime-threshold)
                      ;; Some directory was modified too recently, don't cache.
                      (dbg "don't cache (some directory modified too recently)")
                      (setq do-cache nil)))
                  (cond
                    (do-cache
                      ;; Write into the cache.
                      (dbg "write")
                      (setq rcache (make-hash-table :test #'equal))
                      (setf (gethash query-dir *directory-cache*) (cons dirs rcache)))
                    (t
                      ;; Don't write into the cache.
                      (dbg "no-write"))))
                ;; Check if the search result for the given filename is cached in RCACHE
                ;; (but no need to check if RCACHE has just been created).
                (multiple-value-bind (entry cached) (if cached
                                                      (gethash query-file rcache)
                                                      (values nil nil))
                  (cond
                    (cached
                      ;; Cache hit! Return the cached result.
                      (dbg "result hit")
                      entry)
                    (t
                      ;; Cache miss! Perform the file search by looping over all
                      ;; directories and searching/probing a file with the given name in there.
                      ;; Build a list of all files found.
                      (dbg "result miss")
                      (let ((filename-is-wild (wild-pathname-p query-file))
                            result)
                        (dolist (dir dirs)
                          (let* ((dir-path (car dir))
                                 (merged (merge-pathnames query-file dir-path)))
                            (cond
                              (filename-is-wild
                                ;; The filename part contains wildcards, e.g. "*.lisp".
                                ;; Use DIRECTORY to find the files and make sure to return
                                ;; only regular files, not directories.
                                (dbg "search \"~A\"" merged)
                                (let ((files (remove-if #'apparently-a-directory-p
                                                        (directory merged))))
                                  (when files
                                    (dbg "found ~{\"~A\"~^, ~}"
                                         (mapcar #'(lambda (p)
                                                     (make-pathname :host nil
                                                                    :device nil
                                                                    :directory nil
                                                                    :defaults p))
                                                 files))
                                    (setq result (append files result)))))
                              (t
                                ;; The filename part is concrete, e.g. "test.lisp".
                                ;; Simply check for existence via FILE-EXISTS-P.
                                (dbg "probe \"~A\"" merged)
                                (when (file-exists-p merged)
                                (dbg "found")
                                (push (truename merged) result))))))
                        (cond
                          (do-cache
                            ;; Store the result (list of files found) in the cache entry's
                            ;; result cache. Return a copy of the list, since
                            ;; destructive modification may be used on the list.
                            (dbg "result write")
                            (copy-list (setf (gethash query-file rcache) result)))
                          (t
                            (dbg "result no-write")
                            result)))))))))
          (t
            ;; A path entirely without wildcards, e.g. "/dir/foo.mac".
            ;; No need for extra work, just check directly whether the file exists.
            (if (file-exists-p path)
              (list (truename path))
              nil)))))))

(defvar *debug-new-file-search* nil)

;; Search for a file named NAME.  If the file exists, return it.
;; Otherwise, TEMPLATE is a list of wildcard paths to be searched for
;; the NAME.  Each entry in TEMPLATE should be a Lisp wildcard
;; pathname.
(defun new-file-search (name template)
  (cond ((file-exists-p name))
	(t
	 (let* ((filename (pathname name))
            (merged-pathnames
              ;; Use DELETE-DUPLICATES to avoid searching for the same thing
              ;; twice when the filename contains a type (e.g. ".mac").
              (delete-duplicates
                (mapcar #'(lambda (p) (merge-pathnames filename p)) template)
                :test #'equal))
            (mtime-cache (make-hash-table :test #'equal))
            (filterp (make-directory-cached-filterp)))
	   (dolist (path merged-pathnames)
	     (let ((pathnames (directory-cached path mtime-cache :filterp filterp)))
	       (when *debug-new-file-search*
		 (format *debug-io* "wildpath ~S~%" path))
	       (when pathnames
         ;; We MUST sort the results in alphabetical order
		 ;; because that's how the old search paths were
		 ;; sorted.
		 (setf pathnames (sort pathnames #'string< :key #'namestring))
		 (when *debug-new-file-search*
		   (format *debug-io* "pathname = ~S~%" pathnames))
		 ;; If more than one path is returned, print a warning
		 ;; that we're selecting the first file.  Print all
		 ;; the matches too so the user knows.
		 (unless (= 1 (length pathnames))
		   (mwarning
		    (format nil
			    "More than one file matches.  Selecting the first file from:~
~%~{  ~A~^~%~}~%"
			    (mapcar #'namestring pathnames))))
		 (return-from new-file-search (namestring (first pathnames))))))))))

(defmfun $printfile (file)
  (setq file ($file_search1 file '((mlist) $file_search_usage)))
  (with-open-file (st file)
    (loop
       with tem
       while (setq tem (read-char st nil 'eof)) 
       do
       (if (eq tem 'eof) (return t))
       (princ tem))
    (namestring file)))

(defun disable-some-lisp-warnings ()
  ;; Suppress warnings about redefining functions;
  ;; it appears that only Clisp and SBCL emit these warnings
  ;; (ECL, GCL, CMUCL, and Clozure CL apparently do not).
  ;; Such warnings are generated by the autoload mechanism.
  ;; I guess it is plausible that we could also avoid the warnings by
  ;; reworking autoload to not trigger them. I don't have enough
  ;; motivation to attempt that right now.
  #+sbcl
  (setq sb-ext:*muffled-warnings* '(or sb-kernel:redefinition-with-defun sb-kernel:uninteresting-redefinition))
  #+sbcl
  (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))
  #+clisp
  (setq custom:*suppress-check-redefinition* t)

  ;; Suppress compiler output messages.
  ;; These include the "0 errors, 0 warnings" message output from Clisp,
  ;; and maybe other messages from other Lisps.
  (setq *compile-verbose* nil))

(defun enable-some-lisp-warnings ()
  ;; SB-KERNEL:UNINTERESTING-REDEFINITION appears to be the default value.
  #+sbcl
  (setq sb-ext:*muffled-warnings* 'sb-kernel:uninteresting-redefinition)
  #+sbcl
  (declaim (sb-ext:unmuffle-conditions sb-ext:compiler-note))
  #+clisp
  (setq custom:*suppress-check-redefinition* nil)
  (setq *compile-verbose* t))

(defun simple-remove-dollarsign (x)
  "Like stripdollar, but less heavy.  Intended for use with the
  testsuite implementation."
  (if (symbolp x)
      (subseq (maxima-string x) 1)
      x))

(defun intersect-tests (tests)
  ;; If TESTS is non-NIL, we assume it's a Maxima list of (maxima)
  ;; strings naming the tests we want to run.  They must match the
  ;; file names in $testsuite_files.  We ignore any items that aren't
  ;; in $testsuite_files.
  (mapcar #'simple-remove-dollarsign
	  (cond (tests
		 (let ((results nil))
		   ;; Using INTERSECTION would be convenient, but
		   ;; INTERSECTION can return the result in any
		   ;; order, and we'd prefer that the order of the
		   ;; tests be preserved.  CMUCL and CCL returns the
		   ;; intersection in reverse order.  Clisp produces
		   ;; the original order.  Fortunately, this doesn't
		   ;; have to be very fast, so we do it very naively.
		   (dolist (test (mapcar #'simple-remove-dollarsign (cdr tests)))
		     (let ((matching-test (find test (cdr $testsuite_files)
						:key #'(lambda (x)
							 (maxima-string (if (listp x)
									    (second x)
									    x)))
						:test #'string=)))
		       (when matching-test
			 (push matching-test results))))
		   (nreverse results)))
		(t
		 (cdr $testsuite_files)))))

(defun print-testsuite-summary (errs unexpected-pass error-count total-count)
  (flet
      ((problem-summary (x)
	 ;; We want to print all the elements in the list.
	 (let ((*print-length* nil)
	       (s (if (> (length (rest x)) 1) "s" "")))
	   (format t
		   (intl:gettext
		    "  ~a problem~a:~%    ~a~%")
		   (first x)
		   s
		   (sort (rest x) #'<)))))
    (if (null errs)
	(format t
		(intl:gettext
		 "~%~%No unexpected errors found out of ~:d tests.~%")
		total-count)
	(format t (intl:gettext "~%Error summary:~%")))
    (when errs
      (format t (intl:gettext "Error(s) found:~%"))
      (mapcar #'problem-summary (reverse errs)))
    (when unexpected-pass
      (format t (intl:gettext "Tests that were expected to fail but passed:~%"))
      (mapcar #'problem-summary (reverse unexpected-pass)))
    (when errs
      (format t
	      (intl:gettext
	       "~&~:d test~p failed out of ~:d total tests.~%")
	      error-count
	      error-count
	      total-count))))

(defun validate-given-tests (tests share-tests-p)
  ;; Check the test names and print out some warnings if it
  ;; doesn't exist, or if it does and is part of the share test
  ;; suite, but share_tests was not set.
  (dolist (test (mapcar #'simple-remove-dollarsign
			(if (listp tests)
			    (cdr tests)
			    (list tests))))
    (cond ((and (not share-tests-p)
		(find test (cdr $share_testsuite_files)
		      :key #'(lambda (x)
			       (maxima-string (if (listp x)
						  (second x)
						  x)))
		      :test #'string=))
	   (mwarning test "is a share test, but share_tests was not set"))
	  ((not (find test (cdr $testsuite_files)
		      :key #'(lambda (x)
			       (maxima-string (if (listp x)
						  (second x)
						  x)))
		      :test #'string=))
	   (mwarning "Unknown test: " test)))))
  
(defmfun $run_testsuite (&key tests display_all display_known_bugs share_tests time debug (answers_from_file t))
  "Run the testsuite.  Options are
  tests                List of tests to run
  display_all          Display output from each test entry
  display_known_bugs   Include tests that are known to fail
  time                 Display time to run each test entry
  share_tests          Whether to include the share testsuite or not
  debug                Set to enable some debugging prints
  answers_from_file    Read interactive answers from source file.
"
  (enable-some-lisp-warnings)
  (let ((test-file)
	(expected-failures)
	(test-file-path))
    (format t "Testsuite run for ~a ~a:~%"
	    (lisp-implementation-type) (lisp-implementation-version))
    ;; Allow only T and NIL for display_known_bugs and display_all
    (unless (member display_known_bugs '(t nil))
      (merror (intl:gettext "run_testsuite: display_known_bugs must be true or false; found: ~M") display_known_bugs))
    (unless (member display_all  '(t nil))
      (merror (intl:gettext "run_testsuite: display_all must be true or false; found: ~M") display_all))
    (unless (member time '(t nil $all))
      (merror (intl:gettext "run_testsuite: time must be true, false, or all; found: ~M") time))
    (unless (member share_tests '(t nil $only))
      (merror (intl:gettext "run_testsuite: share_tests must be true, false or only: found ~M") share_tests))
    (unless (member answers_from_file '(t nil))
      (merror (intl:gettext "run_testsuite: answers_from_file must be true or false only; found ~M") answers_from_file))
    
    (setq *collect-errors* nil)

    (multiple-value-bind (desired-tests desired-search-path)
	(ecase share_tests
	  ((nil)
	   ;; Do nothing
	   (values $testsuite_files $file_search_tests))
	  ((t)
	   ;; Append the share files and concatenate the search paths
	   ;; for tests and maxima so we can find both sets of tests.
	   (values ($append $testsuite_files $share_testsuite_files)
		   ;; Is there a better way to do this?
		   (concatenate 'list
				'((mlist))
				(rest $file_search_tests)
				(rest $file_search_maxima))))
	  ($only
	   ;; Only the share test files
	   (values $share_testsuite_files $file_search_maxima)))
      (let* (($testsuite_files desired-tests)
	     ($file_search_tests desired-search-path)
	     (error-break-file)
	     (tests-to-run (intersect-tests (cond ((consp tests) tests)
						  (tests (list '(mlist) tests)))))
	     (test-count 0)
	     (total-count 0)
	     (error-count 0)
	     ($batch_answers_from_file answers_from_file)
	     filename
	     diff
	     upass)
	(declare (special $batch_answers_from_file))
	(validate-given-tests tests share_tests)

	(when debug
	  (let (($stringdisp t))
	    (mformat t "$testsuite_files = ~M~%" $testsuite_files)
	    (mformat t "$file_search_tests = ~M~%" $file_search_tests)))
	(when debug
	  (let (($stringdisp t))
	    (mformat t "tests-to-run = ~M~%" tests-to-run)))

	(unless tests-to-run
	  (mwarning "No tests to run")
	  (return-from $run_testsuite '$done))

	(flet
	    ((testsuite ()
	       (loop with errs = 'nil
		     with unexpected-pass = nil
		     for testentry in tests-to-run
		     do (if (atom testentry)
			    (progn
			      (setf test-file testentry)
			      (setf expected-failures nil))
			    (progn
			      (setf test-file (second testentry))
			      (setf expected-failures
				    ;; Support the expected failures list in
				    ;; two formats:
				    ;;
				    ;; ((mlist) "test" 1 2 3)
				    ;; ((mlist) "test" ((mlist) 1 2 3))
				    ;;
				    ;; The first is the old style whereas the
				    ;; second is the new style.  We support
				    ;; the old style for backward
				    ;; compatibility.
				    (if (consp (caddr testentry))
					(cdaddr testentry)
					(cddr testentry)))))
			(setf test-file-path ($file_search test-file $file_search_tests))
			(format t
				(intl:gettext "Running tests in ~a: ")
				(if (symbolp test-file)
				    (subseq (print-invert-case test-file) 1)
				    test-file))
			(when debug
			  (format t (intl:gettext "(~A) ") test-file-path))
			(or
			  (errset
			    (progn
			      (multiple-value-setq (filename diff upass test-count)
				(test-batch test-file-path
					    expected-failures :show-expected display_known_bugs
					    :show-all display_all :showtime time))
			      (incf total-count test-count)
			      (when (or (rest diff) (rest upass))
				(incf error-count (length (rest diff)))
				(when (rest diff)
				  (push (list* filename (rest diff))
					errs))
				(when (rest upass)
				  (push (list* filename (rest upass))
					unexpected-pass)))))
			  (progn
			    (setq error-break-file (format nil "~a" test-file))
			    (push (list error-break-file "error break")
				  errs)
			    (format t
				    (intl:gettext "~%Caused an error break: ~a")
				    test-file)
			    ;; If the test failed because we
			    ;; couldn't find the file, make a note of
			    ;; that.
			    (unless test-file-path
			      (format t (intl:gettext ": test file not found.")))
			    (format t "~%")))
		     finally
			(print-testsuite-summary errs unexpected-pass error-count total-count))))
	  (time (testsuite))))))
  (disable-some-lisp-warnings)
  '$done)
