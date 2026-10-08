;;; sb-sprof harness for Maxima's testsuite.
;;;
;;; Usage, from maxima-local --batch-string, each :lisp on its own line
;;; (runprof.sh does this):
;;;   :lisp (load "prof.lisp")
;;;   :lisp (prof-start :mode :cpu :interval 0.004)
;;;   run_testsuite(...);
;;;   :lisp (prof-finish "/path/prefix")
;;;
;;; Every test file runs inside its own compiled wrapper function named
;;; |TESTFILE <name>|, so each sample's stack names the test file.
;;;
;;; PROF-FINISH writes PREFIX.summary, .self.tsv, .cum.tsv, .attr.txt
;;; (samples charged to the innermost Maxima frame, with leaves and
;;; callers), .attrcum.tsv, .leafcat.tsv, .files.txt (per test file),
;;; .compiler.tsv, .folded (for stk.py and flamegraph.pl) and, unless
;;; :GRAPH NIL, sb-sprof's own .graph and .flat reports. Use :GRAPH NIL
;;; in :ALLOC mode, where expanding the samples would not fit the heap.

(require :sb-sprof)
(in-package :maxima)

(defvar *prof-orig-test-batch* nil)
(defvar *prof-wrappers* (make-hash-table :test 'equal))
(defvar *prof-sink* nil)
(defvar *prof-mode* nil)
(defvar *prof-interval* nil)
(defvar *prof-start* nil)

(defun prof-test-file-wrapper (filename)
  (let ((base (pathname-name (pathname filename))))
    (or (gethash base *prof-wrappers*)
        (setf (gethash base *prof-wrappers*)
              (sb-sprof:with-sampling (nil)
                (let ((name (intern (concatenate 'string "TESTFILE " base)
                                    :maxima)))
                  (compile nil
                           `(sb-int:named-lambda ,name (&rest args)
                              (multiple-value-prog1
                                  (apply *prof-orig-test-batch* args)
                                (setq *prof-sink* ,base))))))))))

(defun prof-install-wrapper ()
  (unless *prof-orig-test-batch*
    (setq *prof-orig-test-batch* (fdefinition 'test-batch))
    (setf (fdefinition 'test-batch)
          (lambda (filename &rest args)
            (apply (prof-test-file-wrapper filename) filename args)))))

(defun prof-counters ()
  (list :run (get-internal-run-time)
        :real (get-internal-real-time)
        :gc sb-ext:*gc-run-time*
        :bytes (sb-ext:get-bytes-consed)))

(defun prof-start (&key (mode :cpu) (interval 0.004) (max-samples 50000000))
  (prof-install-wrapper)
  (sb-ext:gc :full t)
  (setq *prof-mode* mode
        *prof-interval* interval
        *prof-start* (prof-counters))
  (sb-sprof:start-profiling :mode mode :sample-interval interval
                            :max-samples max-samples)
  t)

;;; Like SB-SPROF::CONVERT-RAW-DATA, but keep the deduplicated traces:
;;; a list of (LOCS . MULTIPLICITY), LOCS = #(info0 pc0 info1 pc1 ...),
;;; innermost frame first. With EXPAND, also fill SB-SPROF::*SAMPLES*
;;; so that SB-SPROF:REPORT works.
(defun prof-collect (expand)
  (let ((ht (sb-sprof::build-serialno-to-code-map))
        (threads)
        (traces)
        (n-unique 0))
    (let ((saved (sb-sprof::sb-toggle-sigprof (sb-sys:int-sap 0) 1)))
      (sb-sprof::call-with-each-profile-buffer
       (lambda (sap thread memusage)
         (push (cons thread memusage) threads)
         (dolist (tr (sb-sprof::extract-traces sap ht))
           (incf n-unique)
           (push (cons tr thread) traces))))
      (setf (sb-alien:extern-alien "sb_sprof_enabled" sb-alien:int) 0)
      (sb-sprof::sb-toggle-sigprof (sb-sys:int-sap 0) saved))
    (when expand
      (let* ((total (loop for ((locs . mult)) in traces
                          sum (* mult (+ (length locs) 2))))
             (vector (make-array total))
             (index 0))
        (loop for ((locs . mult) . thread) in traces
              do (dotimes (i mult)
                   (let ((end (+ index (length locs) 2)))
                     (setf (aref vector index) `(sb-sprof::trace-start . ,end)
                           (aref vector (1+ index)) thread)
                     (replace vector locs :start1 (+ index 2))
                     (setq index end))))
        (setf (sb-sprof::samples-vector sb-sprof::*samples*) vector
              (sb-sprof::samples-unique-trace-count sb-sprof::*samples*) n-unique
              (sb-sprof::samples-sampled-threads sb-sprof::*samples*) threads)))
    (mapcar #'car traces)))

;;; Frame names

(defvar *prof-name-cache*)

(defun prof-name (info)
  (if (null info)
      "<no info>"
      (multiple-value-bind (v found) (gethash info *prof-name-cache*)
        (if found
            v
            (setf (gethash info *prof-name-cache*)
                  (sb-sprof::node-name (sb-sprof::make-node info)))))))

(defun prof-principal-symbol (name)
  (cond ((symbolp name) name)
        ((consp name)
         (let ((in (member :in name)))
           (cond (in (prof-principal-symbol (second in)))
                 ((and (symbolp (car name))
                       (member (symbol-name (car name))
                               '("SETF" "FLET" "LABELS" "FAST-METHOD"
                                 "SLOW-METHOD" "METHOD" "TL-XEP" "XEP"
                                 "VARARGS-ENTRY" "&OPTIONAL-PROCESSOR"
                                 "&MORE-PROCESSOR")
                               :test #'string=))
                  (prof-principal-symbol (second name)))
                 ((eq (car name) 'lambda) nil)
                 (t (prof-principal-symbol (car name))))))
        (t nil)))

(defun prof-package-name (name)
  (let ((sym (prof-principal-symbol name)))
    (cond ((stringp name)
           (if (search "foreign function" name) "<foreign>" "<other>"))
          ((null sym) "<anon>")
          ((null (symbol-package sym)) "<uninterned>")
          (t (package-name (symbol-package sym))))))

(defun prof-maxima-name-p (name)
  (let ((pkg (prof-package-name name)))
    (not (or (char= (char pkg 0) #\<)
             (string= pkg "COMMON-LISP")
             (string= pkg "KEYWORD")
             (and (> (length pkg) 3) (string= "SB-" pkg :end2 3))))))

(defun prof-testfile-name-p (name)
  (and (symbolp name)
       (let ((s (symbol-name name)))
         (and (> (length s) 9) (string= "TESTFILE " s :end2 9)))))

(defun prof-compiler-name-p (name)
  ;; WITH-COMPILATION-UNIT also wraps plain fasl loads.
  (and (string= (prof-package-name name) "SB-C")
       (not (search "WITH-COMPILATION-UNIT" (prof-name-string name)))))

(defun prof-name-string (name)
  (let ((*package* (find-package :maxima))
        (*print-pretty* nil)
        (*print-length* nil)
        (*print-level* nil))
    (if (stringp name) name (prin1-to-string name))))

;;; Analysis

(defmacro prof-inc (key table &optional (n 'mult))
  `(incf (gethash ,key ,table 0) ,n))

(defun prof-sorted (table)
  (sort (loop for k being the hash-keys of table using (hash-value v)
              collect (cons k v))
        #'> :key #'cdr))

(defun prof-leaf-category (name)
  (let ((pkg (prof-package-name name)))
    (cond ((prof-maxima-name-p name) "maxima")
          ((string= pkg "<foreign>") (format nil "foreign: ~A" name))
          (t pkg))))

(defun prof-analyze (traces prefix total)
  (let ((*prof-name-cache* (make-hash-table :test 'eq))
        (self (make-hash-table :test 'equal))
        (cum (make-hash-table :test 'equal))
        (attr (make-hash-table :test 'equal))
        (attr-leaf (make-hash-table :test 'equal))
        (attr-caller (make-hash-table :test 'equal))
        (attr-cum (make-hash-table :test 'equal))
        (files (make-hash-table :test 'equal))
        (file-attr (make-hash-table :test 'equal))
        (leafcat (make-hash-table :test 'equal))
        (folded (make-hash-table :test 'equal))
        (compiler-callers (make-hash-table :test 'equal))
        (compiler-files (make-hash-table :test 'equal))
        (compiler 0)
        (gc 0)
        (in-testfile 0))
    (dolist (tr traces)
      (destructuring-bind (locs . mult) tr
        (let* ((names (loop for i from 0 below (length locs) by 2
                            collect (prof-name (aref locs i))))
               ;; NAMES is innermost first.
               (leaf (first names))
               (seen nil)
               (maxima-frames (remove-if-not #'prof-maxima-name-p names))
               (first-max (find-if (lambda (n)
                                     (and (prof-maxima-name-p n)
                                          (not (prof-testfile-name-p n))))
                                   names))
               (file (find-if #'prof-testfile-name-p names))
               (comp-pos (position-if #'prof-compiler-name-p names
                                      :from-end t))
               (gc-p (find-if (lambda (n)
                                (and (stringp n)
                                     (or (search "maybe_gc" n)
                                         (search "collect_garbage" n))))
                              names)))
          (declare (ignorable maxima-frames))
          (when leaf
            (prof-inc leaf self)
            (prof-inc (prof-leaf-category leaf) leafcat))
          (dolist (n names)
            (unless (member n seen :test #'equal)
              (push n seen)
              (prof-inc n cum)))
          (when file
            (incf in-testfile mult)
            (prof-inc file files))
          (cond (gc-p
                 (incf gc mult)
                 (prof-inc "<GC>" attr)
                 (when file (prof-inc (cons file "<GC>") file-attr)))
                (comp-pos
                 ;; Time inside SBCL's compiler: attribute to the
                 ;; innermost Maxima frame outside the compiler.
                 (incf compiler mult)
                 (let ((caller (find-if (lambda (n)
                                          (and (prof-maxima-name-p n)
                                               (not (prof-testfile-name-p n))))
                                        (nthcdr comp-pos names))))
                   (prof-inc (or caller "<none>") compiler-callers))
                 (when file (prof-inc file compiler-files))
                 (prof-inc "<SBCL compiler>" attr)
                 (when file
                   (prof-inc (cons file "<SBCL compiler>") file-attr)))
                (first-max
                 (prof-inc first-max attr)
                 (prof-inc (cons first-max leaf) attr-leaf)
                 (let* ((rest (cdr (member first-max names :test #'eq)))
                        (caller (find-if (lambda (n)
                                           (and (prof-maxima-name-p n)
                                                (not (equal n first-max))))
                                         rest)))
                   (prof-inc (cons first-max caller) attr-caller))
                 (when file (prof-inc (cons file first-max) file-attr)))
                (t
                 (prof-inc (format nil "<non-Maxima: ~A>"
                                   (prof-leaf-category leaf))
                           attr)))
          ;; Inclusive counts over Maxima frames only, outside compiler.
          (unless (or comp-pos gc-p)
            (let ((seen2 nil))
              (dolist (n names)
                (when (and (prof-maxima-name-p n)
                           (not (member n seen2 :test #'equal)))
                  (push n seen2)
                  (prof-inc n attr-cum)))))
          ;; Folded stacks, outermost first, for flame graphs.
          (prof-inc (format nil "~{~A~^;~}"
                            (mapcar (lambda (n)
                                      (substitute #\: #\;
                                                  (prof-name-string n)))
                                    (reverse names)))
                    folded))))
    (flet ((out (suffix fn)
             (with-open-file (s (concatenate 'string prefix suffix)
                                :direction :output :if-exists :supersede)
               (let ((*package* (find-package :maxima))
                     (*print-pretty* nil))
                 (funcall fn s))))
           (pct (n) (/ (* 100.0 n) (max total 1))))
      (out ".self.tsv"
           (lambda (s)
             (format s "# total ~D~%self	self%	cum	cum%	function~%" total)
             (loop for (k . v) in (prof-sorted self)
                   repeat 1000
                   do (format s "~D	~,2F	~D	~,2F	~A~%"
                              v (pct v) (gethash k cum 0)
                              (pct (gethash k cum 0)) (prof-name-string k)))))
      (out ".cum.tsv"
           (lambda (s)
             (format s "# total ~D~%cum	cum%	self	function~%" total)
             (loop for (k . v) in (prof-sorted cum)
                   repeat 1500
                   do (format s "~D	~,2F	~D	~A~%"
                              v (pct v) (gethash k self 0)
                              (prof-name-string k)))))
      (out ".attr.txt"
           (lambda (s)
             (format s "# Samples attributed to the innermost Maxima frame ~
                        (library time charged to its Maxima caller)~%")
             (format s "# total ~D samples~%~%" total)
             (let ((leaves (make-hash-table :test 'equal))
                   (callers (make-hash-table :test 'equal)))
               (loop for (k . v) in (prof-sorted attr-leaf)
                     do (push (cons (cdr k) v) (gethash (car k) leaves)))
               (loop for (k . v) in (prof-sorted attr-caller)
                     do (push (cons (cdr k) v) (gethash (car k) callers)))
               (loop for (k . v) in (prof-sorted attr)
                     repeat 300
                     do (format s "~7D ~6,2F%  ~A   (incl. ~,2F%)~%"
                                v (pct v) (prof-name-string k)
                                (pct (gethash k attr-cum 0)))
                        (loop for (leaf . n) in (sort (gethash k leaves) #'> :key #'cdr)
                              repeat 6
                              unless (equal leaf k)
                                do (format s "            leaf ~6,2F%  ~A~%"
                                           (pct n) (prof-name-string leaf)))
                        (loop for (caller . n) in (sort (gethash k callers) #'> :key #'cdr)
                              repeat 4
                              do (format s "            from ~6,2F%  ~A~%"
                                         (pct n) (prof-name-string caller)))))))
      (out ".attrcum.tsv"
           (lambda (s)
             (format s "# inclusive samples over Maxima frames, compiler excluded~%")
             (loop for (k . v) in (prof-sorted attr-cum)
                   repeat 1500
                   do (format s "~D	~,2F	~D	~A~%"
                              v (pct v) (gethash k attr 0)
                              (prof-name-string k)))))
      (out ".leafcat.tsv"
           (lambda (s)
             (loop for (k . v) in (prof-sorted leafcat)
                   do (format s "~D	~,2F	~A~%" v (pct v) k))))
      (out ".files.txt"
           (lambda (s)
             (format s "# samples per test file (~D of ~D in a test file)~%"
                     in-testfile total)
             (format s "# compiler samples total ~D (~,2F%)~%"
                     compiler (pct compiler))
             (format s "# GC samples total ~D (~,2F%)~%" gc (pct gc))
             (let ((per-file (make-hash-table :test 'equal)))
               (loop for (k . v) in (prof-sorted file-attr)
                     do (push (cons (cdr k) v) (gethash (car k) per-file)))
               (loop for (k . v) in (prof-sorted files)
                     do (format s "~%~7D ~6,2F%  ~A  (compiler ~,2F%)~%"
                                v (pct v) (subseq (symbol-name k) 9)
                                (pct (gethash k compiler-files 0)))
                        (loop for (f . n) in (sort (gethash k per-file) #'> :key #'cdr)
                              repeat 8
                              do (format s "            ~6,2F%  ~A~%"
                                         (pct n) (prof-name-string f)))))))
      (out ".compiler.tsv"
           (lambda (s)
             (format s "# compiler samples ~D (~,2F%) by Maxima caller~%"
                     compiler (pct compiler))
             (loop for (k . v) in (prof-sorted compiler-callers)
                   do (format s "~D	~,2F	~A~%" v (pct v) (prof-name-string k)))))
      (out ".folded"
           (lambda (s)
             (loop for (k . v) in (prof-sorted folded)
                   do (format s "~A ~D~%" k v)))))))

(defun prof-finish (prefix &key (graph t))
  (sb-sprof:stop-profiling)
  (let* ((end (prof-counters))
         (start *prof-start*)
         (traces (prof-collect graph))
         (total (loop for (nil . mult) in traces sum mult)))
    (flet ((d (key) (- (getf end key) (getf start key)))
           (sec (x) (/ x (float internal-time-units-per-second))))
      (with-open-file (s (concatenate 'string prefix ".summary")
                         :direction :output :if-exists :supersede)
        (format s "lisp ~A ~A~%" (lisp-implementation-type)
                (lisp-implementation-version))
        (format s "mode ~A interval ~A~%" *prof-mode* *prof-interval*)
        (format s "run-time ~,3F s~%real-time ~,3F s~%gc-time ~,3F s (~,1F% of run)~%"
                (sec (d :run)) (sec (d :real)) (sec (d :gc))
                (/ (* 100 (d :gc)) (max 1 (d :run))))
        (format s "bytes-consed ~:D~%" (d :bytes))
        (format s "samples ~D unique-traces ~D~%" total (length traces))
        (when (eq *prof-mode* :cpu)
          (format s "samples*interval ~,3F s~%" (* total *prof-interval*)))))
    (prof-analyze traces prefix total)
    (when graph
      (with-open-file (s (concatenate 'string prefix ".graph")
                         :direction :output :if-exists :supersede)
        (sb-sprof:report :type :graph :stream s))
      (with-open-file (s (concatenate 'string prefix ".flat")
                         :direction :output :if-exists :supersede)
        (sb-sprof:report :type :flat :stream s :max 1000)))
    (sb-sprof:reset)
    t))
