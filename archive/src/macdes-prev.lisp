;;; -*-  Mode: Lisp; Package: Maxima; Syntax: Common-Lisp; Base: 10 -*- ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;     The data in this file contains enhancments.                    ;;;;;
;;;                                                                    ;;;;;
;;;  Copyright (c) 1984,1987 by William Schelter,University of Texas   ;;;;;
;;;     All rights reserved                                            ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "MAXIMA")

(defconstant *doc-start* (code-char 31))

(defun $example (item &optional (file
				 (merge-pathnames "manual.demo"
						  $describe_documentation))
		      )
  (and (symbolp file) (setq file (stripdollar file)))
  (or (probe-file file)
      (return-from $example "Please supply a file name as the second arg"))
  (and (symbolp item) (setq item (symbol-name item))
       (setq item (subseq item 1))
       (with-open-file
	(st file)
	(sloop with tem
	       while (setq tem (read-char st nil))
	       do
	       (cond ((and (eql tem #\&)
			   (eql (setq tem (read-char st nil)) #\&))
		      (cond
		       ((and (symbolp (setq tem (read st nil)))
			     (string-search item (symbol-name tem)))
			(format t "~%Examples for ~a :~%" tem)
			;; This code fulls maxima into thinking that it just
			;; started, by resetting the values of the special
			;; variables $labels and $linenum to their initial
			;; values. They will be reset just after $example
			;; is done. The d-labels will also not be disturbed
			;; by calling example.
		        (progv 
		         ;; Protect the user labels and variables 
			 ;; from being voerwritten by creating a new 
			 ;; binding.
		         (append '($linenum
				   $labels
				   $values)
				 (cdr $labels)
				 (cdr $values))
			 (list 1
			       '((mlist simp))
			       '((mlist simp)))
		         ;; Run the example.
			 (unwind-protect
			     (sloop until
				    (or (null (setq tem (peek-char nil st nil)))
					(eql tem #\&))
				    for expr = (mread st nil)
				    do
				    (let ($display2d) (displa (third  expr)))
				    ;; Make the c-label and d-label.
				    (let ((c-label (makelabel $inchar))
					  (d-label (makelabel $outchar)))
				      ;; Set the c-label to the input
				      ;; expression.
				      (set c-label (third expr))
				      (format t "<~d>==>" $linenum)
				      (displa (setq $% (meval* (third  expr))))
				      (set d-label $%)
				      (incf $linenum)
				      ))
			   ;; Clean-up form, which will be
			   ;; evaluated even if an error occurs,
			   ;; because of unwind-protect.  Kill
			   ;; all labels and values used the
			   ;; example. This is harmless, because
			   ;; the local binding established with
			   ;; progv is in effect.
			   (mapc #'makunbound
				 (append
				  (cdr $labels)
				  (cdr $values))
				 )))))))))))

  
(eval-when (compile eval load)

(defvar *pdp-describe* t "The file positioning is not so good for
 pdp type servers, because of character translation so we index more carefully.
 This should be ok for any file system")

;;this should really be in "macsyma-source:macsyma;macsym.doc" but I don't
;;want to copy it for here
     

(defmvar $describe_documentation #+lispm
  "maxima-documentation:maxima;macsym.doc"
  #-lispm "/usr/public/maxima/doc/macsym.doc"
  "This is the name of the main macsyma documentation file")



;;keep documentation on logical host maxima-documentation
(defmvar $all_macsyma_documentation
  (list'(mlist)
       $describe_documentation
       ))


;(defstruct (describe-index :named (:conc-name index-))
; documentation-file
;  entries)


(defstruct (describe-index (:type list) :named (:conc-name index-))
  documentation-file
  entries)

(defun make-entry (&key string file-pointer)
  (cons string file-pointer))

(defmacro key-string (x) `(car ,x))

(defmacro key-file-pointer (x) `(cdr , x))

;;The following assumes that :set-pointer will do something reasonable as
;;it does currently under chaos.  When and if we switch to tcp that will create
;;problems.  The reasonable strategy is then to switch to a line number
;;index and use a number of smaller files.  It took 37 seconds to read
;;once through the documentation file using tcp on the r20.

(defmacro read-1-char (st)
  `(let ((tem  (read-byte ,st nil nil)))
     (if tem (code-char tem))))


#+ti 
(defun file-position (a &optional b)
  (if b (send a :set-pointer b) (send a :read-pointer)))

(defun file-options (pathname) pathname
  #-lispm  (list  nil  8)
  #+lispm
   (case (send    (send pathname :host) :system-type)
	    (:lispm (list  nil  8))
	    (:tops-20 (error " can't use any more")(list  nil  7))
	    (t (format t "~%Assuming byte-size 8 for making index") (list  nil  8)  )))

(defun index-file  ( file &aux
			  word (ch 0) cha   fil options ind)
  (setq fil (probe-file file))
  (setq options (file-options fil))
  (with-open-file
      (st file :element-type #+symbolics '(unsigned-byte 8)
	  #-symbolics '(mod 256))
    (sloop while  ch
	   when (eql (setq ch  (read-1-char st)) *doc-start* )
	   do
	   ;;the first character after *doc-start* is used to say what
	   ;;kind of documentation this is.  This is compatible with
	   ;;the gnu doc string files.
	   (read-1-char st)
	   (setq ind (file-position st))
	   (setq word (with-output-to-string
			(strin )
			(sloop  until (zl-MEMBER
					(progn   
					  (setq cha (read-1-char st))
					  )
					'(#\space #\return #\newline nil)
					)
				do
				(tyo cha strin)
				finally  (if cha (unread-char cha st))
				)))
	   and
	   collecting   (make-entry :string
				    (cond (
					   (array-has-fill-pointer-p word)
					   (subseq word 0
						   (length (the string  word))))
					  (t word))
				    :file-pointer ind) into llist
	   finally
	   (print (length llist))
	   (return (make-describe-index
			     :documentation-file
			     (namestring file)
			     :entries  llist)))))

(defun font-number (ch)
  (cond ((char> ch #\*) (f- (char-code ch)#. (char-code #\0)))
	(t 0)))

(defun line-in (stream buffer end-char)
  (let ((tem (or buffer
		 (make-array 70 :fill-pointer 0 :element-type ' #.(array-element-type "abc")
			     :adjustable t
			     ))))
    (sloop with ch until
	  (eql (setq ch (read-1-char stream)) end-char)  
	  do (if (null ch) (loop-finish))
	  (vector-push-extend ch tem))
    tem))



(defun read-item (stream file-pos  &key (up-to   *doc-start*) item (out-stream *standard-output*) &aux (prev-ch) (ch 0) lin)
  ;;have to set pointer to 0 for non lispm server.
  (cond ( *pdp-describe* (file-position stream 0)))
  (file-position stream file-pos)
  (setq lin (line-in stream nil #\newline))
  (cond ((and item  (null (string-search item lin))
	      (y-or-n-p "~%Bad index file.  Try making a new one?"))
	 (set-up-index (pathname stream) :make-new-one t)
	 (error "now start over"))
	(t (format out-stream "~%~A~%" lin)))
  (sloop until (and (eql (setq ch (read-1-char stream))    ;(send stream :tyi))
		  up-to ) (eql prev-ch #\newline))
	 while ch
	do (setq prev-ch ch)
	#-symbolics
	(cond ((char= ch #\)
	       (send stream :set-current-font (setq prev-ch (tyi stream)))))
	(tyo ch out-stream) ; (send out-stream :tyo ch)
	))

(defvar *describe-indices* nil)
;;convention:use the same name for the documentation file as the index-name
(defun add-to-describe-indices (index-name describe-index &aux tem)
  (declare (special *index-path*))
  (setq index-name (string-downcase index-name))
  (and (boundp '*index-path*)
    (setf (index-documentation-file describe-index)
	(alter-pathname *index-path* :name index-name :type "doc")))
  (cond ((setq tem (zl-MEMBER index-name *describe-indices*))
	 (setf (second tem) describe-index))
	(t (push describe-index *describe-indices*)
	   (push index-name *describe-indices*))))

(defun set-up-index (file &key write-file-name make-new-one
			  &aux index-name index tem)
  (setq file (pathname file))
  #+kcl
  (or (probe-file file)
      (and (setq tem (alter-pathname file :directory
				     (append
				      (pathname-directory
				       (pathname si::*system-directory*))
				      (pathname-directory "../doc/"))))
	   (probe-file tem)
	   (setq file tem)
	   ))
  
;  (setq index-name (string-upcase (pathname-name file)))
  (cond ((null write-file-name)
	 (setq write-file-name
	       (format nil "~a~a-index.~a"
		       *describe-index-directory*
		       (pathname-name file) *index-file-type*)))
	(t (setq write-file-name (alter-pathname write-file-name :type *index-file-type*))))
  (cond ((or make-new-one (null  (probe-file write-file-name)))
	 (format t "~%Having to make a new index...")
	 (setq index  (index-file file))
	 (add-to-describe-indices index-name  index)
	 (write-forms-to-file write-file-name 
			      (lisp:LIST (lisp:LIST 'add-to-describe-indices
							index-name
							(lisp:LIST
							  'QUOTE index)))
			      :in-package "MAXIMA" :type *index-file-type*))
	(t
	 (let ((*index-path* write-file-name))
	   (declare (special *index-path*))
	   (load write-file-name)))))




(defun write-forms-to-file
  (file-name forms   &key
	     ( in-package *package*) (type :lisp) &aux #+lispm tem) 
  (cond 
    ((eq type :lisp)
     (with-open-file (st (alter-pathname file-name :type "LISP") :direction :output)
       (format st ";;; - * - Mode:Lisp; Package:~A;Syntax:common-lisp  - * -~%" in-package)
       (format st "~%(in-package \"~a\")~%" in-package)
       (prin1 (cons 'progn forms) st) (pathname st)))
    #+lispm
    ((member type '(:bin :xfasl))
     (si::dump-forms-to-file (setq tem (alter-pathname file-name :type type))
       forms (list :package in-package)) tem)))


(defmvar *index-file-type* :lisp)

(defvar *describe-index-directory* #-lispm "/usr/public/maxima/doc/"
    #+lispm "cl-maxima-object:maxima;")


(defun cl-string (zl-string &aux (leng (length zl-string))  answ)
       (cond ((and  (> leng 0)
	      (not (integerp(aref zl-string 0)))) zl-string)
	     (t (setq answ (make-array leng :element-type ' #.(array-element-type "abc")))
		(sloop for i below leng
		      do (setf (aref answ i) (code-char (aref zl-string i))))
		answ)))

(defun add-main-macsyma-documentation ( &aux  file-name)
	 (sloop for v in (cdr $all_macsyma_documentation)
	       do
	       (setq file-name (pathname v) )
	       (cond ((not (MEMBER (pathname-name file-name)
			       *describe-indices* :test 'equalp))
		      (set-up-index file-name)))))
								       
(defun maxima-union (&rest lists &aux test)
  (setq test (or (second (memq :test lists)) #'eq))
  (sloop for v in lists until (eql v :test)
	with answ = nil
	do (sloop for vv in v do (pushnew vv answ :test test))
 (return answ)))

(defun $describe ( items-to-describe &key editor
				     (index-names 'use-all)
				     &aux items done zl-SOME  all-files)
  (cond ((not (consp items-to-describe))
	 (setq items-to-describe (list items-to-describe))))
  (setq items-to-describe (sloop for v in items-to-describe
				 collecting (string-trim "&$" v)))
  (add-main-macsyma-documentation)
  (setq items (sloop for (name this-ind) on *describe-indices* by 'cddr  
		     when (or (eq index-names 'use-all)
			      (zl-MEMBER name index-names))
		     appending
		     (sloop for u in (index-entries this-ind)
			    when (sloop for v in items-to-describe
					when (string-search v (car u))
					do (loop-return t))
			    collecting
			    (cons (index-documentation-file this-ind) u))))

  (sloop for v in items
	 for i from 0
	 do (format *terminal-io* "~%~3D: ~A" i (key-string (cdr v))))
  (cond (items
	 (sloop until done
		do
		(let ((*standard-input* *query-io*))
		  #+lispm(send *query-io* :send-if-handles :fresh-line)
		  #-lispm (terpri)
		  (format *query-io* "Enter a number, or a Maxima list of numbers,  all or none:")
;		  (setq zl-SOME (mread-noprompt ))
		  (setq zl-SOME (let ((*mread-prompt* ""))
				  (dbm-read *standard-input* nil nil)))
		  (print (list 'zl-some zl-some))
		  )
		(cond ((atom zl-SOME)
		       (cond ((numberp zl-SOME) (setq zl-SOME (list zl-SOME)))
			     ((eq zl-some '$all)
			      (setq zl-SOME (sloop for i below
						   (length items)
						   collecting i)))
			     ((eq '$none zl-some) (setq zl-SOME nil))))
		      (($listp zl-SOME)(setq zl-SOME (cdr zl-SOME))))
		(cond ((null zl-SOME) (setq done t))
		      ((numberp (car zl-SOME))
		       (setq items (sloop for i in zl-SOME
					  collecting  (nth i items)))
		       (setq all-files (maxima-union (mapcar 'car items)  :test 'equalp))
		       (sloop for fil in all-files
			      do (read-documentation
				  (sloop for it in items
					 when (equal (car it) fil)
					 collecting (cdr it))
				  fil :editor editor))
		       (setq done t)))))
	(t (format t "~%No keys contain the strings ~A" items-to-describe)))
  '$done)

(setf (symbol-function '$apropos) #'$describe)
(defun read-documentation (items file &key editor) editor
  (cond
    ((null items) nil)
    (t
     (with-open-file (stream file :element-type #+symbolics '(unsigned-byte 8) #-symbolics '(mod 256)
			     ;:byte-size 8.
			     )
       (let ((output (cond #+lispm
			   (editor
			    (zwei::REST-OF-INTERVAL-STREAM (zwei::POINT)))
			   (t *standard-output*))))
	 (sloop for v in items
	       do (read-item  stream (cdr v)
			      :out-stream output :item (car v))))))))

	
(defun search-file ( expr &optional (file $describe_documentation) &aux (ch 0) cha leng)
  (with-open-file (st file)
    (setq leng   (send st :length))
    (sloop while ch
	  when (eql  (setq ch(send st :tyi)) *doc-start*)
	  do (setf (fill-pointer *word* )0)
	  (with-output-to-string
	    (strin *word* )
	    (sloop  until (zl-MEMBER (setq cha (send st :tyi))
				   '(#\space #\return #\newline nil))
		   do
		  (send strin :tyo cha)))
	  and
	  when(string-search expr *word*)
	  do
	  (sloop while cha until (eql (setq cha (send st :tyi)) *doc-start*)
		do
		(send *standard-output* :tyo cha)))))

#+cl
(defun get-next-keyword (stream &aux cha (ch 0))
  (setf (fill-pointer *word*) 0)
  (sloop while ch when 
	(eql (setq ch (tyi stream )) *doc-start*)
	    do 
	    (with-output-to-string
	      (strin *word* )
	      (sloop until (zl-MEMBER (setq cha (tyi stream))
				     '(#\space #\return #\newline))
		    do
		    (tyo cha strin)
		    ))
	    (return (values *word*))))
#-cl
(defun get-next-keyword (stream &aux cha (ch 0))
  (setf (fill-pointer *word*) 0)
  (sloop while ch when 
	(eql (setq ch (send stream :tyi)) *doc-start* )
	do 
	(with-output-to-string
	  (strin *word* )
	  (sloop until (zl-MEMBER (setq cha (send st :tyi))
			      '(#\space #\return #\newline))
		do
		(send strin :tyo cha)))
	 (return (values *word*))))

)



(defun mread-noprompt (&rest read-args)
  (let ((*mread-prompt* ""))
    (declare (special *mread-prompt*))
    (or read-args (setq read-args (list *query-io*)))
  (caddr (apply #'mread read-args))))


;;;some functions for converting from the DOE style documentation file
;;;to the type we use. DO-IT is the function that does it.
;;in our documentation files the key always follows a a \n *doc-start*


;(defun my-alphabetp (ch)
;  (or (maxima:alphabetp ch)
;      (memq ch '(#/")
;	    )))
;(defun read-from-stream-for-fix (stream to-string prev-name left-over &aux (from-file t) in-white info (spaces 0) tem eof ch)
;  (setf (fill-pointer to-string) 0)
;  (cond (prev-name
;	 (with-output-to-string (str to-string)
;	   (cond ((and left-over (> (length (the string  left-over)) 0)
;		       (eql (aref left-over 0) #/())
;		     (format str "~%~%&~s" prev-name))
;		 (t   (format str "~%~%&~s " prev-name))))))
;  (cond (left-over
;	 (with-output-to-string (str to-string)
;	   (format str "~A" left-over))))
;;  (cond ((setq tem (string-search "                               " to-string))
;;	 (show to-string)
;;	  (setq spaces 20)(setq from-file nil)(setf (fill-pointer to-string) tem)))
;  (sloop named sue
;	when (> spaces 40)
;	  do  (setq in-white (fill-pointer to-string))
;	      (cond (from-file
;		     (sloop with past-space
;			   while (setq ch (send stream :tyi))
;			   do (vector-push-extend  ch to-string)
;			   until (memq ch  '(#\linefeed #\newline))
;			   when (not (eql ch #\space))
;			     do(setq past-space t)
;			   until (and past-space (eql ch #\space))))
;		    (t (setq from-file t)))
;	      (multiple-value-bind (answ left-over end) (check-for-end to-string in-white)
;		(cond (answ (setf (fill-pointer to-string) end)
;			    (return-from sue  (values answ left-over)))))
;	while (setq ch (send stream :tyi))
;	do (vector-push-extend  ch to-string)
;	when (eql ch #\space)
;	  do
;	    (incf spaces)
;	else do (setq spaces 0)))
;
;;;for copying documentation file
;
;(defun do-it (strin in-file out-file &aux next-name left-over )
;  (with-open-file
;    (st  in-file
;;        (st  "isaac:>wfs>doc.tem")
;    (with-open-file (st1 out-file '(:out))
;    (sloop ;for i below n
;	  do
;      (multiple-value-setq (next-name left-over)
;	  (read-from-stream-for-fix st strin next-name left-over ))
;	  while next-name do
;;      (format t "~%~S" next-name)
;      (format st1 "~A" strin)
;             ))))
;;(do-it *strin*)
;(defvar *strin* (make-array 100 :type 'art-string :fill-pointer 0))
;
;(defun not-white-space-p (n) (cond ((not (memq n '(#\space #\newline ))) n)))
;(defun advance-over-white-space (string start &optional reversep &aux ch )
;  (cond (reversep (sloop for i downfrom start to 0
;				until (setq ch (not-white-space-p (aref string i)))
;	finally (return (and ch (add1 i)))))
;	(t (sloop for  i from start below (length (the string  string))
;	until (setq ch (not-white-space-p (aref string i)))
;	finally (return (and ch i))))))
;(defun read-atom-from-string (string start)
;  (declare (values contents end-char-position))
;  (condition-case (condit)
;      (progn (read-from-string string nil start))
;      (sys:read-package-not-found nil)))
;


;; Some list creation utilities.


(defmacro $create_list(form &rest l)
  `(create-list2 ',form ',l))

(defun create-list2 (form l)
  (cons '(mlist) (apply 'create-list1 form l)))

(defun create-list1(form &rest l &aux lis var1 top)
  (cond ((null l)(list (meval* form)))
	(t
	 (setq var1 (car l)
	       lis (second l)
	       l (cddr l))
	 (or (symbolp var1) (merror "~a not a symbol" var1))
 	 (setq lis (meval* lis))
	 (progv (list var1)
		(list nil)
		(cond ((and (numberp lis)
			    (progn
			      (setq top (car l) l (cdr l))
			      (setq top (meval* top))
			      (numberp top)))
		       (sloop for i from lis to top
			      nodeclare t
			      do (set var1 i)
			      append
			      (apply 'create-list1
				     form l)))
		      (($listp lis)
		       (sloop for v in (cdr lis)
			      do (set var1 v)
			      append
			      (apply 'create-list1
				     form l)
			      ))
		      (T (merror "BAD ARG")))))))
