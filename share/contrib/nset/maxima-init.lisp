;; -------start of *optional* stuff---------------------------------

;; Set verbose and print to true and load a file.

(defun $xload (f)
  (load ($file_search f) :verbose 't :print 't))

;; If file fn (with _no_ file extension) is out of date, compile it. We 
;; assume that the extension of the source file is "lisp". 

(defun $make (fn)
  (setq fn (string-left-trim "&" (string fn)))
  (let ((fl) (fb) 
	(bin-ext #+gcl "o"
		 #+cmu (c::backend-fasl-file-type c::*target-backend*)
		 #+clisp "fas"
		 #+allegro "fasl"
		 #-(or gcl cmu clisp allegro) ""))
    (setq fl (concatenate 'string (coerce fn 'string) ".lisp"))
    (setq fl ($file_search fl))
    (setq fb (concatenate 'string (coerce fn 'string) "." bin-ext))
    (setq fb ($file_search fb))
    (if (or (and fl (not fb)) 
	    (and fl fb (> (file-write-date fl) (file-write-date fb))))
	($compile_file fl))))

;; If you place nset in a directory that Maxima can't find,
;; you'll need to append its directory to $file_search_lisp. To do this,
;; replace the path "l://nset-1.2.03" with the correct path
;; for your machine.  

;; To allow set member access using [], load the file mapply. This 
;; file should be compiled -- if it isn't, many Maxima functions
;; (not just nset functions) will be slowed down.

;; The make function recompiles nset automatically on startup if 
;; the binary file is older than nset.lisp. Unless you
;; anticipate making changes to nset, you may comment out
;; ($make "nset")

(eval-when (load compile eval)
  (setq $file_search_lisp ($cons "l://nset-1.2.03/###.{x86f,lisp}"
				 $file_search_lisp))
  ($make "nset"))

;;-----start of required stuff---------------------------------------

(defun $setup_autoload (filename &rest functions)
  (let ((file ($file_search filename)))
    (dolist (func functions)
      (nonsymchk func '$setup_autoload)
      (putprop (setq func (dollarify-name func)) file 'autoload)
      (add2lnc func $props)))
  '$done)

;; This version of generic-autoload in Maxima 5.9.0 doesn't recognize
;; "fas", "fasl" and "x86f" as valid extensions for compiled Lisp
;; code.

#+cl
(defun generic-autoload (file &aux type)
  (setq file (pathname (cdr file)))
  (setq type (pathname-type file))
  (let ((bin-ext #+gcl "o"
		 #+cmu (c::backend-fasl-file-type c::*target-backend*)
		 #+clisp "fas"
		 #+allegro "fasl"
		 #-(or gcl cmu clisp allegro) ""))
    (if (member type (list bin-ext "lisp" "lsp")  :test 'equalp)
	(load file :verbose 't) ($batchload file))))

;; Autoload the nset functions that simplify.

(add2lnc '$set $props)
(defprop $set simp-set operators)
(autof 'simp-set '|nset|)

(add2lnc '$kron_delta $props)
(defprop $kron_delta simp-kron-delta operators)
(autof 'simp-kron-delta '|nset|)

(add2lnc '$belln $props)
(defprop $belln simp-belln operators)
(autof 'simp-belln '|nset|)

(add2lnc '$divisors $props)
(defprop $divisors simp-divisors operators)
(autof 'simp-divisors '|nset|)

(add2lnc '$moebius $props)
(defprop $moebius simp-moebius operators)
(autof 'simp-moebius '|nset|)

(add2lnc '$stirling1 $props)
(defprop $stirling1 simp-stirling1 operators)
(autof 'simp-stirling1 '|nset|)

(add2lnc '$stirling2 $props)
(defprop $stirling2 simp-stirling2 operators)
(autof 'simp-stirling2 '|nset|)

;; Autoload non-simplifying functions in nset.

($setup_autoload "nset" 
		 '$adjoin 
		 '$cardinality 
		 '$cartesian_product 
		 '$disjoin
		 '$disjointp 
		 '$elementp
		 '$emptyp
		 '$equiv_classes 
		 '$extremal_subset 
		 '$flatten  
		 '$full_listify 
		 '$fullsetify 
		 '$intersect 
		 '$intersection
		 '$integer_partitions
		 '$listify  
		 '$makeset
		 '$num_distinct_partitions
		 '$num_partitions
		 '$partition_set 
		 '$permutations 
		 '$powerset 
		 '$rreduce
		 '$lreduce
		 '$xreduce
		 '$setdifference 
		 '$set_partitions
		 '$setify 
		 '$setp 
		 '$subset 
		 '$subsetp 
		 '$symmdifference 
		 '$union)

