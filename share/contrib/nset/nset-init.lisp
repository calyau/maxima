;; This version of setup_autoload uses file_search.

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
	(load file) ($batchload file))))

;; Add set to Maxima's prop list.

(add2lnc '$set $props)

;; Autoload the functions in nset.

($setup_autoload "nset" '$adjoin '$cardinality '$complement 
'$cartesian_product, '$disjointp '$dupe '$elementp '$equiv_classes 
'$extremal_subset '$flatten  '$full_listify '$fullsetify '$intersect 
'$intersection '$listify  '$partition_set '$permutations '$powerset 
'$setdifference '$setequality  '$setify '$setp '$subpowerset '$subset 
'$subsetp '$symmdifference '$union)
