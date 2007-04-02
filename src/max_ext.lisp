;;Autoloads for maxima
(in-package :maxima)

($auto_mexpr '$nusum "nusum")
($auto_mexpr '$unsum "nusum")
($auto_mexpr '$funcsolve "nusum")

($auto_mexpr '$bfzeta "bffac")
($auto_mexpr '$bfpsi "bffac")

(auto-mexpr '$trigrat '|trigrat|)
($auto_mexpr '$gcdex '|gcdex|)
($auto_mexpr '$expandwrt "simplification/stopex")
($auto_mexpr '$expandwrt_factored "simplification/stopex")
(declaim (special $expandwrt_denom $expandwrt_nonrat))

($auto_mexpr '$facsum "simplification/facexp")
($auto_mexpr '$factorfacsum "simplification/facexp")
($auto_mexpr '$collectterms "simplification/facexp")
(declaim (special $nextlayerfactor $facsum_combine))

($auto_mexpr '$disolate "simplification/disol")

($auto_mexpr '$linsimp "misc/declin")
($auto_mexpr '$declare_linear_operator "misc/declin")

($auto_mexpr '$nonumfactor "simplification/genut")
(meval '((%setup_autoload simp) &bffac $bfzeta))

;jfa
($auto_mexpr '$eigenvectors '|eigen|)
($auto_mexpr '$eigenvalues '|eigen|)

($auto_mexpr '$trigsimp "trgsmp.mac")
($auto_mexpr '$ode2 "ode2.mac")
($auto_mexpr '$ic1 "ode2.mac")
($auto_mexpr '$ic2 "ode2.mac")
($auto_mexpr '$bc2 "ode2.mac")

(dolist (v       
	  '($arite
	    $card_orbit
	    $card_stab
	    $comp2ele
	    $comp2pui
	    $cont2part
	    $contract
	    $direct
	    $ele2comp
	    $ele2polynome
	    $ele2pui
	    $elem
	    $explose
	    $kostka
	    $lgtreillis
	    $ltreillis
	    $mon2schur
	    $multi_elem
	    $multi_orbit
	    $multi_pui
	    $multinomial
	    $multsym
	    $orbit
	    $part2cont
	    $partpol
	    $permut
	    $polynome2ele
	    $prodrac
	    $pui
	    $pui2comp
	    $pui2ele
	    $pui2polynome
	    $pui_direct
	    $puireduc
	    $resolvante
	    $resolvante_alternee1
	    $resolvante_bipartite
	    $resolvante_diedrale
	    $resolvante_klein
	    $resolvante_klein3
	    $resolvante_produit_sym
	    $resolvante_unitaire
	    $resolvante_vierer
	    $schur2comp
	    $somrac
	    $tcontract
	    $tpartpol
	    $treillis
	    $treinat
	    ))
  (setf (get v 'autoload)        "sym.mac")
  )

(dolist (f       
     '($close
       $flength
       $fposition
       $freshline
       $newline
       $opena
       $openr
       $openw
       $readline
       $alphacharp
       $alphanumericp
       $ascii
       $cequal
       $cequalignore
       $cgreaterp
       $cgreaterpignore
       $charp
       $cint
       $clessp
       $clesspignore
       $constituent
       $cunlisp
       $digitcharp
       $lcharp
       $lowercasep
       $uppercasep
       $sunlisp
       $lstringp
       $stringp
       $charat
       $charlist
       $parsetoken
       $sconc
       $scopy
       $sdowncase
       $sequal
       $sequalignore
       $sexplode
       $simplode
       $sinsert
       $sinvertcase
       $slength
       $smake
       $smismatch
       $split
       $sposition
       $sremove
       $sremovefirst
       $sreverse
       $ssearch
       $ssort
       $ssubst
       $ssubstfirst
       $strim
       $striml
       $strimr
       $substring
       $supcase
       $tokens ))
  (autof f "stringproc"))

;; $printf doesn't work with autol.lisp/autom when calling with streams true and false
;; don't know if (funcall ... in autom can be replaced by (apply ... 
;; so here is an intermediate workaround   VvN
(let ((mf '$printf))
  (unless (fboundp mf)
    (setf (macro-function mf)
          #'(lambda (&rest l)
              (aload "stringproc")
              (apply (macro-function mf) l)))))  

(dolist (f       
     '($read_matrix
       $read_lisp_array
       $read_maxima_array
       $read_hashed_array
       $read_nested_list
       $read_list
       $write_data ))
  (autof f "numericalio"))

(autof '$eval_string "eval_string")
(autof '$parse_string "eval_string")


;; begin functions from share/linearalgebra 

; loading linearalgebra.mac loads the complete linearalgebra stuff
(defun autof-linearalgebra (fun)
  (unless (fboundp fun)
    (setf (symbol-function fun)
        #'(lambda (&rest l)
         ($aload_mac "linearalgebra")
         (apply fun l)))))

(dolist (f       
     '($eigens_by_jacobi   ; eigens-by-jacobi.lisp
     
       $cholesky           ; linalgcholesky.lisp
       
       $circulant          ; linalg-extra.lisp
       $cauchy_matrix
       $hessian
       $jacobian
       $matrix_sign
       
       $blockmatrixp       ; linalg-utilities.lisp
       $ctranspose
       $identfor
       $matrix_size
       $mytest
       $require_list 
       $require_matrix
       $require_nonempty_matrix 
       $require_posinteger
       $require_selfadjoint_matrix
       $require_square_matrix
       $require_symmetric_matrix
       $require_unblockedmatrix 
       $zerofor
       $zeromatrixp
       
       $get_lu_factors     ; lu.lisp
       $invert_by_lu 
       $linsolve_by_lu
       $lu_backsub
       $lu_factor
       $mat_cond
       
       $matrixexp          ; matrixexp.lisp
       $matrixfun
       $spectral_rep
       
       $addmatrices        ; mring.lisp
       $require_ring
       
       $nonnegintegerp     ; polynomialp.lisp
       $polynomialp ))
  (autof-linearalgebra f))

(let ((fun '$ringeval))    ; mring.lisp
  (unless (get fun 'mfexpr*)
    (setf (get fun 'mfexpr*)
     #'(lambda (l)
         ($aload_mac "linearalgebra")
         (funcall (get fun 'mfexpr*) l)))))

(dolist (mexpr       
     '($column_reduce      ;linearalgebra.mac
       $columnop
       $columnspace 
       $columnswap
       $diag_matrix
       $dotproduct 
       $good_pivot
       $hankel
       $hilbert_matrix
       $hipow_gzero
       $kronecker_product
       $locate_matrix_entry
       $mat_fullunblocker
       $mat_norm
       $mat_trace
       $mat_unblocker
       $nullity
       $nullspace
       $orthogonal_complement
       $polytocompanion
       $ptriangularize
       $ptriangularize_with_proviso
       $rank
       $request_rational_matrix
       $require_integer
       $require_symbol
       $rowop
       $rowswap
       $toeplitz
       $vandermonde_matrix ))
  ($auto_mexpr mexpr "linearalgebra"))

;; end functions from share/linearalgebra


'$parametric
'(defvar $plot_options '((mlist)
					;((mlist) $x -3 3)
					;((mlist) $y -3 3)
			 ((mlist) $grid 30 30)
			 ((mlist) $view_direction 1 1 1)
			 ((mlist) $colour_z nil)
			 ((mlist) $transform_xy nil)
			 ((mlist) $run_viewer t)
			 ((mlist) $plot_format $openmath)
			 ((mlist) $nticks 100)
			 ))
;;for hypgeo.lisp
'($%y $%k $%j)
