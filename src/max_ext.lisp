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
