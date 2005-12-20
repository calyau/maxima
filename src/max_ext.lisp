;;Autoloads for maxima
(in-package :maxima)


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
