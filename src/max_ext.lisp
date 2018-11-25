;;Autoloads for maxima
(in-package :maxima)

($auto_mexpr '$nusum "nusum")
($auto_mexpr '$unsum "nusum")
($auto_mexpr '$funcsolve "nusum")

($auto_mexpr '$bffac "bffac")
($auto_mexpr '$cbffac "bffac")
($auto_mexpr '$bfzeta "bffac")
($auto_mexpr '$bfpsi "bffac")
($auto_mexpr '$bfpsi0 "bffac")

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
(meval '((%setup_autoload simp) "bffac" $bfzeta))

;jfa
($auto_mexpr '$eigenvectors "eigen.mac")
($auto_mexpr '$eivects "eigen.mac")
($auto_mexpr '$eigenvalues "eigen.mac")
($auto_mexpr '$eivals "eigen.mac")

($auto_mexpr '$trigsimp "trgsmp.mac")
($auto_mexpr '$ode2 "ode2.mac")
($auto_mexpr '$ic1 "ode2.mac")
($auto_mexpr '$ic2 "ode2.mac")
($auto_mexpr '$bc2 "ode2.mac")
($auto_mexpr '$desimp "ode2.mac")
($auto_mexpr '$linear2 "ode2")

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
     '($adjust_external_format
       $close
       $flush_output
       $flength
       $fposition
       $freshline
       $newline
       $opena
       $openr
       $openw
       $make_string_input_stream
       $make_string_output_stream
       $get_output_stream_string
       $stderr
       $stdin
       $stdout
       $printf
       $sprint
       $writebyte
       $readbyte
       $readchar
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
       $stringp
       $charat
       $charlist
       $eval_string
       $parse_string
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
       $tab
       $tokens
       $unicode
       $unicode_to_utf8
       $utf8_to_unicode
       $base64
       $base64_decode
       $crc24sum
       $md5sum
       $mgf1_sha1
       $number_to_octets
       $octets_to_number
       $octets_to_oid
       $oid_to_octets
       $octets_to_string
       $string_to_octets
       $sha1sum
       $sha256sum
       $inferred_encoding
       $recognized_encoding_p))
  (setf (get f 'autoload) "stringproc"))


(dolist (f       
  '($regex_compile
    $regex_match_pos
    $regex_match
    $regex_split
    $regex_subst_first
    $regex_subst
    $string_to_regex ))
  (setf (get f 'autoload) "sregex"))


(setf (get '$romberg 'autoload) "romberg")

(dolist (f
  '($assume_external_byte_order
    $opena_binary
    $openr_binary
    $openw_binary
    $read_array
    $read_binary_array
    $read_binary_list
    $read_binary_matrix
    $read_hashed_array
    $read_lisp_array
    $read_list
    $read_matrix
    $read_maxima_array
    $read_nested_list
    $write_binary_data
    $write_data))
  (setf (get f 'autoload) "numericalio"))

(dolist (f       
  '($bit_and
    $bit_length
    $bit_lsh
    $bit_not
    $bit_onep
    $bit_or
    $bit_rsh
    $bit_xor ))
  (setf (get f 'autoload) "bitwise"))


;; functions from share/linearalgebra 
(dolist (f       
     '($eigens_by_jacobi       ; eigens-by-jacobi.lisp
     
       $cholesky               ; linalgcholesky.lisp
       
       $circulant              ; linalg-extra.lisp
       $cauchy_matrix
       $hessian
       $jacobian
       $krylov_matrix
       $matrix_sign
       $sylvester_matrix
       $vandermonde_matrix
       
       $blockmatrixp           ; linalg-utilities.lisp
       $ctranspose
       $identfor
       $matrix_size
       $require_list 
       $require_matrix
       $require_nonempty_matrix 
       $require_posinteger
       $require_real_symmetric_matrix
       $require_selfadjoint_matrix
       $require_square_matrix
       $require_symmetric_matrix
       $require_unblockedmatrix 
       $zerofor
       $zeromatrixp
       
       $determinant_by_lu        ; lu.lisp
       $get_lu_factors 
       $invert_by_lu 
       $linsolve_by_lu
       $lu_backsub
       $lu_factor
       $mat_cond
       
       $matrixexp              ; matrixexp.lisp
       $matrixfun
       $spectral_rep
       
       $addmatrices            ; mring.lisp
       $require_ring
       
       $nonnegintegerp         ; polynomialp.lisp
       $polynomialp ))
  (setf (get f 'autoload) "linearalgebra"))

(dolist (mexpr       
     '($column_reduce          ; linearalgebra.mac
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
       $linalg_rank
       $locate_matrix_entry
       $mat_fullunblocker
       $mat_norm
       $mat_trace
       $mat_unblocker
       $moore_penrose_pseudoinverse
       $nullity
       $nullspace
       $orthogonal_complement
       $polytocompanion
       $ptriangularize
       $ptriangularize_with_proviso
       $request_rational_matrix
       $require_integer
       $require_symbol
       $rowop
       $rowswap
       $toeplitz))
  ($auto_mexpr mexpr "linearalgebra"))


(dolist (f
     '($assoc_legendre_p
       $assoc_legendre_q
       $chebyshev_t
       $chebyshev_u
       $gen_laguerre
       $hermite
       $intervalp
       $jacobi_p
       $laguerre
       $legendre_p
       $legendre_q
       $orthopoly_recur
       $orthopoly_weight
       $pochhammer
       $spherical_bessel_j
       $spherical_bessel_y
       $spherical_hankel1
       $spherical_hankel2
       $spherical_harmonic
       $ultraspherical))
  (setf (get f 'autoload) "orthopoly"))

(defprop $unit_step simp-unit-step operators)
(autof 'simp-unit-step "orthopoly")

(defprop $pochhammer simp-pochhammer operators)
(autof 'simp-pochhammer "orthopoly")

(dolist (f
  '($draw
    $draw2d
    $draw3d
    $set_draw_defaults
    $multiplot_mode
    $make_level_picture
    $make_rgb_picture
    $get_pixel
    $take_channel))
  (setf (get f 'autoload) "draw"))

(dolist (f
  '($julia
    $mandelbrot
    $plotdf
    $ploteq
    $rk))
  (setf (get f 'autoload) "dynamics"))

(dolist (mexpr       
  '($evolution
    $staircase
    $evolution2d
    $chaosgame
    $ifs
    $orbits))
  ($auto_mexpr mexpr "dynamics"))

(setf (get '$scene 'autoload) "visualization")

(defprop $hypergeometric simp-hypergeometric operators)
(autof 'simp-hypergeometric "hypergeometric")

(dolist (f
  '($cl_eval
    $common_lisp    
    $to_cl))
  (setf (get f 'autoload) "tocl"))
