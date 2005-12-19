; orthopoly autoload declarations, suitable for inclusion in maxima-init.lisp

(defprop $unit_step simp-unit-step operators)
(autof 'simp-unit-step "orthopoly")

($setup_autoload "orthopoly"
		 '$assoc_legendre_p
		 '$assoc_legendre_q
		 '$chebyshev_t
		 '$chebyshev_u
		 '$gen_laguerre
		 '$hermite
		 '$intervalp
		 '$jacobi_p
		 '$laguerre
		 '$legendre_p
		 '$legendre_q
		 '$orthopoly_recur
		 '$orthopoly_weight
		 '$pochhammer
		 '$spherical_bessel_j
		 '$spherical_bessel_y
		 '$spherical_hankel1
		 '$spherical_hankel2
		 '$spherical_harmonic
		 '$ultraspherical)
