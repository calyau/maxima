;; Load all Lisp files within WITH-COMPILATION-UNIT macro.
;; This quiets the undefined function warnings from SBCL
;; which are otherwise very voluminous (and this construct
;; is accepted by other CL implementations).

#+gcl (unless (macro-function 'with-compilation-unit)
	       (defmacro with-compilation-unit (a &rest b) `(progn ,@b)))

(with-compilation-unit nil
  ($load "macros")
  ($load "operations")
  ($load "chbase")
  ($load "util")
  ($load "lecteur")
  ($load "ecrivain")
  ($operation)
  ($load "arite")
  ($load "elem")
  ($load "pui")
  ($load "schur")
  ($load "direct")
  ($load "kak")
  ($load "partpol")
  ($load "multmon")
  ($load "permut")
  ($load "treillis")
  ($load "resolv1")
  ($load "resolvante")
  ($load "resolcayley"))

