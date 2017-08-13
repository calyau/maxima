;; ezunits: yet another units package for Maxima
;; This program copyright 2008 by Robert Dodier.
;; I release this program under the terms of the
;; GNU General Public License.

(defprop $\`\` tex-infix tex)

;; Process tex(a`b): throw away the backtick, texify a,
;; and texify b with all symbols in b output as mathrm.

(defun tex-ezunits (x l r)
  (setq l (tex (second x) l '("\\;") lop (caar x)))
  (tex (mathrm-ify (third x)) l r (caar x) rop))

;; If a symbol has a texword property, preserve it.
;; Otherwise replace each symbol with a gensym which has a mathrm texword property.

(defun mathrm-ify (e)
  (let
    ((v (reverse (cdr ($listofvars e)))))
    (let
      ((L (mapcar #'(lambda (s)
                      (if (get s 'texword)
                        s
                        (let
                          ((g (gensym)))
                          (putprop
                            g
                            (concatenate
                              'string
                              "\\mathrm{"
                              (maybe-invert-string-case (symbol-name (stripdollar s)))
                              "}")
                            'texword)
                          g)))
                  v)))
      ($substitute `((mlist) ,@(mapcar #'(lambda (a b) `((mequal) ,a ,b)) v L)) e))))

(defprop $\` tex-ezunits tex)

(defun $odds (a) (cons '(mlist) (odds (cdr a) 1)))

(defun $evens (a) (cons '(mlist) (odds (cdr a) 0)))

;; MathML presentation code integration.
;;
;; The MATHML-PRESENTATION-EZUNITS function is used to display an ezunits
;; expression. It should only be called after the MathML presentation library
;; has been loaded (and, as such, MPR_ENGINE will be fbound)

(defun mathml-presentation-ezunits (mexpress)
  (funcall (symbol-function 'mPr_engine)
           `((mtimes) ,(second mexpress) ,(third mexpress)) 'mparen 'mparen))

;; The ezunits-setup-mathml thunk gets installed into *MATHML-PRESENTATION-HOOK*
;; and is an idempotent setup function that registers the mathml output for
;; ezunits expressions.

(defun ezunits-setup-mathml ()
  (funcall (symbol-function 'setup)
           '($\` (mPrprocess mathml-presentation-ezunits))))

;; This is a re-implementation of Emacs's WITH-EVAL-AFTER-LOAD. If and when we
;; put a more general version of that into Maxima, use it here!
;;
;; Register a hook so we set up properly if the MathML code hasn't yet been
;; loaded.
(setf (symbol-value '*mathml-presentation-hook*)
      (cons 'ezunits-setup-mathml
            (remove 'ezunits-setup-mathml
                    (when (boundp '*mathml-presentation-hook*)
                      (symbol-value '*mathml-presentation-hook*)))))
;; But make sure we do the right thing if it's been loaded already
(when (fboundp 'mPr_engine)
  (ezunits-setup-mathml))
