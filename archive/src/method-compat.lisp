;;;-*- Mode: Lisp; package:maxima; syntax:common-lisp -*-
 
#-symbolics
(error "should not load for other than symbolics")

;#+genera
(shadow '(defmethod defflavor) 'cl-maxima)

;#+genera
(eval-when (compile load eval)

(defmacro defflavor ( name instance-variables component-flavors &rest options)
  "This is just to allow compatibility with new names in symbolics"
  `(global:defflavor  ,name ,instance-variables ,component-flavors
     ,@ (sublis '(#-ti  ( :gettable-instance-variables . :readable-instance-variables )
		  #-ti ( :settable-instance-variables . :writable-instance-variables )
		  #-ti ( :outside-accessible-instance-variables . :writable-instance-variables )
		  #-ti (:inittable-instance-variables . :initable-instance-variables)
		  #-ti ( :ACCESSOR-PREFIX  . :conc-name ))
		  options)))

(defmacro defmethod (spec args &body body &aux leng)
  (cond ((listp spec)(setq spec (copy-list spec))
          (setq leng (length spec))
           (setf spec (append (last spec) (firstn (- leng 1) spec) ))
	   )
	(t (error "how to handle specs")))
     `(global:defmethod  ,spec ,args ,@ body))

)