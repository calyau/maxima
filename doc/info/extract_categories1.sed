s/@"/@\\"/g
s/^@deffn  *{[^}]*}  *\([^[:blank:]]*\).*/(setq items (list (list "deffn" "\1")))/
s/^@deffn  *[^[:blank:]][^[:blank:]]*  *\([^[:blank:]]*\).*/(setq items (list (list "deffn" "\1")))/
s/^@defvr  *{[^}]*}  *\([^[:blank:]]*\).*/(setq items (list (list "defvr" "\1")))/
s/^@defvr  *[^[:blank:]][^[:blank:]]*  *\([^[:blank:]]*\).*/(setq items (list (list "defvr" "\1")))/
s/^@deffnx  *{[^}]*}  *\([^[:blank:]]*\).*/(if (not (member (list "deffn" "\1") items :test #'equal)) (nconc items (list (list "deffn" "\1"))))/
s/^@deffnx  *[^[:blank:]][^[:blank:]]*  *\([^[:blank:]]*\).*/(if (not (member (list "deffn" "\1") items :test #'equal)) (nconc items (list (list "deffn" "\1"))))/
s/^@defvrx  *{[^}]*}  *\([^[:blank:]]*\).*/(if (not (member (list "defvr" "\1") items :test #'equal)) (nconc items (list (list "defvr" "\1"))))/
s/^@defvrx  *[^[:blank:]][^[:blank:]]*  *\([^[:blank:]]*\).*/(if (not (member (list "defvr" "\1") items :test #'equal)) (nconc items (list (list "defvr" "\1"))))/
s/^@end deffn//
s/^@end defvr//
s/^@node  *\([^,]*\).*/(setq items (list (list "node" "\1")))/
s/@opencatbox//
s/@closecatbox//
s/@category{\([^}]*\)}\s*/(setf (gethash "\1" *categories*) (append (gethash "\1" *categories*) (foo items)))/g
