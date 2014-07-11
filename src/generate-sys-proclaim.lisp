(load "../lisp-utils/defsystem.lisp")
(compiler::emit-fn t)
(mk::oos "maxima" :compile :verbose t)
(compiler::make-all-proclaims "*/*.fn" "*/*/*/*.fn")
