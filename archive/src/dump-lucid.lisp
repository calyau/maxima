(load "sysdef.lisp")
(change-memory-management :growth-limit 500)
(make :maxima :compile t)
(disksave "maxima_lucid" :restart-function 'maxima::macsyma-top-level)
