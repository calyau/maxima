(cond ((probe-file  "/public/akcl/cmpnew/collectfn.o")
       (load "/public/akcl/cmpnew/collectfn")
       (compiler::emit-fn t)))

(setq compiler::*fasd-data* :system-p)




