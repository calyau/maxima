RUNLISP = ../lisp-utils/maxima-run-lisp
bin_SCRIPTS = maxima
verpkglibdir = $(pkglibdir)/@VERSION@

if CLISP
all-local: binary-clisp/maxima.mem
install-exec-local: install-clisp
uninstall: uninstall-clisp
clean: clean-clisp
distclean: distclean-clisp

binary-clisp/maxima.mem:
	LISPTYPE=clisp ; export LISPTYPE ;\
	$(RUNLISP) -i ../lisp-utils/defsystem -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :compile :verbose t)' -f user::run -d binary-clisp/maxima

install-clisp:
	$(mkinstalldirs) $(DESTDIR)$(verpkglibdir)/binary-clisp
	$(INSTALL_DATA) binary-clisp/maxima.mem $(DESTDIR)$(verpkglibdir)/binary-clisp/maxima.mem

uninstall-clisp:
	rm -f $(DESTDIR)$(verpkglibdir)/binary-clisp/maxima.mem

clean-clisp:
	-test -d binary-clisp && \
                ( cd binary-clisp && \
                rm -f `ls -1 | grep -v maxima.mem` )
distclean-clisp:
	rm -rf binary-clisp
clisp-depends.mk: maxima.system
	LISPTYPE=clisp ; export LISPTYPE ;\
	$(RUNLISP) -i ../lisp-utils/defsystem -x '(load "../lisp-utils/make-depends")(funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-clisp/maxima.mem" "clisp-depends.mk")'
include clisp-depends.mk
endif

if CMUCL
all-local: binary-cmucl/maxima.core
install-exec-local: install-cmucl
uninstall: uninstall-cmucl
clean: clean-cmucl
distclean: distclean-cmucl

binary-cmucl/maxima.core:
	LISPTYPE=cmucl ; export LISPTYPE ;\
	$(RUNLISP) -i ../lisp-utils/defsystem -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :compile :verbose t)' -f user::run -d binary-cmucl/maxima

install-cmucl:
	$(mkinstalldirs) $(DESTDIR)$(verpkglibdir)/binary-cmucl
	$(INSTALL_DATA) binary-cmucl/maxima.core $(DESTDIR)$(verpkglibdir)/binary-cmucl/maxima.core

uninstall-cmucl:
	rm -f $(DESTDIR)$(verpkglibdir)/binary-cmucl/maxima.core

clean-cmucl:
	-test -d binary-cmucl && \
                ( cd binary-cmucl && \
                rm -f `ls -1 | grep -v maxima.core` )
distclean-cmucl:
	rm -rf binary-cmucl
cmucl-depends.mk: maxima.system
	LISPTYPE=cmucl ; export LISPTYPE ;\
	$(RUNLISP) -i ../lisp-utils/defsystem -x '(load "../lisp-utils/make-depends")(funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-cmucl/maxima.core" "cmucl-depends.mk")'
include cmucl-depends.mk
endif

if GCL
all-local: binary-gcl/maxima
install-exec-local: install-gcl
uninstall: uninstall-gcl
clean: clean-gcl
distclean: distclean-gcl

binary-gcl/maxima:
	test -d binary-gcl || mkdir binary-gcl
	LISPTYPE=gcl ; export LISPTYPE ;\
	$(RUNLISP) -x '(load "../lisp-utils/defsystem.lisp")(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :compile :verbose t)' -f user::run -d binary-gcl/maxima

install-gcl:
	$(mkinstalldirs) $(DESTDIR)$(verpkglibdir)/binary-gcl
	$(INSTALL_PROGRAM) binary-gcl/maxima $(DESTDIR)$(verpkglibdir)/binary-gcl/maxima

uninstall-gcl:
	rm -f $(DESTDIR)$(verpkglibdir)/binary-gcl/maxima

clean-gcl:
	-test -d binary-gcl && \
                ( cd binary-gcl && \
                rm -f `ls -1 | grep -v maxima` )
distclean-gcl:
	rm -rf binary-gcl
gcl-depends.mk: maxima.system
	LISPTYPE=gcl ; export LISPTYPE ;\
	$(RUNLISP) -x '(load "../lisp-utils/defsystem.lisp")(load "../lisp-utils/make-depends.lisp")(funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-gcl/maxima" "gcl-depends.mk")'
include gcl-depends.mk
endif

echo_%:
	@echo "$(subst echo_,,$@)=$($(subst echo_,,$@))"
	@echo "origin $(subst echo_,,$@) returns $(origin $(subst echo_,,$@))"
