include $(top_srcdir)/common.mk
RUNLISP = $(top_srcdir)/lisp-utils/maxima-run-lisp
bin_SCRIPTS = maxima

if CLISP
all-local: binary-clisp/maxima.mem
install-exec-local: install-clisp
uninstall: uninstall-clisp
clean: clean-clisp
distclean: clean-clisp

binary-clisp/maxima.mem:
	LISPTYPE=clisp ; export LISPTYPE ;\
	CLISP=$(CLISP_NAME) ; export CLISP ;\
	$(RUNLISP) -i $(top_srcdir)/lisp-utils/defsystem \
        -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :compile :verbose t)' && \
        $(RUNLISP) -i $(top_srcdir)/lisp-utils/defsystem \
        -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :load :verbose t)' \
         -f user::run -d binary-clisp/maxima


install-clisp:
	$(mkinstalldirs) $(DESTDIR)$(verpkglibdir)/binary-clisp
	$(INSTALL_DATA) binary-clisp/maxima.mem $(DESTDIR)$(verpkglibdir)/binary-clisp/maxima.mem

uninstall-clisp:
	rm -f $(DESTDIR)$(verpkglibdir)/binary-clisp/maxima.mem

clean-clisp:
	rm -rf binary-clisp

clisp-depends.mk: maxima.system
	LISPTYPE=clisp ; export LISPTYPE ;\
	$(RUNLISP) -i $(top_srcdir)/lisp-utils/defsystem -x '(load "$(top_srcdir)/lisp-utils/make-depends")(funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-clisp/maxima.mem" "clisp-depends.mk")'
include clisp-depends.mk
endif

if CMUCL
all-local: binary-cmucl/maxima.core
install-exec-local: install-cmucl
uninstall: uninstall-cmucl
clean: clean-cmucl
distclean: clean-cmucl

binary-cmucl/maxima.core:
	LISPTYPE=cmucl ; export LISPTYPE ;\
	CMUCL=$(CMUCL_NAME) ; export CMUCL ;\
	$(RUNLISP) -i $(top_srcdir)/lisp-utils/defsystem \
        -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :compile :verbose t)' && \
        $(RUNLISP) -i $(top_srcdir)/lisp-utils/defsystem \
        -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :load :verbose t)' \
        -d binary-cmucl/maxima

install-cmucl:
	$(mkinstalldirs) $(DESTDIR)$(verpkglibdir)/binary-cmucl
	$(INSTALL_DATA) binary-cmucl/maxima.core $(DESTDIR)$(verpkglibdir)/binary-cmucl/maxima.core

uninstall-cmucl:
	rm -f $(DESTDIR)$(verpkglibdir)/binary-cmucl/maxima.core

clean-cmucl:
	rm -rf binary-cmucl
cmucl-depends.mk: maxima.system
	LISPTYPE=cmucl ; export LISPTYPE ;\
	$(RUNLISP) -i $(top_srcdir)/lisp-utils/defsystem -x '(load "$(top_srcdir)/lisp-utils/make-depends")(funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-cmucl/maxima.core" "cmucl-depends.mk")'
include cmucl-depends.mk
endif

if GCL
all-local: binary-gcl/maxima
install-exec-local: install-gcl
uninstall: uninstall-gcl
clean: clean-gcl
distclean: clean-gcl

if GCL_ALT_LINK
binary-gcl/maxima:
	test -d binary-gcl || mkdir binary-gcl
	test -d binary-gcl/numerical || mkdir binary-gcl/numerical
	test -d binary-gcl/numerical/slatec || mkdir binary-gcl/numerical/slatec
	LISPTYPE=gcl ; export LISPTYPE ;\
        GCL=$(GCL_NAME) ; export GCL ;\
	j=$$($(RUNLISP) \
                 -x '(load "$(top_srcdir)/lisp-utils/defsystem.lisp")(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :load :test t)' < /dev/null |\
                 grep Loading |\
                 grep file | \
                 awk 'BEGIN {a="$(shell pwd)"}\
                          {gsub("\"","",$$NF);b=b " \"" a "/" $$NF "\""}\
                      END {printf("(%s (list %s) %s %s )",\
                            "compiler::link",b,"\"" a "/$@\"",\
                            "\"(provide \\\"maxima\\\")\"")}') && \
        $(RUNLISP) -x "$$j" < /dev/null
else
binary-gcl/maxima:
	test -d binary-gcl || mkdir binary-gcl
	test -d binary-gcl/numerical || mkdir binary-gcl/numerical
	test -d binary-gcl/numerical/slatec || mkdir binary-gcl/numerical/slatec
	LISPTYPE=gcl ; export LISPTYPE ;\
	GCL=$(GCL_NAME) ; export GCL ;\
	$(RUNLISP) \
        -x '(load "$(top_srcdir)/lisp-utils/defsystem.lisp")(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :compile :verbose t)' && \
        $(RUNLISP) \
        -x '(load "$(top_srcdir)/lisp-utils/defsystem.lisp")(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :load :verbose t)' \
         -d binary-gcl/maxima
endif

install-gcl:
	$(mkinstalldirs) $(DESTDIR)$(verpkglibdir)/binary-gcl
	$(INSTALL_PROGRAM) binary-gcl/maxima $(DESTDIR)$(verpkglibdir)/binary-gcl/maxima

uninstall-gcl:
	rm -f $(DESTDIR)$(verpkglibdir)/binary-gcl/maxima
clean-gcl:
	rm -rf binary-gcl

gcl-depends.mk: maxima.system
	LISPTYPE=gcl ; export LISPTYPE ;\
	$(RUNLISP) -x '(load "$(top_srcdir)/lisp-utils/defsystem.lisp")(load "$(top_srcdir)/lisp-utils/make-depends.lisp")(funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-gcl/maxima" "gcl-depends.mk")'
include gcl-depends.mk
endif

echo_%:
	@echo "$(subst echo_,,$@)=$($(subst echo_,,$@))"
	@echo "origin $(subst echo_,,$@) returns $(origin $(subst echo_,,$@))"

genericdir = $(instsrcdir)
real_lisp_sources = SYS-DECLAIM.lisp \
SYS-PROCLAIM.lisp \
acall.lisp \
algfac.lisp \
algsys.lisp \
ar.lisp \
askp.lisp \
asum.lisp \
autol.lisp \
bessel.lisp \
buildq.lisp \
cl-info.lisp \
clmacs.lisp \
combin.lisp \
comm.lisp \
comm2.lisp \
commac.lisp \
compar.lisp \
compat.lisp \
cpoly.lisp \
csimp.lisp \
csimp2.lisp \
db.lisp \
defcal.lisp \
defint.lisp \
defopt.lisp \
desoln.lisp \
displa.lisp \
displm.lisp \
dskfn.lisp \
elim.lisp \
ellipt.lisp \
evalw.lisp \
ezgcd.lisp \
factor.lisp \
fcall.lisp \
float.lisp \
fortra.lisp \
generr.lisp \
grind.lisp \
hayat.lisp \
homog.lisp \
hyp.lisp \
hypgeo.lisp \
init-cl.lisp \
inmis.lisp \
intpol.lisp \
invert.lisp \
irinte.lisp \
kclmac.lisp \
laplac.lisp \
ldisp.lisp \
lesfac.lisp \
letmac.lisp \
limit.lisp \
linnew.lisp \
lmdcls.lisp \
logarc.lisp \
macdes.lisp \
macsys.lisp \
mactex.lisp \
marray.lisp \
mat.lisp \
matcom.lisp \
matrix.lisp \
matrun.lisp \
max_ext.lisp \
maxima-package.lisp \
maxmac.lisp \
mdebug.lisp \
mdefun.lisp \
mdot.lisp \
merror.lisp \
mforma.lisp \
mformt.lisp \
mhayat.lisp \
misc.lisp \
mlisp.lisp \
mload.lisp \
mmacro.lisp \
mopers.lisp \
mormac.lisp \
mrgmac.lisp \
mstuff.lisp \
mtrace.lisp \
mutils.lisp \
nalgfa.lisp \
newdet.lisp \
newinv.lisp \
nforma.lisp \
nisimp.lisp \
nparse.lisp \
nrat4.lisp \
nregex.lisp \
numer.lisp \
numerm.lisp \
numth.lisp \
nusum.lisp \
ode2.lisp \
opers.lisp \
optim.lisp \
optimize.lisp \
option.lisp \
outmis.lisp \
pade.lisp \
parse-body.lisp \
plot.lisp \
pois2.lisp \
pois3.lisp \
polyrz.lisp \
procs.lisp \
psolve.lisp \
rat3a.lisp \
rat3b.lisp \
rat3c.lisp \
rat3d.lisp \
rat3e.lisp \
ratmac.lisp \
ratout.lisp \
ratpoi.lisp \
residu.lisp \
result.lisp \
risch.lisp \
rombrg.lisp \
rpart.lisp \
runtim.lisp \
rzmac.lisp \
schatc.lisp \
scs.lisp \
series.lisp \
simp.lisp \
sin.lisp \
sinint.lisp \
sloop.lisp \
solve.lisp \
specfn.lisp \
spgcd.lisp \
sprdet.lisp \
strmac.lisp \
sublis.lisp \
sumcon.lisp \
suprv1.lisp \
tlimit.lisp \
todd-coxeter.lisp \
trans1.lisp \
trans2.lisp \
trans3.lisp \
trans4.lisp \
trans5.lisp \
transf.lisp \
transl.lisp \
transm.lisp \
transq.lisp \
transs.lisp \
trdata.lisp \
trgred.lisp \
trgsmp.lisp \
trigi.lisp \
trigo.lisp \
trmode.lisp \
troper.lisp \
trpred.lisp \
trprop.lisp \
trutil.lisp \
ufact.lisp \
utils.lisp \
zero.lisp \
autoconf-variables.lisp \
numerical/slatec/d9aimp.lisp \
numerical/slatec/d9b0mp.lisp \
numerical/slatec/d9b1mp.lisp \
numerical/slatec/d9lgmc.lisp \
numerical/slatec/dai.lisp \
numerical/slatec/daie.lisp \
numerical/slatec/dasyik.lisp \
numerical/slatec/dasyjy.lisp \
numerical/slatec/dbesi.lisp \
numerical/slatec/dbesi0.lisp \
numerical/slatec/dbesi1.lisp \
numerical/slatec/dbesj.lisp \
numerical/slatec/dbesj0.lisp \
numerical/slatec/dbesj1.lisp \
numerical/slatec/dbesy0.lisp \
numerical/slatec/dbesy1.lisp \
numerical/slatec/dbsi0e.lisp \
numerical/slatec/dbsi1e.lisp \
numerical/slatec/dcsevl.lisp \
numerical/slatec/de1.lisp \
numerical/slatec/dei.lisp \
numerical/slatec/derf.lisp \
numerical/slatec/derfc.lisp \
numerical/slatec/dgamlm.lisp \
numerical/slatec/dgamln.lisp \
numerical/slatec/dgamma.lisp \
numerical/slatec/djairy.lisp \
numerical/slatec/dlngam.lisp \
numerical/slatec/fdump.lisp \
numerical/slatec/initds.lisp \
numerical/slatec/j4save.lisp \
numerical/slatec/xercnt.lisp \
numerical/slatec/xerhlt.lisp \
numerical/slatec/xermsg.lisp \
numerical/slatec/xerprn.lisp \
numerical/slatec/xersve.lisp \
numerical/slatec/xgetua.lisp \
numerical/slatec/zabs.lisp \
numerical/slatec/zacai.lisp \
numerical/slatec/zairy.lisp \
numerical/slatec/zasyi.lisp \
numerical/slatec/zbesj.lisp \
numerical/slatec/zbinu.lisp \
numerical/slatec/zbknu.lisp \
numerical/slatec/zbuni.lisp \
numerical/slatec/zdiv.lisp \
numerical/slatec/zexp.lisp \
numerical/slatec/zkscl.lisp \
numerical/slatec/zlog.lisp \
numerical/slatec/zmlri.lisp \
numerical/slatec/zmlt.lisp \
numerical/slatec/zrati.lisp \
numerical/slatec/zs1s2.lisp \
numerical/slatec/zseri.lisp \
numerical/slatec/zshch.lisp \
numerical/slatec/zsqrt.lisp \
numerical/slatec/zuchk.lisp \
numerical/slatec/zunhj.lisp \
numerical/slatec/zuni1.lisp \
numerical/slatec/zuni2.lisp \
numerical/slatec/zunik.lisp \
numerical/slatec/zuoik.lisp \
numerical/slatec/zwrsk.lisp \
numerical/slatec/dbesk.lisp \
numerical/slatec/dbesk0.lisp \
numerical/slatec/dbesk1.lisp \
numerical/slatec/dbesy.lisp \
numerical/slatec/dbsk0e.lisp \
numerical/slatec/dbsk1e.lisp \
numerical/slatec/dbsknu.lisp \
numerical/slatec/dbsynu.lisp \
numerical/slatec/dyairy.lisp \
numerical/slatec/zacon.lisp \
numerical/slatec/zbesh.lisp \
numerical/slatec/zbesi.lisp \
numerical/slatec/zbesk.lisp \
numerical/slatec/zbesy.lisp \
numerical/slatec/zbunk.lisp \
numerical/slatec/zunk1.lisp \
numerical/slatec/zunk2.lisp \
numerical/f2cl-package.lisp \
numerical/slatec.lisp \
numerical/f2cl-lib.lisp

genericdirDATA = $(real_lisp_sources) autoconf-variables.lisp

EXTRA_DIST = $(real_lisp_sources) maxima.system clisp-depends.mk  cmucl-depends.mk  gcl-depends.mk numerical/slatec/fortran
