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
	$(RUNLISP) -i "$(top_srcdir)/lisp-utils/defsystem" \
        -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :compile :verbose t)' && \
        $(RUNLISP) -i "$(top_srcdir)/lisp-utils/defsystem" \
        -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :load :verbose t)' \
         -f user::run -d binary-clisp/maxima


install-clisp:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-clisp"
	$(INSTALL_DATA) binary-clisp/maxima.mem "$(DESTDIR)$(verpkglibdir)/binary-clisp/maxima.mem"

uninstall-clisp:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-clisp/maxima.mem"

clean-clisp:
	rm -rf binary-clisp

clisp-depends.mk: maxima.system
	test -d binary-clisp || mkdir binary-clisp
	LISPTYPE=clisp ; export LISPTYPE ;\
	$(RUNLISP) -i "$(top_srcdir)/lisp-utils/defsystem" -x '(load "$(top_srcdir)/lisp-utils/make-depends")(funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-clisp/maxima.mem" "clisp-depends.mk")'
include clisp-depends.mk
endif

if CMUCL
all-local: binary-cmucl/maxima.core
install-exec-local: install-cmucl
uninstall: uninstall-cmucl
clean: clean-cmucl
distclean: clean-cmucl

binary-cmucl/maxima.core:
	test -d binary-cmucl || mkdir binary-cmucl
	test -d binary-cmucl/numerical || mkdir binary-cmucl/numerical
	test -d binary-cmucl/numerical/slatec || mkdir binary-cmucl/numerical/slatec
	LISPTYPE=cmucl ; export LISPTYPE ;\
	CMUCL=$(CMUCL_NAME) ; export CMUCL ;\
	$(RUNLISP) -i "$(top_srcdir)/lisp-utils/defsystem" \
        -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :compile :verbose t)' && \
        $(RUNLISP) -i "$(top_srcdir)/lisp-utils/defsystem" \
        -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :load :verbose t)' \
        -d binary-cmucl/maxima

install-cmucl:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-cmucl"
	$(INSTALL_DATA) binary-cmucl/maxima.core "$(DESTDIR)$(verpkglibdir)/binary-cmucl/maxima.core"

uninstall-cmucl:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-cmucl/maxima.core"

clean-cmucl:
	rm -rf binary-cmucl
cmucl-depends.mk: maxima.system
	test -d binary-cmucl || mkdir binary-cmucl
	LISPTYPE=cmucl ; export LISPTYPE ;\
	$(RUNLISP) -i "$(top_srcdir)/lisp-utils/defsystem" -x '(load "$(top_srcdir)/lisp-utils/make-depends")(funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-cmucl/maxima.core" "cmucl-depends.mk")'
include cmucl-depends.mk
endif


if ACL6
all-local: binary-acl6/maxima.dxl
install-exec-local: install-acl6
uninstall: uninstall-acl6
clean: clean-acl6
distclean: clean-acl6

binary-acl6/maxima.dxl:
	test -d binary-acl6 || mkdir binary-acl6
	test -d binary-acl6/numerical || mkdir binary-acl6/numerical
	test -d binary-acl6/numerical/slatec || mkdir binary-acl6/numerical/slatec
	LISPTYPE=acl6 ; export LISPTYPE ;\
	ACL6=$(ACL6_NAME) ; export ACL6 ;\
	$(RUNLISP) -i "$(top_srcdir)/lisp-utils/defsystem" \
        -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :compile :verbose t)' && \
        $(RUNLISP) -i "$(top_srcdir)/lisp-utils/defsystem" \
        -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :load :verbose t)' \
        -d binary-acl6/maxima

install-acl6:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-acl6"
	$(INSTALL_DATA) binary-acl6/maxima.dxl "$(DESTDIR)$(verpkglibdir)/binary-acl6/maxima.dxl"

uninstall-acl6:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-acl6/maxima.dxl"

clean-acl6:
	rm -rf binary-acl6
acl6-depends.mk: maxima.system
	test -d binary-acl6 || mkdir binary-acl6
	LISPTYPE=acl6 ; export LISPTYPE ;\
	ACL6=$(ACL6_NAME); export ACL6 ;\
	$(RUNLISP) -i "$(top_srcdir)/lisp-utils/defsystem" -x '(load "$(top_srcdir)/lisp-utils/make-depends")(funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-acl6/maxima.dxl" "acl6-depends.mk")'
include acl6-depends.mk
endif


if SBCL
all-local: binary-sbcl/maxima.core
install-exec-local: install-sbcl
uninstall: uninstall-sbcl
clean: clean-sbcl
distclean: clean-sbcl

binary-sbcl/maxima.core:
	test -d binary-sbcl || mkdir binary-sbcl
	test -d binary-sbcl/numerical || mkdir binary-sbcl/numerical
	test -d binary-sbcl/numerical/slatec || mkdir binary-sbcl/numerical/slatec
# Work around some problem that sbcl has with defsystem
	test -h numerical/binary-sbcl || ln -s ../binary-sbcl numerical
	test -h  numerical/slatec/binary-sbcl || ln -s ../../binary-sbcl numerical/slatec
	LISPTYPE=sbcl ; export LISPTYPE ;\
	SBCL=$(SBCL_NAME) ; export SBCL ;\
	$(RUNLISP) -i $(top_srcdir)/lisp-utils/defsystem \
        -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :compile :verbose t)' && \
        $(RUNLISP) -i $(top_srcdir)/lisp-utils/defsystem \
        -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :load :verbose t)' \
        -d binary-sbcl/maxima

install-sbcl:
	$(mkinstalldirs) $(DESTDIR)$(verpkglibdir)/binary-sbcl
	$(INSTALL_DATA) binary-sbcl/maxima.core $(DESTDIR)$(verpkglibdir)/binary-sbcl/maxima.core

uninstall-sbcl:
	rm -f $(DESTDIR)$(verpkglibdir)/binary-sbcl/maxima.core

clean-sbcl:
	rm -rf binary-sbcl
	rm -f numerical/binary-sbcl numerical/slatec/binary-sbcl
sbcl-depends.mk: maxima.system
	test -d binary-sbcl || mkdir binary-sbcl
	LISPTYPE=sbcl ; export LISPTYPE ;\
	$(RUNLISP) -i $(top_srcdir)/lisp-utils/defsystem -x '(load "$(top_srcdir)/lisp-utils/make-depends")(funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-sbcl/maxima.core" "sbcl-depends.mk")'
include sbcl-depends.mk
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
	$(RUNLISP) \
        -x '(load "$(top_srcdir)/lisp-utils/defsystem.lisp")(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :compile :verbose t)' && \
	j=$$($(RUNLISP) \
                 -x '(load "$(top_srcdir)/lisp-utils/defsystem.lisp")(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :load :test t)' < /dev/null |\
                 grep Loading |\
                 grep file | \
                 awk 'BEGIN {a="$(shell pwd)"}\
                          {gsub("\"","",$$NF);b=b " \"" a "/" $$NF "\""}\
                      END {printf("(%s (list %s) %s %s \"\" t)",\
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
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-gcl"
	$(INSTALL_PROGRAM) binary-gcl/maxima "$(DESTDIR)$(verpkglibdir)/binary-gcl/maxima"

uninstall-gcl:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-gcl/maxima"
clean-gcl:
	rm -rf binary-gcl

gcl-depends.mk: maxima.system
	test -d binary-gcl || mkdir binary-gcl
	LISPTYPE=gcl ; export LISPTYPE ;\
	$(RUNLISP) -x '(load "$(top_srcdir)/lisp-utils/defsystem.lisp")(load "$(top_srcdir)/lisp-utils/make-depends.lisp")(funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-gcl/maxima" "gcl-depends.mk")'
include gcl-depends.mk
endif

if OPENMCL
all-local: binary-openmcl/maxima.image
install-exec-local: install-openmcl
uninstall: uninstall-openmcl
clean: clean-openmcl
distclean: clean-openmcl

binary-openmcl/maxima.image:
	LISPTYPE=openmcl ; export LISPTYPE ;\
	OPENMCL=$(OPENMCL_NAME) ; export OPENMCL ;\
	$(RUNLISP) -i "$(top_srcdir)/lisp-utils/defsystem" \
        -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :compile :verbose t)' && \
        $(RUNLISP) -i "$(top_srcdir)/lisp-utils/defsystem" \
        -x '(funcall (intern "OPERATE-ON-SYSTEM" :mk) "maxima" :load :verbose t)' \
        -f cl-user::run -d binary-openmcl/maxima

install-openmcl:
	$(mkinstalldirs) "$(DESTDIR)$(verpkglibdir)/binary-openmcl"
	$(INSTALL_DATA) binary-openmcl/maxima.image "$(DESTDIR)$(verpkglibdir)/binary-openmcl/maxima.image"

uninstall-openmcl:
	rm -f "$(DESTDIR)$(verpkglibdir)/binary-openmcl/maxima.image"

clean-openmcl:
	rm -rf binary-openmcl
openmcl-depends.mk: maxima.system
	test -d binary-openmcl || mkdir binary-openmcl
	LISPTYPE=openmcl ; export LISPTYPE ;\
	OPENMCL=$(OPENMCL_NAME) ; export OPENMCL;\
	$(RUNLISP) -i "$(top_srcdir)/lisp-utils/defsystem" -x '(load "$(top_srcdir)/lisp-utils/make-depends")(funcall (intern "CREATE-DEPENDENCY-FILE" :mk) "binary-openmcl/maxima.image" "openmcl-depends.mk")'
include openmcl-depends.mk
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
flatten.lisp \
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
powers.lisp \
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
set.lisp \
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

EXTRA_DIST = $(real_lisp_sources) maxima.system clisp-depends.mk  cmucl-depends.mk  gcl-depends.mk acl6-depends.mk openmcl-depends.mk sbcl-depends.mk numerical/slatec/fortran
