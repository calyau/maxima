
.SUFFIXES: .o .c .lisp

SHELL	= sh
SRC	= .
PORTDIR	=$(GCLDIR)/unixport
MAXIMA	= ./saved_maxima
LISP	= $(PORTDIR)/saved_gcl${EXE} $(PORTDIR)/
#       sloop.o is already in our kcl. otherwise it goes here.
HERE=${MAXDIR}/src

# begin makedefs

# use=386-linux

# for main link of raw_gcl
LIBS= -lm

#The multi precision library stuff
MPFILES=$(MPDIR)/mpi-386_no_under.o $(MPDIR)/libmport.a


# root for the installation, eg /usr/local
# This would cause make install to create /usr/local/bin/gcl and
# /usr/local/lib/gcl-2-??/* with some basic files.
prefix=/usr/local

# where to place the info files
INFO_DIR=/usr/local/info/

# where to put emacs lisp files.
EMACS_SITE_LISP=/usr/share/emacs/20.7/site-lisp

# the default.el file
EMACS_DEFAULT_EL=/usr/share/emacs/20.7/site-lisp/default.el

# numerous TCL/TK variables culled from the tkConfig.sh and tclConfig.sh
# if these are found.
TK_CONFIG_PREFIX=/usr/local/lib
TK_LIBRARY=/usr/local/lib/tk8.3
TCL_LIBRARY=/usr/local/lib/tcl8.3
TK_XINCLUDES=-I/usr/X11R6/include
TK_INCLUDE=-I/usr/local/lib/../include
TCL_INCLUDE=-I/usr/local/lib/../include
TK_LIB_SPEC=-L/usr/local/lib -ltk8.3
TK_BUILD_LIB_SPEC=-L/home/wfs/src/tk8.3.2/unix -ltk8.3
TK_XLIBSW=-L/usr/X11R6/lib -lX11
TK_XINCLUDES=-I/usr/X11R6/include
TCL_LIB_SPEC=-L/usr/local/lib -ltcl8.3${TCL_DBGX}
TCL_DL_LIBS=-ldl
TCL_LIBS=-ldl -lm

NOTIFY=yes
GCC=gcc





GCLDIR=/home/wfs/cvs/gcl
SHELL=/bin/sh
MACHINE=386-linux

# notes for redhat 6.0
#  the configure should select the compiler GCC=/usr/bin/i386-glibc20-linux-gcc
#  However for the gcl-tk directory, you must use plain 'gcc' since
#  that must link with the tcl tk libs which have been compiled with it.
#  so after configure change to GCC=gcc in the gcl-tk/makefile


# Machine dependent makefile definitions for intel 386,486 running linux

LBINDIR=/usr/local/bin

OFLAG	=  -O 
LIBS	= -lm 

ODIR_DEBUG= -O4 

# This CC string will be used for compilation of the system,
# and also in the compiler::*cc* variable for later compilation of
# lisp files.
# (the -pipe is just since our file system is slow..)
CC = ${GCC} -pipe -fwritable-strings  -DVOL=volatile -I$(GCLDIR)/o -fsigned-char

# under redhat 6.1 and slackware 7.0 we needed to have this
# link be static, but should be ok with the fix to unixport/rsym_elf.c
LDCC=${CC} -static
LDCC=${CC}

# note for linuxaout on an elf machine add -b i486-linuxaout 
# CC = gcc -pipe -fwritable-strings  -DVOL=volatile -I$(GCLDIR)/o -fsigned-char -b i486-linuxaout 

# Enable the fastloading mechanism which does not use ld -A
# requires c/rel_.. machine dependent code.

RSYM	= rsym
SFASL	= $(ODIR)/sfasl.o


#MPFILES= $(MPDIR)/mpi-386d.o   $(MPDIR)/libmport.a


# When using SFASL it is good to have (si::build-symbol-table)
INITFORM=(si::build-symbol-table)

# Use symbolic links
SYMB=-s

LIBFILES=bsearch.o

# the  make to use for saved_kcp the profiler.
KCP=kcp-bsd
MPFILES=${GMP_DIR}libgmp.a


# end makedefs
GCLDIR=/home/wfs/cvs/gcl
MAXDIR=/home/wfs/cvs/maxima
PREFIX_DIR=/usr/local
EMACS_SITE_LISP=/usr/local/lib/emacs/site-lisp
INFO_DIR=/usr/local/info
MAN_DIR=/usr/local/man/man1
# end maxmakedefs



MACROS = lmdcls.o letmac.o kclmac.o clmacs.o commac.o mormac.o \
	compat.o defopt.o defcal.o maxmac.o mopers.o \
	mforma.o mrgmac.o procs.o rzmac.o strmac.o \
	displm.o ratmac.o mhayat.o \
	numerm.o transm.o optimize.o 

# serror.o is now in gcl
OBJS   = opers.o utils.o sumcon.o sublis.o runtim.o \
	 merror.o mformt.o mutils.o \
	outmis.o ar.o misc.o comm.o comm2.o mlisp.o \
	mmacro.o buildq.o simp.o float.o csimp.o \
	csimp2.o zero.o logarc.o rpart.o macsys.o mload.o \
	suprv1.o dskfn.o lesfac.o \
	factor.o algfac.o nalgfa.o ufact.o result.o \
	rat3a.o rat3b.o rat3d.o rat3c.o rat3e.o \
	nrat4.o ratout.o  transl.o transs.o \
	trans1.o trans2.o trans3.o trans4.o \
	trans5.o transf.o troper.o trutil.o trmode.o trdata.o \
	trpred.o transq.o acall.o \
	fcall.o evalw.o trprop.o mdefun.o bessel.o ellipt.o \
	numer.o intpol.o rombrg.o \
	nparse.o displa.o nforma.o ldisp.o \
	grind.o spgcd.o ezgcd.o option.o macdes.o \
	inmis.o db.o compar.o askp.o sinint.o \
	sin.o risch.o hayat.o defint.o residu.o trigi.o \
	trigo.o trgred.o specfn.o mat.o matrix.o \
	sprdet.o newinv.o linnew.o newdet.o \
	schatc.o matcom.o matrun.o nisimp.o tlimit.o \
	limit.o solve.o psolve.o algsys.o \
	polyrz.o cpoly.o mtrace.o scs.o asum.o fortra.o \
	optim.o marray.o mdot.o irinte.o \
	series.o numth.o laplac.o pade.o homog.o combin.o \
	mstuff.o ratpoi.o pois2.o pois3.o  mdebug.o


TRANSLATED = nusum.o desoln.o elim.o trgsmp.o ode2.o invert.o plot.o

OTHERS 	= hypgeo.o hyp.o todd-coxeter.o mactex.o

all:   $(MACROS) $(OBJS)  saved_maxima $(OTHERS)
	make translated


install:
	cp ${TRANSLATED} ${OTHERS} ${INSTALL_LIB_DIR}/src

sysinit.o maxobjs: sysinit.lsp makefile ../makefile
	echo '(load "sysdef.lisp")(compile-file "sysinit.lsp" :system-p t :c-file t)' | $(LISP)

# make an init_maxima.lsp in the PORTDIR

init_max0.lisp: makefile
	echo '(si::chdir "${HERE}")' > init_max0.lisp
	echo '(load "maxima-package.lisp")' >> init_max0.lisp
	echo '(setq maxima::*maxima-directory* (namestring (truename "../")))' >> init_max0.lisp

comparedirs:
	for v in `echo $OBJS $OTHERS $TRANSLATED | sed -e "s:\.o:.lisp:g"` ; do echo for $v ; diff -c $v  /home/wfs/cvs/maxima/src/$v ; done

init_max2.lisp: makefile
	echo '(si::chdir "'${PORTDIR}'")' > init_max2.lisp

saved_maxima: maxobjs sysinit.o $(OBJS) $(MACROS) version.lisp init_max0.lisp init_max2.lisp
	cat $(PORTDIR)/init_gcl.lsp | sed -e \
	"s;system-init(si::chdir \"${HERE}\")(si::save-system \"saved_maxima\")g" \
	> init_maxima.lsp
	- if cmp init_maxima.lsp $(PORTDIR)/init_maxima.lsp > /dev/null ;\
	then true; else  cp init_maxima.lsp $(PORTDIR)/init_maxima.lsp;fi
	(cd $(PORTDIR) ; make saved_maxima "SYSTEM=maxima" "SYSTEM_OBJS=`cat $(MAXDIR)/src/maxobjs`" "PORTDIR=$(PORTDIR)")



version.lisp: ../majvers  ../minvers ${OBJS} ${MACROS}
	echo "(setf (get :maxima :version) (quote (" `cat ../majvers` `cat ../minvers` \"`date`\" ")))" > version.lisp

PREAMBLE='(load "sysdef.lisp")' \
	'(load "SYS-PROCLAIM.lisp")' \
	'(proclaim (quote (optimize (safety 0) (speed 0) (space 0))))' \
	'(setq compiler::*ifuncall* t)'

PREAMBLE='(load "sysdef.lisp")' \
	'(load "'${GCLDIR}'/lsp/sys-proclaim.lisp")' '(load "sys-proclaim.lisp")' \
	'(proclaim (quote (optimize (safety 0) (speed 3) (space 0))))' \
	'(setq *load-verbose* nil)'


$(MACROS):  
	echo $(PREAMBLE)\
	'(proclaim (quote (optimize (safety 2) (speed 2) (space 2))))' \
	'(make::make :maxima-macros :compile t)' | $(LISP)

$(TRANSLATED) $(OTHERS):
	./saved_maxima -compile  $*.lisp  -o-file t

translated:	$(TRANSLATED) $(OTHERS)

sloop.o make.o:
	 $(SAVEDKCLDIR)/saved_kcl $(SAVEDKCLDIR) $*.lisp $* S1000

# This would cause recompiling of all objects if one macro file changed.
# $(OBJS):: saved_max_compile
#	rm $@

.lisp.o:
	echo $(PREAMBLE)'(make::make :maxima-macros)(compile-file '\"$*.lisp\"' :system-p  t)' | ${LISP}

# c files should not get left but they're sometimes there.

.c.o:
	echo $(PREAMBLE)'(make::make :maxima-macros)(compile-file '\"$*.lisp\"' :system-p t)' | ${LISP}

all-clean:
	rm -f $(TRANSLATED) $(OBJS) $(MACROS) core saved_maxima
	rm -f *.o

clean:
	rm -f  $(OBJS) $(MACROS) core saved_max_compile make.o $(PORTDIR)/raw_maxima

eliminate-source:
	rm -f `echo  $(OBJS) | sed -e "s:[.]o:.lisp"`
	rm -f $(OBJS) sysdef.lisp make.lisp
	rm -f info/*.texi
	rm -f $(PORTDIR)/raw_maxima

tags:
	@echo etags $(OTHERS) $(TRANSLATED) $(OBJS) $(MACROS) | sed -e "s:\.o:.lisp:g" |sh

proclaims:
	echo "(setq user::lisp-dir \"$(KCLDIR)\")" \
	'(load "makepro.lisp")' | $(KCLDIR)/xbin/kcl
	cat var-proclaims.lisp >> SYS-PROCLAIM.lisp

sys-proclaim.lisp:
	echo $(PREAMBLE)\
	'(setq maxima::*maxima-directory* (namestring (truename "../")))'\
	'(proclaim (quote (optimize (safety 2) (speed 2) (space 2))))' \
	'(in-package "MAXIMA")(make::make :maxima :proclaims t)' | $(LISP)
	cat var-proclaims.lisp >> sys-proclaim.lisp

CLISP=clisp
CMULISP=lisp
clisp-compile:
	echo '(progn (load "compile-clisp.lisp")(compile-maxima))' | ${CLISP}
maxima-clisp.mem: clisp-compile
	echo '(progn (load "compile-clisp.lisp")(save-maxima))' | ${CLISP}
# invoke clisp maxima on the test suite
test-clisp: maxima-clisp.mem
	(cd ../doc ; ${CLISP} -M ../src/maxima-clisp.mem tests.lisp)


cmulisp-compile:
	echo '(progn (load "compile-cmulisp.lisp")(compile-maxima))' | ${CMULISP}
maxima.core:  cmulisp-compile
	echo '(progn (load "compile-cmulisp.lisp")(save-maxima))' | ${CMULISP}

test-cmulisp: maxima.core
	(cd ../doc ; ${CMULISP} -core ../src/maxima.core -load ../src/maxima-clisp.mem tests.lisp)








# explicitly state these... necessary for some 'make' s

lmdcls.o:lmdcls.lisp
letmac.o:letmac.lisp
kclmac.o:kclmac.lisp
clmacs.o:clmacs.lisp
commac.o:commac.lisp
mormac.o:mormac.lisp
compat.o:compat.lisp
defopt.o:defopt.lisp
defcal.o:defcal.lisp
maxmac.o:maxmac.lisp
mopers.o:mopers.lisp
mforma.o:mforma.lisp
mrgmac.o:mrgmac.lisp
procs.o:procs.lisp
rzmac.o:rzmac.lisp
strmac.o:strmac.lisp
displm.o:displm.lisp
ratmac.o:ratmac.lisp
mhayat.o:mhayat.lisp
numerm.o:numerm.lisp
transm.o:transm.lisp
optimize.o:optimize.lisp
plot.o:plot.lisp

othernames:
	@ for v in ${OTHERS} ${TRANSLATED} ; do echo src/$$v ; done
