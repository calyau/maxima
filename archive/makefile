# edit ./configure to reflect paths at your site.  This will insert
# things in these directories.  Then do './configure' 

# the lines down to '# end makedefs' are added by ./configure from the gcl dir.

# place to put maxima in installation procedure
PREFIX_DIR=/usr/local
# the above will result in /usr/local/bin/maxima and /usr/local/lib/maxima-x.x

# begin makedefs

# use=solaris

# for main link of raw_gcl
LIBS= -lm -lsocket -lnsl

#The multi precision library stuff
MPFILES=$(MPDIR)/ $(MPDIR)/libmport.a


# root for the installation, eg /usr/local
# This would cause make install to create /usr/local/bin/gcl and
# /usr/local/lib/gcl-2-??/* with some basic files.
prefix=/usr/local

# where to place the info files
INFO_DIR=/local/sol/2.6/depot/emacs-20.3/info/

# where to put emacs lisp files.
EMACS_SITE_LISP=/local/sol/2.6/depot/emacs-20.3/share/emacs/20.3/site-lisp

# the default.el file
EMACS_DEFAULT_EL=/local/sol/2.6/depot/emacs-20.3/share/emacs/20.3/site-lisp/default.el

# numerous TCL/TK variables culled from the tkConfig.sh and tclConfig.sh
# if these are found.
TK_CONFIG_PREFIX=/local/sol/2.6/depot/tk8.0/lib
TK_LIBRARY=/local/sol/2.6/depot/tk8.0/lib/tk8.0
TCL_LIBRARY=
TK_XINCLUDES=-I/usr/openwin/include
TK_INCLUDE=-I/local/sol/2.6/depot/tk8.0/lib/../include
TCL_INCLUDE=-I/local/sol/2.6/depot/tcl8.0/lib/../include
TK_LIB_SPEC=-L/local/sol/2.6/depot/tk8.0/lib -ltk8.0
TK_BUILD_LIB_SPEC=-L/local/sol/2.6/src/tk8.0/unix -ltk8.0
TK_XLIBSW=-L/usr/openwin/lib -lX11
TK_XINCLUDES=-I/usr/openwin/include
TCL_LIB_SPEC=-L/local/sol/2.6/depot/tcl8.0/lib -ltcl8.0
TCL_DL_LIBS=-ldl
TCL_LIBS=-ldl  -lsocket -lnsl -lm

NOTIFY=yes
GCC=yes





GCLDIR=/local/sol/2.6/depot/build/GCL/gcl-2.3
SHELL=/bin/sh
MACHINE=solaris

OFLAG	=  -O
LIBS	= -lm -lsocket -lnsl

# tell linker to remember where it got the shared object...
# should have the same for TCL/TK if you used shared libs..
X11_LIBS=-Xlinker -R${X11_LIBS_DIR} -L${X11_LIBS_DIR} -lX11

ODIR_DEBUG=-O4

#gcc 2.1 and 2.2 compile akcl correctly as far as I have been able to determine.
#gcc 2.3.3 does not compile akcl correctly
#gcc 2.4.5 does compile akcl, but does fail on some subsequent tests.
#gcc 2.5.3 does compile gcl correctly however it has a known bug.
#gcc 2.6.3 appears to compile gcl ok
# gcc 2.7.2 and 2.7.2.1 cause the gcl to core dump on start up of saved
#    image.  something to do with 'ctor'..   Add -B flag as in:
#    get gcc -B /lusr/gnu/lib/gcc-lib/sparc-sun-solaris2.5/2.6.3/
#    to use previous gcc.   Note final '/'
# gcc  2.8.1 compiles gcl ok (running on solaris 2.6).
# used sun's make from /usr/ccs/bin

CC = gcc -I${GCLDIR}/o  -DVOL=volatile -fsigned-char
# LDCC= ${CC} -static
# we can use non static linking now
LDCC= ${CC}
ODIR_DEBUG= -O

#The new optional for money compiler has not been tested recently.
# it used to fail to compile o/format.o correctly.
#CC =  /usr/local/lang/cc  -DVOL=  -I$(GCLDIR)/o -Bstatic -temp=. -pipe
ODIR_DEBUG= -O4

AS=/usr/ccs/bin/as -P -D__svr4__

CFLAGS	= -c $(DEFS)  -I../h

MAIN    = ../o/main.o

MPFILES=$(MPDIR)/mpi-sol-sparc.o $(MPDIR)/sparcdivul3.o $(MPDIR)/libmport.a
#MPFILES=${MPDIR}/mpi.o ${MPDIR}/libmport.a

RSYM	= rsym
SFASL	= $(ODIR)/sfasl.o

# This function will be run before dumping.
# When using SFASL it is good to have (si::build-symbol-table)
INITFORM=(si::build-symbol-table)

GNULIB1=

# Use symbolic links
SYMB=-s
# the  make to use for saved_kcp the profiler.
KCP=kcp-sun

NULLFILE = ../h/secondary_sun_magic
# no ranlib so use dummy
RANLIB=true
# use the sun ar
AR=/usr/ccs/bin/ar qc

# end makedefs
GCLDIR=/local/sol/2.6/depot/build/GCL/gcl-2.3
MAXDIR=/local/sol/2.6/depot/build/Maxima/maxima-5.4
PREFIX_DIR=/local/sol/2.6/depot/maxima-5.4
EMACS_SITE_LISP=/local/sol/2.6/depot/emacs-20.3/share/emacs/site-lisp
# end maxmakedefs

#
SRC=./src

all: 
	(cd $(SRC) ; make )


clean:
	(cd src ; make clean)
	(cd info ; make clean)

test:
	make test1 "MAXIMA=`pwd`/src/saved_maxima `pwd`/src/"

test1:
	date
	(cd doc ; ${MAXIMA} -dir `pwd`/../src/   -load "tests.lisp" < /dev/null )
	date

#just for checking sgc is ok... not really a check on maxima.
testsgc:
	date
	(cd doc ; ${MAXIMA} -dir `pwd`/../src/ -eval '(progn (si::sgc-on t) (setq si::*notify-gbc* t))'  -load "tests.lisp" < /dev/null )
	date

time:
	@echo "***** FACTOR TEST *****"
	@date
	@echo Running on `hostname`
	cat doc/time.test | ${MAXIMA}

install:
	make install1 PREFIX_DIR=${PREFIX_DIR} INSTALL_LIB_DIR=${PREFIX_DIR}/lib/maxima-`cat majvers`.`cat minvers`

PLOTTING_BIN=bin/omplotdata bin/mgnuplot

install1: command
	if [  -f ${INSTALL_LIB_DIR}/makefile  ] ; then true; else \
	tar cvf - src/plot.o */*.mac sym/*.o sym/*.lsp sym/makefile  sym/*.tex info/*.texi info/*.info* info/makefile ${PLOTTING_BIN} \
	*/*.mc */*.dem */*.usg src/saved_maxima  | (cd ${INSTALL_LIB_DIR}/ ; tar xvf - ) ;fi
	(cd src ; make install "INSTALL_LIB_DIR=${INSTALL_LIB_DIR}")
	(cd info ; make install)
	(cd elisp ; make install)

command:
	- mkdir ${PREFIX_DIR}
	- mkdir ${PREFIX_DIR}/lib
	- mkdir ${PREFIX_DIR}/bin
	- mkdir ${INSTALL_LIB_DIR}/
	echo '#!/bin/sh' > ${PREFIX_DIR}/bin/maxima
	echo export MAXIMA_DIRECTORY >> ${PREFIX_DIR}/bin/maxima
	echo MAXIMA_DIRECTORY=${INSTALL_LIB_DIR} >> ${PREFIX_DIR}/bin/maxima
	echo exec ${INSTALL_LIB_DIR}/src/saved_maxima -dir ${INSTALL_LIB_DIR}/src/ '$$@' \
	>>  ${PREFIX_DIR}/bin/maxima
	chmod a+x ${PREFIX_DIR}/bin/maxima

tar-bin:
	make tar-bin1 PREFIX_DIR=${PREFIX_DIR} INSTALL_LIB_DIR=${PREFIX_DIR}/lib/maxima-`cat ${MAXDIR}/majvers`.`cat ${MAXDIR}/minvers`
tar-bin1:
	(cd / ; tar cvf - ${PREFIX_DIR}/bin/maxima ${INSTALL_LIB_DIR} | gzip  -c > /tmp/maxima-`cat ${GCLDIR}/machine`-`cat ${MAXDIR}/majvers`.`cat ${MAXDIR}/minvers`-bin.tgz) 

BETA=-beta
tar:
	bin/distribute maxima-`cat majvers`.`cat minvers`$(BETA).tgz

