# edit ./configure to reflect paths at your site.  This will insert
# things in these directories.  Then do './configure' 

# the lines down to '# end makedefs' are added by ./configure from the gcl dir.

# place to put maxima in installation procedure
PREFIX_DIR=/usr/local
# the above will result in /usr/local/bin/maxima and /usr/local/lib/maxima-x.x

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

# end makedefs
GCLDIR=/home/wfs/cvs/gcl
MAXDIR=/home/wfs/cvs/maxima
PREFIX_DIR=/usr/local
EMACS_SITE_LISP=/usr/local/lib/emacs/site-lisp
INFO_DIR=/usr/local/info
MAN_DIR=/usr/local/man/man1
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
	tar cvf - doc/manual.demo src/plot.o doc/*.mac doc/tests.lisp src/server.lisp intro.html doc/*.html info/*.html */*.mac sym bin/xmaxima maxima.gif info/*.texi info/*.info* info/makefile ${PLOTTING_BIN} \
	*/*.mc */*.dem */*.usg src/saved_maxima${EXE}  | (cd ${INSTALL_LIB_DIR}/ ; tar xvf - ) ;fi
	(cd src ; make install "INSTALL_LIB_DIR=${INSTALL_LIB_DIR}")
	(cd info ; make install)
	(cd elisp ; make install)
	cp  ./doc/maxima.1 ${MAN_DIR}
	chmod a+r ${MAN_DIR}/maxima.1

command:
	for v in ${PREFIX_DIR} ${PREFIX_DIR}/lib ${PREFIX_DIR}/bin  ${INSTALL_LIB_DIR}/ ; do if test -d $$v ; then true ; else mkdir $$v ; fi ; done
	echo '#!/bin/sh' > ${PREFIX_DIR}/bin/maxima
	echo export MAXIMA_DIRECTORY >> ${PREFIX_DIR}/bin/maxima
	echo MAXIMA_DIRECTORY=${INSTALL_LIB_DIR} >> ${PREFIX_DIR}/bin/maxima
	echo exec ${INSTALL_LIB_DIR}/src/saved_maxima -dir ${INSTALL_LIB_DIR}/src/ '$$@' \
	>>  ${PREFIX_DIR}/bin/maxima
	chmod a+x ${PREFIX_DIR}/bin/maxima
	echo '#!/bin/sh' > ${PREFIX_DIR}/bin/xmaxima
	echo export MAXIMA_DIRECTORY >> ${PREFIX_DIR}/bin/xmaxima
	echo MAXIMA_DIRECTORY=${INSTALL_LIB_DIR} >> ${PREFIX_DIR}/bin/xmaxima
	echo exec ${INSTALL_LIB_DIR}/bin/xmaxima  '$$@' >>  ${PREFIX_DIR}/bin/xmaxima
	chmod a+x ${PREFIX_DIR}/bin/xmaxima


tar-bin:
	make tar-bin1 PREFIX_DIR=${PREFIX_DIR} INSTALL_LIB_DIR=${PREFIX_DIR}/lib/maxima-`cat ${MAXDIR}/majvers`.`cat ${MAXDIR}/minvers`
tar-bin1:
	(cd / ; tar cvf - ${PREFIX_DIR}/bin/maxima ${INSTALL_LIB_DIR} | gzip  -c > /tmp/maxima-`cat ${GCLDIR}/machine`-`cat ${MAXDIR}/majvers`.`cat ${MAXDIR}/minvers`-bin.tgz) 

BETA=-beta
tar:
	bin/distribute maxima-`cat majvers`.`cat minvers`$(BETA).tgz


maxima.zip: src/saved_maxima.exe
	rm -f maxima.zip
	OTHERS=`make -s -f src/makefile othernames` ; \
	zip maxima.zip $${OTHERS} src/server.lisp doc/manual.demo */*.dem doc/*.mac src/xmaxima.exe info/*.html src/saved_maxima.exe .exe info/*info* doc/*.html src/*.ico src/winkill.exe src/tclwinkill.dll maxima.gif doc/tests.lisp


DL=./deliver/

${DL}zipinfo.tcl ${DL}setup_contents.txt: maxima.zip gcc.zip ${DL}pkginfo.tcl
	tclsh  ${DL}getzipinfo.tcl

DELIVER_FILES=${DL}setup.tcl ${DL}unpack.tcl  ${DL}install1.gif  ${DL}fdlogo3.gif  ${DL}pkginfo.tcl ${DL}zipinfo.tcl  ./src/maxicon.gif ./COPYING ./readme.xmaxima

gcc.zip:
	zip -r gcc.zip gcc

${DL}setup.exe: gcc.zip maxima.zip ${DL}setup_contents.txt ${DL}zipinfo.tcl ${DELIVER_FILES}
	freewrap-cross  ${DELIVER_FILES} -p ${DL}setup_contents.txt


	

