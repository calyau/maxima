
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
EMACS_SITE_LISP=/usr/share/emacs/20.3/site-lisp

# the default.el file
EMACS_DEFAULT_EL=/usr/share/emacs/20.3/site-lisp/default.el

# numerous TCL/TK variables culled from the tkConfig.sh and tclConfig.sh
# if these are found.
TK_CONFIG_PREFIX=/usr/lib
TK_LIBRARY=/usr/lib/tk8.0
TCL_LIBRARY=/usr/lib/tcl8.0
TK_XINCLUDES=-I/usr/X11R6/include
TK_INCLUDE=-I/usr/lib/../include
TK_LIB_SPEC=-L/usr/lib -ltk8.0
TK_BUILD_LIB_SPEC=-L/work-in-progress/tk8.0/unix -ltk8.0
TK_XLIBSW=-L/usr/X11R6/lib -lX11
TK_XINCLUDES=-I/usr/X11R6/include
TCL_LIB_SPEC=-L/usr/lib -ltcl8.0
TCL_DL_LIBS=-ldl
TCL_LIBS=-ldl  -lieee -lm

NOTIFY=yes






GCLDIR=/home/wfs/gcl-2.3
SHELL=/bin/sh
MACHINE=386-linux

# Machine dependent makefile definitions for intel 386,486 running linux

LBINDIR=/usr/local/bin

OFLAG	=  -O 
LIBS	= -lm 

ODIR_DEBUG= -O4 

# This CC string will be used for compilation of the system,
# and also in the compiler::*cc* variable for later compilation of
# lisp files.
# (the -pipe is just since our file system is slow..)
CC = gcc -pipe -fwritable-strings  -DVOL=volatile -I$(GCLDIR)/o -fsigned-char
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
GCLDIR=/home/wfs/gcl-2.3
MAXDIR=/home/wfs/maxima-5.4
# end maxmakedefs

file: ${EMACS_SITE_LISP}/${FILE}

${EMACS_SITE_LISP}/${FILE}: ${MAXDIR}/elisp/${FILE}
	cp ${MAXDIR}/elisp/${FILE} ${EMACS_SITE_LISP}

install:
	- for v in *.el ; do make file "FILE=$$v" ; done
