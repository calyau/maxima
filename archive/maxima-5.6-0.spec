Summary: Maxima Symbolic Computation Program
Name: maxima
Version: 5.6
Release: 0
Copyright: GPL
Group: Development/Languages
Source0: ftp.ma.utexas.edu:/pub/maxima/maxima-5.6.tgz
Source1: ftp.ma.utexas.edu:/pub/gcl/gcl-2.4.0.tgz

%description

Maxima is a full symbolic computation program.  It is full featured
doing symbolic manipulation of polynomials, matrices, rational
functions, integration, Todd-coxeter, graphing, bigfloats.  It has a
symbolic debugger source level debugger for maxima code.  Maxima is
based on the original Macsyma developed at MIT in the 1970's.  It is
quite reliable, and has good garbage collection, and no memory leaks.
It comes with hundreds of self tests.  William Schelter at University
of Texas, has been responsible for development since the mid 1980's.
See http://www.ma.utexas.edu/maxima.html for more information.  He has
recently been able to get DOE to allow him to distribute Maxima under
the GPL.

%prep
%setup -b 1

%build


MAXIMA_DIR=`pwd`
GCL=`echo ${MAXIMA_DIR}/../gcl*`
#build gcl
cd ${GCL}
./configure
make

#build maxima after setting up the paths
cd ${MAXIMA_DIR}

# grab emacs lisp directory where we will put .el files:
EMACS_SITE_LISP=/usr/share/emacs/site-lisp/maxima

# fix the directories for Red Hat
cat configure.in | sed -e "s:GCLDIR=.*:GCLDIR=${GCL}:g" \
 -e "s:^MAXDIR=.*:MAXDIR=${MAXIMA_DIR}:g" \
 -e "s:^PREFIX_DIR=.*:PREFIX_DIR=/usr:g" \
 -e "s:^INFO_DIR=.*:INFO_DIR=/usr/share/info:g" \
 -e "s:^MAN_DIR=.*:MAN_DIR=/usr/share/man/man1:g" \
 -e "s:^EMACS_SITE_LISP=.*:EMACS_SITE_LISP=${EMACS_SITE_LISP}:g" > configure
chmod a+x configure
./configure
make

%install

# establish directories:
MAXIMA_DIR=`pwd`
GCL=`echo ${MAXIMA_DIR}/../gcl*`
EMACS_SITE_LISP=/usr/share/emacs/site-lisp/maxima

if test -d ${EMACS_SITE_LISP} ; then true ; else mkdir ${EMACS_SITE_LISP} ; fi

make install
# copy .el files from gcl also, they are needed, and

(cd ${GCL}/elisp ; tar cvf - *.el) | (cd ${EMACS_SITE_LISP} ; tar xvf -)

START=${EMACS_SITE_LISP}/../site-start.el

# add the path where we put the maxima .el files, and
# add autoloads
if fgrep maxima ${START} > /dev/null ;
 then true ; else
 cat ${MAXIMA_DIR}/elisp/add-defaults.el >> ${START}
 echo "(setq load-path (cons \"${EMACS_SITE_LISP}\" load-path))" >> ${START}
fi

%files
%doc README COPYING 
/usr/share/emacs/site-lisp/maxima
/usr/lib/maxima-5.5
/usr/share/info/maxima*
/usr/share/man/man1/maxima.1
/usr/bin/maxima
/usr/bin/xmaxima

%postun
cat /usr/share/emacs/site-lisp/site-start.el | sed -e '/BEGIN maxima/,/lisp\/maxima/d' > foo
cp foo /usr/share/emacs/site-lisp/site-start.el
rm -f foo



