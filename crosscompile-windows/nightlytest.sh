#!/bin/bash

# Script for nightly testruns
# should be copied to a private location (~/bin), so that
# git changes to that script can be reviewed before.
#
# a working directory is ~/maxima-test

MAXIMAGITREPOSITORY=https://git.code.sf.net/p/maxima/code
# if there is already a local repository (from the nightly Windows build),
# clone from that git repository
test -d ~/maxima-code/.git && MAXIMAGITREPOSITORY=~/maxima-code

mkdir -p ~/maxima-test

cd ~/maxima-test || exit

rm -rf -- * .git*


git clone $MAXIMAGITREPOSITORY .

export PATH=/opt/ccl:$PATH


./bootstrap >logfile-bootstrap.txt 2>&1

./configure --enable-clisp --enable-ecl --enable-sbcl --enable-gcl --enable-ccl64 --with-ccl64=lx86cl64 --prefix=$(pwd)/installroot >logfile-configure.txt 2>&1

make VERBOSE=1 >logfile-make.txt 2>&1
make pdf VERBOSE=1 >logfile-makepdf.txt 2>&1
make install VERBOSE=1 >logfile-makeinstall.txt 2>&1
make dist VERBOSE=1 >logfile-makedist.txt 2>&1

~/maxima-test/installroot/bin/maxima --run-string="build_info();" >logfile-buildinfo.txt
for lisp in clisp ecl sbcl gcl ccl64 ; do
      ~/maxima-test/installroot/bin/maxima --lisp=$lisp --run-string="run_testsuite();" >logfile-testsuite-$lisp.txt
      echo "$lisp summary" >>logfile-summary.txt
      sed -n -e '/^Error summary\|^No unexpected errors/,$p' logfile-testsuite-$lisp.txt >>logfile-summary.txt
      echo >>logfile-summary.txt
      echo >>logfile-summary.txt
      echo >>logfile-summary.txt
done

scp -i ~/.ssh/maximakopierkey ~/maxima-test/logfile-*.txt maxima@ns1.dautermann.at:/var/www/wolfgang.dautermann.at/maxima/nightlybuild/
