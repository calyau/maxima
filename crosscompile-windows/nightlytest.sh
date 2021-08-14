#!/bin/bash
# SPDX-License-Identifier: GPL-2.0-or-later
# Script for nightly testruns
# should be copied to a private location (~/bin), so that
# git changes to that script can be reviewed before.
#
# the working directory is ~/maxima-test - attention: it will be deleted and recreated.

# do everything in English
export LANG=C

MAXIMAGITREPOSITORY=https://git.code.sf.net/p/maxima/code

rm -rf ~/maxima-test

git clone $MAXIMAGITREPOSITORY ~/maxima-test
cd ~/maxima-test || exit

./bootstrap >logfile-bootstrap.txt 2>&1

echo "./configure"
./configure --enable-clisp --enable-ecl --with-ecl=/opt/ecl-21.2.1/bin/ecl --enable-sbcl --with-sbcl=/opt/sbcl-2.1.7/bin/sbcl --enable-gcl --enable-ccl64 --with-ccl64=/opt/ccl/lx86cl64 --enable-cmucl --with-cmucl=/opt/cmucl-21d/bin/lisp --with-cmucl-runtime=/opt/cmucl-21d/bin/lisp --enable-acl --with-acl=/opt/acl10.1express/alisp --enable-abcl --with-abcl-jar=/opt/abcl-bin-1.8.0/abcl.jar --prefix="$(pwd)/installroot" >logfile-configure.txt 2>&1

echo "make"
make VERBOSE=1 >logfile-make.txt 2>&1
echo "make pdf"
make pdf VERBOSE=1 >logfile-makepdf.txt 2>&1
echo "make install"
make install VERBOSE=1 >logfile-makeinstall.txt 2>&1
echo "make dist"
make dist VERBOSE=1 >logfile-makedist.txt 2>&1

# limit GCLs memory consumption to 20% of the main memory:
export GCL_MEM_MULTIPLE=0.2

~/maxima-test/installroot/bin/maxima --batch-string="build_info();" >logfile-buildinfo.txt
commands=$(
for lisp in abcl clisp ecl sbcl gcl ccl64 cmucl acl ; do
      echo "echo Running Maxima testsuite with $lisp ; /usr/bin/time --portability --output=logfile-timing-testsuite-$lisp.txt ~/maxima-test/installroot/bin/maxima --lisp=$lisp --batch-string='run_testsuite();' >logfile-testsuite-$lisp.txt 2>&1"
      echo "echo Running Maxima share testsuite with $lisp ; /usr/bin/time --portability --output=logfile-timing-share-testsuite-$lisp.txt ~/maxima-test/installroot/bin/maxima --lisp=$lisp --batch-string='run_testsuite(share_tests=only);' >logfile-share-testsuite-$lisp.txt 2>&1"
done
)
echo "$commands" | parallel --no-notice


rm -f logfile-summary.txt logfile-share-summary.txt logfile-timings-summary.txt
for lisp in clisp ecl sbcl gcl ccl64 cmucl acl abcl ; do
      echo "$lisp summary" >>logfile-summary.txt
      echo "$lisp summary" >>logfile-share-summary.txt
      echo -e "$lisp summary" >>logfile-timings-summary.txt
      echo -e "\nMain testsuite"  >>logfile-timings-summary.txt
      cat logfile-timing-testsuite-$lisp.txt >>logfile-timings-summary.txt
      echo -e "\nShare testsuite"  >>logfile-timings-summary.txt
      cat logfile-timing-share-testsuite-$lisp.txt >>logfile-timings-summary.txt
      sed -n -e '/^Error summary\|^No unexpected errors/,$p' logfile-testsuite-$lisp.txt >>logfile-summary.txt
      sed -n -e '/^Error summary\|^No unexpected errors/,$p' logfile-share-testsuite-$lisp.txt >>logfile-share-summary.txt
      echo -e "\n\n" >>logfile-summary.txt
      echo -e "\n\n" >>logfile-share-summary.txt
      echo -e "\n\n-----------------------------------\n" >>logfile-timings-summary.txt
done

# remove the single timing files before copying to the server
# every information is in the logfile-timings-summary.txt file.
rm logfile-timing-*
scp -i ~/.ssh/maximakopierkey ~/maxima-test/logfile-*.txt maxima@ns3.dautermann.at:/var/www/wolfgang.dautermann.at/maxima/nightlybuild/
