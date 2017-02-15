#!/bin/bash

# compiliation-script for nightly builds 

# do everything in english:
LANG=C
export LANG

CMAKE=/opt/cmake-3.7.2-Linux-x86_64/bin/cmake
BUILDLOG=buildlog-win32

cd build
rm -rf *
echo "32 Bit Build Information:">$BUILDLOG
$CMAKE -DBUILD_CURRENT=YES .. >>$BUILDLOG 2>&1
make 2>&1 | tee -a $BUILDLOG
make package >> $BUILDLOG
echo >>$BUILDLOG
echo >>$BUILDLOG
echo "Build Information:">>$BUILDLOG
lsb_release  -d >>$BUILDLOG
echo -n "Maxima GIT Version: ">>$BUILDLOG
git describe >>$BUILDLOG
echo -n "Wxmaxima GIT Version: ">>$BUILDLOG
(cd wxmaxima/wxmaxima-git-prefix/src/wxmaxima-git ; git describe) >>$BUILDLOG
echo -n "Build date and time (UTC): " >>$BUILDLOG
date --utc >>$BUILDLOG
cp $BUILDLOG maxima-clisp-sbcl-current-win32.exe ~
cd ..

BUILDLOG=buildlog-win64
cd build
rm -rf *
echo "64 Bit Build Information:">$BUILDLOG
$CMAKE -DBUILD_CURRENT=YES -DBUILD_64BIT=YES .. >>$BUILDLOG 2>&1
make 2>&1 | tee -a $BUILDLOG
make package >> $BUILDLOG
echo >>$BUILDLOG
echo >>$BUILDLOG
echo "Build Information:">>$BUILDLOG
lsb_release  -d >>$BUILDLOG
echo -n "Maxima GIT Version: ">>$BUILDLOG
git describe >>$BUILDLOG
echo -n "Wxmaxima GIT Version: ">>$BUILDLOG
(cd wxmaxima/wxmaxima-git-prefix/src/wxmaxima-git ; git describe) >>$BUILDLOG
echo -n "Build date and time (UTC): " >>$BUILDLOG
date --utc >>$BUILDLOG
cp $BUILDLOG maxima-clisp-sbcl-current-win64.exe ~
cd ..

