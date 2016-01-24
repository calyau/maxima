#!/bin/bash

# compiliation-script for nightly builds 

# do everything in english:
LANG=C
export LANG

CMAKE=/opt/cmake-3.4.2-Linux-x86_64/bin/cmake

cd build
rm -rf *
WINEPREFIX=$(pwd)/wine
export WINEPREFIX
$CMAKE -DBUILD_CURRENT=YES .. >buildlog 2>&1
xvfb-run -a -s "-screen 0 1024x768x24" make 2>&1 | tee -a buildlog
make package >> buildlog
echo >>buildlog
echo >>buildlog
echo "Build Information:">>buildlog
lsb_release  -d >>buildlog
echo -n "Maxima GIT Version: ">>buildlog
git describe >>buildlog
echo -n "Wxmaxima GIT Version: ">>buildlog
(cd wxMaxima-git-prefix/src/wxMaxima-git/ ; git describe) >>buildlog
echo -n "Build date and time (UTC): " >>buildlog
date --utc >>buildlog

cd ..
