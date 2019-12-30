#!/bin/bash

# Compilation-script for nightly builds.
# Should be copied to a private location (~/bin), so that
# git changes to that script can be reviewed before.
#
# Expects the Maxima git checkout in ~/maxima-code
#
# Calling from a cronjob does not work (don't know why),
# so call it from a screen session (which you can detach)
# and do everything in a loop.


buildinformation () {
    echo "Build information"
    echo "-----------------"
    echo
    echo "Operating system: "
    lsb_release  -d
    echo -n "Maxima GIT Version: "
    git describe
    echo -n "Wxmaxima GIT Version: "
    git -C wxmaxima/wxmaxima-git-prefix/src/wxmaxima-git describe
    echo -n "Build date and time (UTC): "
    date --utc
    echo
    echo
}

# Should be called as buildprocess win32 or buildprocess win64
buildprocess () {
    rm -rf -- *
    echo "$1 build log:"
    if [ "$1" == "win64" ]
    then
        $CMAKE -DBUILD_CURRENT=YES -DWITH_ABCL=YES -DBUILD_64BIT=YES ..
    else
        $CMAKE -DBUILD_CURRENT=YES -DWITH_ABCL=YES ..
    fi
    make
    make package
    echo
    echo
    buildinformation
    cp maxima-*-current-*.exe ~
}

# sleep until a given time
sleepuntil () {
    sleep $(( (24*60*60 + $(date -d "$1" +%s) - $(date +%s) ) % (24*60*60) ))
}

# do everything in English:
LANG=C
export LANG

CMAKE=/opt/cmake-3.12.2-Linux-x86_64/bin/cmake

test -x $CMAKE || exit

cd ~/maxima-code/crosscompile-windows/build || exit

while true; do

    rm -f ~/maxima-*-win32.exe ~/maxima-*-win64.exe ~/buildlog-win32 ~/buildlog-win64
    git pull

    buildprocess "win32" 2>&1 | tee ~/buildlog-win32
    buildprocess "win64" 2>&1 | tee ~/buildlog-win64

    for i in ~/maxima-*-current-win32.exe ~/maxima-*-current-win64.exe ~/buildlog-win32 ~/buildlog-win64 ; do
        test -r $i && scp -i ~/.ssh/maximakopierkey $i maxima@ns3.dautermann.at:/var/www/wolfgang.dautermann.at/maxima/nightlybuild/
    done
    sleepuntil 23:00
done
