#!/bin/bash

# compiliation-script for nightly builds
# should be copied to a private location (~/bin), so that
# git changes to that script can be reviewed before.
#
# expects the git checkout in ~/maxima-code
#
# calling from a cronjob does not work (don't know why),
# so call it from a screen session (which you can detach)
# and do everything in a loop


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

# should be called as buildprocess win32  or buildprocess win64
buildprocess () {
    rm -rf -- *
    echo "$1 build log:"
    if [ "$1" == "win64" ]
    then
        $CMAKE -DBUILD_CURRENT=YES -DBUILD_64BIT=YES ..
    else
        $CMAKE -DBUILD_CURRENT=YES ..
    fi
    make
    make package
    echo
    echo
    buildinformation
    cp "maxima-clisp-sbcl-current-$1.exe" ~
}

# sleep until a given time
sleepuntil () {
    sleep $(( (24*60*60 + $(date -d "$1" +%s) - $(date +%s) ) % (24*60*60) ))
}

# do everything in english:
LANG=C
export LANG

CMAKE=/opt/cmake-3.7.2-Linux-x86_64/bin/cmake

cd ~/maxima-code/crosscompile-windows/build || exit

while true; do

    git pull

    buildprocess "win32" 2>&1 | tee ~/buildlog-win32
    buildprocess "win64" 2>&1 | tee ~/buildlog-win64

    scp -i ~/.ssh/maximakopierkey ~/maxima-clisp-sbcl-current-win32.exe ~/maxima-clisp-sbcl-current-win64.exe ~/buildlog-win32 ~/buildlog-win64 maxima@ns1.dautermann.at:/var/www/wolfgang.dautermann.at/maxima/nightlybuild/
    sleepuntil 23:00
done
