#!/bin/bash

# Debian generates libraries with the suffix libXYZ-i686-w64-mingw32.a,
# but i686-w64-mingw32-msw-unicode-static-3.0 --libs
# does not contain this suffix.

# correct the names of the generated libraries
# (libXYZ-i686-w64-mingw32.a => libXYZ.a)

cd "$1"
for i in *i686-w64-mingw32.a ; do 
    cp $i $(basename $i -i686-w64-mingw32.a).a ; 
done

