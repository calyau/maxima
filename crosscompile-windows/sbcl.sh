#!/bin/sh

# recreate SBCL directory structure from the extracted installer

mkdir -p contrib
# Move files File_obj_sbcl.home_contrib_* in the directory "contrib" and remove the "File_obj_sbcl.home_contrib_"-prefix:
for i in File_obj_sbcl.home_contrib_* ; do
    mv "$i" contrib/$(echo "$i" | sed s/File_obj_sbcl.home_contrib_//)
done
# Rename files in contrib directory - every but the last "." should be a "-".
# (e.g. contrib/sb-bsd.sockets.fasl ==> contrib/sb-bsd-sockets.fasl
for i in contrib/sb.* ; do
    mv "$i" $(echo "$i" | sed 's/\./-/g' | rev | sed  s/-/\./ | rev)
done

