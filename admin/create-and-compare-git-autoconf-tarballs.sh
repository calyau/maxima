#!/bin/sh
# Check out a tagged maxima version
# Create a package using "make dist-gzip" and "git archive"
# Create a list of files in each package and a report of differences.
#
# Files which are in the archive created with 'git archive' but not in the archive with 'make dist-gzip'
# - might be missing in the GNU autoconf build system
# - might be obsolete and can maybe removed from git (after all it is a VCS, so older versions can be recovered...)
# - should be in the GIT repository but not in the distributed tarballs (why?).
#   (if that is the case, they can be excluded from git archive exports using gitattributes (export-ignore))
#
# Files which are in the archive created with 'make dist-gzip' but not in the archive created with 'git archive'
# - are maybe generated files. One can probably remove them from the 'make dist-gzip'-archive (in the autoconf build system)
#
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 maximaversion"
  echo "e.g.: $0 5.37.3"
  echo "      to package Maxima 5.37.3 from the git tag 5.37.3"
  echo
  echo "I recommend, that $0 should be called from an empty directory"
  exit 1
fi

MAXIMAVERSION="$1"


# fetch Git tag $MAXIMAVERSION from Git repo into directory maxima-$MAXIMAVERSION
fetch_git() {
	echo "fetching Git tag $MAXIMAVERSION from Sourceforge into maxima-$MAXIMAVERSION"
	git archive --format=tar --prefix=maxima-$MAXIMAVERSION/ --remote=git://git.code.sf.net/p/maxima/code  $MAXIMAVERSION | tar xf -
}

# add "./configure" to the extracted git archive and package again
create_git_tarball() {
	echo "Running ./bootstrap (to create 'configure' and packaging again."
	cd maxima-$MAXIMAVERSION/
	./bootstrap
	cd ..
	tar czf maxima-$MAXIMAVERSION.tar.gz maxima-$MAXIMAVERSION
}


# add "./configure" by running ./bootstrap and package with "make dist-gzip"
create_autoconf_tarball() {
	echo "Packaging with 'make dist-gzip'."
	cd maxima-$MAXIMAVERSION/
	./bootstrap
	./configure
	make dist-gzip
	cd ..
}

# create list of files in both tarballs and a list of differences.
create_list_of_files() {
	echo "Creating a list of files in both archives and differences."
	tar tzf maxima-$MAXIMAVERSION/maxima-$MAXIMAVERSION.tar.gz  | sort >dist-gzip-files
	tar tzf maxima-$MAXIMAVERSION.tar.gz  | sort >git-archive-files
	diff -u dist-gzip-files git-archive-files >differences_git-archive_dist-gzip
}

fetch_git
create_git_tarball
create_autoconf_tarball
create_list_of_files

echo
echo "------------------------------------------------------------"
echo "Created maxima-$MAXIMAVERSION.tar.gz (packaged with git archive)"
echo "Created maxima-$MAXIMAVERSION/maxima-$MAXIMAVERSION.tar.gz (packaged with make dist-gzip)"
echo
echo "Created a list of files in each archive"
echo "Created a difference of these files in 'differences_git-archive_dist-gzip'"
echo "Review these differences."
