# -*-mode: makefile; fill-column: 75; tab-width: 8; coding: iso-latin-1-unix -*-
#
#	$Id: Readme.txt,v 1.1 2002-09-05 20:37:29 mikeclarkson Exp $
#

Maxima Tcl Development Notes (by Mike Clarkson):
===============================================

Tkmaxima subdirectory
---------------------

The file xmaxima is generated from its components
by simply cating the components together. Originally the
components were mainly one file, xmaxima.tcl, but that
has now been broken up into files in the Tkmaxima
subdirectory. This will make the files easier to maintain, 
and ultimately allow much of the Tcl code to autoload.
The only problem about auto-loading them is that the way of finding
the directory they're in may differ in a wrapped executable.

There is a trick to breaking up the old file into the 
Tkmaxima pieces: the order of usage of global constants
may have to be preserved. Ultimately may of these
constants will go away, but for now the breaking up
must be done carefully.

Utils subdirectory
------------------

There are a number of simple Tcl utilities that are
used to help improve and/or clean up the code, in the
Utils directory. These should all be auto-loadable.
These originally come from the Tixapps collection:
http://tix.sourceforge.net/Tixapps/. The latter may
contain more recent versions, which should drop in
to overwrite these files at any time.

Naming Conventions
------------------

Filenames should always be Mixed Case, to allow for easier
cross-platform work with Windows and Unix. It makes sure
long filenames are used on Windows partitions which can be
important when working on a Windows partition from Linux.
