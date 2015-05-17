gCrosscompiling Maxima for Windows
=================================

On a Ubuntu/Debian System just install some tools for crosscompiling:

apt-get install g++-mingw-w64-i686 cmake nsis wine mingw-w64-dev p7zip-full rsync

then you can start the crosscompiling-process:


cd build # change to the build directory
cmake ..
make
make package

This will first download the required Software (clisp, gnuplot,
maxima, wxmaxima, wxwidgets, tcl, tk) into the directory "download"

Then the packages will be compiled (if necessary) and a windows 
installer for Maxima is generated.

This should work (at least) on Ubuntu, Debian and Opensuse.
(if you want you may even omit the first "make")

Instead of "make clean" just remove everything in the build directory.

If you want to use the current wxMaxima development version, you can use
cmake -DUSE_WXMAXIMA_GIT=YES ..



In case a software gets upgraded (and no new patches are needed), it should
be sufficient to just increase the version number and MD5-checksum for the new
release in CMakeLists.txt.


Best regards,
Wolfgang Dautermann


Some issues:
------------

Creating a windows compiled help file (chm) is only possible with 
proprietary tools and it does work without on Linux. 
So the (generated) help files from Andrej's package are included.
I suggest removing the code (or make it optional (e.g. --enable-chm-help or 
something similar) and use by default the standard HTML help (as in Linux)
Why create a proprietary file format?


