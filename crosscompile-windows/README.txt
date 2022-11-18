Crosscompiling Maxima for Windows
=================================

On a Ubuntu/Debian System just install some tools for crosscompiling:

apt-get install g++-mingw-w64-x86-64 cmake nsis wine automake texlive texlive-plain-generic texlive-xetex rsync p7zip-full g++ gettext python3 tcl pandoc po4a

The Mingw compiler comes in two flavors for threading (win32 and posix threads).
wxMaxima requires posix threads, so you must reconfigure mingw and select the posix
version, on Debian/Ubuntu Linux using:
update-alternatives --config x86_64-w64-mingw32-g++
update-alternatives --config x86_64-w64-mingw32-gcc

You will need CMake >= 3.10, if that is not included in your distribution,
download a recent CMake from https://cmake.org/files/

Then you can extract the Maxima sourcecode or clone the git repository
and start the crosscompiling-process:

mkdir crosscompile-windows/build
cd crosscompile-windows/build # change to the build directory
cmake ..  # use the right CMake executable (CMake >= 3.10)
make
make package

The build directory should be called "build", because the Maxima code
is copied during the build and this directory will be excluded.

This will download the required Software (CLISP, SBCL, Gnuplot, wxMaxima,
wxWidgets, Tcl, Tk, VTK) from the Internet into the directory
"crosscompile-windows/download".

The packages will be compiled (if necessary) and a Windows 
installer for Maxima is generated.

This should work (at least) on Ubuntu and Debian (I hope on other
Linux-Distributions too...).
(if you want you may even omit the first "make")

Instead of "make clean" just remove everything in the build directory.

If you want to use the current wxMaxima development version, you can use
cmake -DUSE_WXMAXIMA_GIT=YES ..

If you want to exclude VTK, use
cmake -DUSE_VTK=NO ..
(The size of the installer with VTK will approximately be 50% larger
than without VTK).

One can also include ABCL - a Java based Lisp compiler - with the option
-DWITH_ABCL=YES
Of course you will need a Java installation.

If you want to change the default Lisp, which will be used, you can
use the option "-DWITH_DEFAULT_LISP=clisp" (or abcl) in the cmake call
(otherwise SBCL would be the default).


In case a new release of a software is released (and no new patches are needed),
it should be sufficient to just increase the version number and MD5-checksum
for the new release in CMakeLists.txt.


Building a 32 bit installer
===========================

By default a 64 bit installer will be generated.
If you want to crosscompile a 32 bit installer, install the 32 bit
crosscompiler package (i686-w64-mingw32-g++) - and reconfigure it
for posix threads.
It might be necessary to add the i386 architecture:
https://wiki.debian.org/Multiarch/HOWTO

Then use the following commands to build a 32 bit installer:
cmake -DBUILD_64BIT=NO ..
make
make package


Installing the package
======================

Of course just by double-clicking/executing the generated EXE on
Windows. This Maxima installer installs into C:\maxima-VERSION,
changing this directory is not recommended (because special characters
or foreign language characters might cause problems).

If you want to do an *unattended* installation (e.g. if you plan to
install Maxima on many computers in a school, university or company),
this installer (and uninstaller) understands the command line switch
"/S" (for 'silent install').

To select an installation directory for a unattended installation, use
"/D=directory", e.g. to install to C:\maxima the command would be:
maxima-VERSION-win64.exe /S /D=C:\maxima
This parameter must be the last one.

The installer supports components, you can deselect (by default a full
installation is done) some parts either during the installation process
or using the command line switches:
- VTK using the command line option /no_vtk
- Gnuplot using /no_gnuplot (installation is strongly recommended!)
- wxMaxima using /no_wxmaxima (installation is strongly recommended!)

These switches are mainly useful for an unattended installation.


Testing the installed package:
==============================

After building it, you can (and should) test the new Maxima installation 
package. Install it on Windows and check that the installation (and later
the deinstallation) works properly. To test Maxima, try the following:

 o Run the maxima testsuite: run_testsuite();
 o Try compiling a function.  This has been a problem in the past
    f(x):=x+2;
    compile(f);
    f(2);
 o Test the graphics systems in both xmaxima and wxmaxima
    plot2d(sin(x),[x,0,10]);
    plot2d(sin(x),[x,0,10],[plot_format,xmaxima]);
    plot3d(x*y,[x,-1,1],[y,-1,1]);
    plotdf([-y,x],[trajectory_at,5,0]);
    load(draw)$
    draw3d(xu_grid = 30, yv_grid = 60, surface_hide = true,
          parametric_surface(cos(phi) * sin(theta),
                       sin(phi) * sin(theta),
                       cos(theta),
                       theta, 0, %pi, phi, 0, 2 * %pi))$
 o Check that plotting to Postscript works
    plot2d(sin(x),[x,0,10],[ps_file,"ps_test.ps"]);
 o Try out the on-line help: describe(sin);
 o Try out, if external packages (e.g. lapack) work:
   load(lapack);
   fpprintprec : 6;
   M : matrix ([9.5, 1.75], [3.25, 10.45]);
   dgeev (M);

   should return the eigenvalues of M (and false, false since we did
   not compute eigenvectors: [[7.54331, 12.4067], false, false]

 o Try if double-clicking on a .wxmx file opens it
 o The wxMaxima source comes with a file (test/testbench_simple.wxmx)
   that tries to trigger everything that has gone wrong in previous
   wxMaxima builds.  They include the commands that will test the
   graphics system in the next step.
   Open that file and then select "Cells/Evaluate all cells" in this
   file and check if the file is processed correctly.


Further development / TODO:
===========================

More packages could be included in the Windows installer. 

- Userinterface: Emacs maxima/imaxima mode:
  Should be rather easy, there are Windows zip files which could be included.
  But the size of the Maxima installer would be *much* larger.
  (Discussion on the Maxima mailing list (2020-02-28), that that does not
  make much sense.
  Users should install Emacs / Miktex(or Texlive) by themselves, if they want
  to use that interface).
- Userinterface Climaxima (https://github.com/lokedhs/maxima-client):
  Might be possible, but many dependencies, which must be crosscompiled.
- rlwrap (https://github.com/hanslub42/rlwrap):
  would provide readline features (advanced command line editing) for
  command line Maxima, when using SBCL or ABCL (CLISP already supports this).
  Dependencies: readline and that requires either termcap or ncurses
  (and neither termcap nor ncurses can be crosscompiled here...)
- Other Lisp versions: 
  Must have a Windows port or be crosscompileable and be usable in Wine.
  I tried other Lisps, but currently only CLISP, SBCL and ABCL work.
  And including many more Lisp versions might confuse ordinary users.


Wolfgang Dautermann

