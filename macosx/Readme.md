MAXIMA.APP
===========

Quick instructions for building a Maxima.app on Mac OS X.

Platypus
---------

The package is built with the Platypus application. Install Platypus
from http://sveinbjorn.org/platypus. You also need to install the
command line tools (start the Platypus application and install command
line tools from the Preferences dialog).

Maxima.app
-----------

The default configuration uses sbcl which is installed as
/usr/local/bin/sbcl, but other lisps can be used too. It creates a
Maxima.app on the Desktop.

To create Maxima.app with default settings execute

   make -f macosx/Makefile

To use a different lisp and set the version execute

    make -f macosx/Makefile VERSION=5.24.0 LISP_NAME=cmucl \
      LISP_PROGRAM=/usr/local/bin/lisp

Maxima will be configured with correct options, built and installed
into Maxima.app. Some parts of Maxima.app are created with Platypus.

Running Maxima
---------------

Double-cliking Maxima.app starts maxima in a new Terminal window.

To run maxima from Terminal, execute
Maxima.app/Contents/Resources/maxima.sh

Creating a .dmg file with maxima and wxMaxima
---------------------------------------------

 - Open the Disk Utility
 - Create a new Disk Image (read/write; fixed size and big enough for maxima and
   wxmaxima. If there is empty space at the end of the image that is fine: After
   compressing the image it won't use up much disk space any more)
 - Double-click on it and open the newly-mounted .dmg in the Finder.
 - Move maxima and wxMaxima into the finder window.
 - In case that the gnuplot project doesn't build a gnuplot image in a format we
   can include besides maxima and wxMaxima a suitable gnuplot may be found at
   http://ricardo.ecn.wfu.edu/pub/gnuplot.
 - Add "How to install.rtf" to the image.
 - Create a symlink to /Applications in the folder containing the three
   applications
 - Switch the window to icon view and arrange icons as needed.
 - Go to the "view options" and add a maxima Logo as the background
 - re-enter the Disk Utility
 - return to the Disk Utility, right-click on the .dmg disk image
   and select "Convert to read-only".
 - Done.