Maxima can now be built using a purely lisp-based procedure. This
procedure is not yet as polished as the GNU Autotools system described
in the file INSTALL.

Note: xmaxima cannot be built using this procedure.

Note (2): Plotting on Windows does not (yet) work using this procedure.

To build Maxima:

1) Launch your lisp implementation.

2) Load the file configure.lisp.

3) Execute "(configure)". You will be prompted for several inputs. Hit
   return to use the default values. The configure process can be
   automated through the use of optional arguments to configure. See
   the file configure.lisp for details.

4) Change to the src directory.

Maxima builds with defsystem. The file maxima-build.lisp is provided
for rudimentary guidance in using defsystem. Experts should feel free
to subsitute their knowledge of defsystem for the following steps.

5) Load maxima-build.lisp.

6) Execute "(maxima-compile)".

7) Optionally, quit Lisp at this point and restart.

8) Execute "(maxima-load)".

9) Dump an image, using (user::run) for the startup function if
   possible. The command "(maxima-dump)" will work for a (very)
   limited set of Lisp(s).

Two scripts are provided to act as front-ends to the dumped lisp
image. The script "maxima" requires Bourne shell. Even if Bourne shell
is not available on your system, it is worth looking at the way images
are invoked at the end of the script. You will have to manually "chmod
+x maxima". The file "maxima.bat" provides a Windows batch file
interface roughly equivalant to "maxima".

User feedback on this procedure would be greatly appreciated.