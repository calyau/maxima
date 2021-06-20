Maxima 5.45.1 change log
========================

Changes in core:
----------------
 * Reverts the changes made to plot3d to fix the mesh_lines_color option
   in newer versions of Gnuplot, because it breaks plot3d for users of older
   versions. In newer versions of Gnuplot the mesh lines will appear orange
   and mesh_lines_color will not work.

Changes in the Windows installer:
---------------------------------
 * Strip the included C programs (winkill, winkill_lib, maxima_longnames)
 * Downgrade the included Gnuplot version.
   The Gnuplot team announced, that version < 5.4 should be used with Windows.
   That solves a problem, that plots could not be rotated (bug #3796)
 * Compile the included TCL/TK statically
   Solves a strange error when TCL wanted to load the library tcl86.dll. (*)
 * Use draw_renderer:gnuplot as default for Windows/SBCL.
   gnuplot_pipes did not work with SBCL (*)
 * Downgrade the included wxWidgets to 3.1.4.
   version 3.1.5 caused a strange warning (about localization), 3.1.4
   does not have that problem. (*)

(*) These changes were already included in the binary Windows installer
    for 5.45.0, they were discovered, after the source code release was made.

Bug fixes for numbered bugs:
----------------------------
 * #926: sign errors in cartan package
 * #3789: package ezunits: ev(dimensions(u), nouns) stack overflow
 * #3793: plot2d fails on small x-range
 * #3796 plot3d doesn't support rotation in 5.45
 * #3797: plot2d(0, ...) gives "can't plot with empty y range" -- regression
 * #3801: error sourcing .xmaximarc on Windows
