Maxima 5.37 change log
======================

New items in core:
------------------


New items in share:
-------------------


Changes in core:
----------------


Changes in share:
--------------


Bug fixes:
----------


Unnumbered bugs:
----------------

 * commit [f4af2a1]: Use per-maxima-instance filenames for draw and plot. Without this simultaneous plotting from concurrent maxima instances might result in name collisions.
 * commit [5cc41da]: Allow sbcl to use enough memory to compile Lapack (linux+mac only).
 * commit [4be3d80]: Remove the two black lines at the border of the bounding box that were introduced with gnuplot 5.0
