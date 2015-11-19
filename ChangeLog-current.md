Maxima 5.37 change log
======================

New items in core:
------------------
 * commit [ae11414]: Images in the HTML manual now are automatically shrinked to fit
   within the browser window, if neeeded.
 * commit [d817ac5]: A singlepage HTML manual (doc/info/maxima_singlepage.html)

New items in share:
-------------------
 * commits [68d866e]-[4d1f046]: draw: grid now not only accepts boolean values, but also
   a list of 2 numbers.

Changes in core:
----------------
 * commits [3631127]-[82aea3a]: Documentation: Add example images for the draw options.

Changes in share:
--------------


Bug fixes:
----------


Unnumbered bugs:
----------------

 * commit [f4af2a1]: Use per-maxima-instance filenames for draw and plot. Without this simultaneous plotting from concurrent maxima instances might result in name collisions.
 * commit [5cc41da]: Allow sbcl to use enough memory to compile Lapack (linux+mac only).
 * commit [4be3d80]: Remove the two black lines at the border of the bounding box that were introduced with gnuplot 5.0
