Maxima 5.37 change log
======================

New items in core:
------------------

* new function with_default_2d_display: ensure pretty-printing output 


New items in share:
-------------------

* new package cryptools: tools for cryptography
* new package elliptic_curves: elliptic curves over prime fields and binary fields
* new function cgrind: output Maxima expressions as C code
* package draw: new function vennplot (Venn diagrams)
* package stringproc: new functions flush_output, readbyte, and writebyte

Changes in core:
----------------

* function gf_symmetric renamed to gf_balanced 


Changes in share:
--------------

* functions md5sum, sha1sum, sha256sum and base64_decode: accept and return numbers and octet lists as well as strings 
* packages aes and aes2: provide flexible support for different data types, e.g. octet lists 


Bug fixes:
----------

* [#2998]: extra () in display2d:false output
* [#2988]: documentation error in gf_manual.pdf 
* [#2987]: Some divergent integrals give error, some don't
* [#2982]: Display of taylor series in wrong order
* [#2980]: "rectform" causes infinite recursion depending on variable name 
* [#2975]: number of distinct partitions gives wroing result
* [#2972]: Wrong limits involving logs
* [#2937]: dotscrules and antisymmetric
* [#2936]: stack overflow in integrate
* [#2934]: dotscrules and antisymmetric
* [#2929]: misformatting in debugger help message
* [#2905]: Assigning variable twice yields different results
* [#2230]: abs(x)^(2*int) doesn't simplify
* [#2211]: rtest_sign #77
* [#2183]: eigenvectors of a 10 x 10

Unnumbered bugs:
----------------

* commit [6779ac0]: zn-nrt: root of zero should be zero
* mailing list 2015-06-11: [rationalize(0.1) and the manual][1]

[1]: https://sourceforge.net/p/maxima/mailman/message/34196346/
