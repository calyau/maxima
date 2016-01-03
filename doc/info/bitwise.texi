The package @code{bitwise} provides functions that allow to manipulate
bits of integer constants. As always maxima attempts to simplify the result
of the operation if the actual value of a constant isn't known considering
attributes that might be known for the variables, see the @mref{declare}
mechanism.

@menu
* Functions and Variables for bitwise::
@end menu

@node Functions and Variables for bitwise, , Top, Top
@section Functions and Variables for bitwise

@anchor{bit_not}
@deffn {Function} bit_not (@var{int})

Inverts all bits of a signed integer. The result of this action reads
@code{-int - 1}.

@c ===beg===
@c load("bitwise")$
@c bit_not(i);
@c bit_not(bit_not(i));
@c bit_not(3);
@c bit_not(100);
@c bit_not(-101);
@c ===end===
@example
(%i1) load("bitwise")$
@group
(%i2) bit_not(i);
(%o2)                      bit_not(i)
@end group
@group
(%i3) bit_not(bit_not(i));
(%o3)                           i
@end group
@group
(%i4) bit_not(3);
(%o4)                          - 4
@end group
@group
(%i5) bit_not(100);
(%o5)                         - 101
@end group
@group
(%i6) bit_not(-101);
(%o6)                          100
@end group
@end example

@opencatbox
@category{Number theory}
@category{Binary operations}
@category{Package bitwise}
@closecatbox
@end deffn

@anchor{bit_and}
@deffn {Function} bit_and (@var{int1}, ...)

This function calculates a bitwise @code{and} of two or more signed integers.

@c ===beg===
@c load("bitwise")$
@c bit_and(i,i);
@c bit_and(i,i,i);
@c bit_and(1,3);
@c bit_and(-7,7);
@c ===end===
@example
(%i1) load("bitwise")$
@group
(%i2) bit_and(i,i);
(%o2)                           i
@end group
@group
(%i3) bit_and(i,i,i);
(%o3)                           i
@end group
@group
(%i4) bit_and(1,3);
(%o4)                           1
@end group
@group
(%i5) bit_and(-7,7);
(%o5)                           1
@end group
@end example

If it is known if one of the parameters to @code{bit_and} is even this information
is taken into consideration by the function.
@c ===beg===
@c load("bitwise")$
@c declare(e,even,o,odd);
@c bit_and(1,e);
@c bit_and(1,o);
@c ===end===
@example
(%i1) load("bitwise")$
@group
(%i2) declare(e,even,o,odd);
(%o2)                         done
@end group
@group
(%i3) bit_and(1,e);
(%o3)                           0
@end group
@group
(%i4) bit_and(1,o);
(%o4)                           1
@end group
@end example

@opencatbox
@category{Number theory}
@category{Binary operations}
@category{Package bitwise}
@closecatbox
@end deffn

@anchor{bit_or}
@deffn {Function} bit_or (@var{int1}, ...)

This function calculates a bitwise @code{or} of two or more signed integers.

@c ===beg===
@c load("bitwise")$
@c bit_or(i,i);
@c bit_or(i,i,i);
@c bit_or(1,3);
@c bit_or(-7,7);
@c ===end===
@example
(%i1) load("bitwise")$
@group
(%i2) bit_or(i,i);
(%o2)                           i
@end group
@group
(%i3) bit_or(i,i,i);
(%o3)                           i
@end group
@group
(%i4) bit_or(1,3);
(%o4)                           3
@end group
@group
(%i5) bit_or(-7,7);
(%o5)                          - 1
@end group
@end example

If it is known if one of the parameters to @code{bit_or} is even this information
is taken into consideration by the function.
@c ===beg===
@c load("bitwise")$
@c declare(e,even,o,odd);
@c bit_or(1,e);
@c bit_or(1,o);
@c ===end===
@example
(%i1) load("bitwise")$
@group
(%i2) declare(e,even,o,odd);
(%o2)                         done
@end group
@group
(%i3) bit_or(1,e);
(%o3)                         e + 1
@end group
@group
(%i4) bit_or(1,o);
(%o4)                           o
@end group
@end example

@opencatbox
@category{Number theory}
@category{Binary operations}
@category{Package bitwise}
@closecatbox
@end deffn

@anchor{bit_xor}
@deffn {Function} bit_xor (@var{int1}, ...)

This function calculates a bitwise @code{or} of two or more signed integers.

@c ===beg===
@c load("bitwise")$
@c bit_xor(i,i);
@c bit_xor(i,i,i);
@c bit_xor(1,3);
@c bit_xor(-7,7);
@c ===end===
@example
(%i1) load("bitwise")$
@group
(%i2) bit_xor(i,i);
(%o2)                           0
@end group
@group
(%i3) bit_xor(i,i,i);
(%o3)                           i
@end group
@group
(%i4) bit_xor(1,3);
(%o4)                           2
@end group
@group
(%i5) bit_xor(-7,7);
(%o5)                          - 2
@end group
@end example

If it is known if one of the parameters to @code{bit_xor} is even this information
is taken into consideration by the function.
@c ===beg===
@c load("bitwise")$
@c declare(e,even,o,odd);
@c bit_xor(1,e);
@c bit_xor(1,o);
@c ===end===
@example
(%i1) load("bitwise")$
@group
(%i2) declare(e,even,o,odd);
(%o2)                         done
@end group
@group
(%i3) bit_xor(1,e);
(%o3)                         e + 1
@end group
@group
(%i4) bit_xor(1,o);
(%o4)                         o - 1
@end group
@end example

@opencatbox
@category{Number theory}
@category{Binary operations}
@category{Package bitwise}
@closecatbox
@end deffn


@anchor{bit_lsh}
@deffn {Function} bit_lsh (@var{int}, @var{nBits})

This function shifts all bits of the signed integer @code{int} to the left by
@code{nBits} bits. The width of the integer is extended by @code{nBits} for
this process. The result of @code{bit_lsh} therefore is @code{int * 2}.

@c ===beg===
@c load("bitwise")$
@c bit_lsh(0,1);
@c bit_lsh(1,0);
@c bit_lsh(1,1);
@c bit_lsh(1,i);
@c bit_lsh(-3,1);
@c bit_lsh(-2,1);
@c ===end===
@example
(%i1) load("bitwise")$
@group
(%i2) bit_lsh(0,1);
(%o2)                           0
@end group
@group
(%i3) bit_lsh(1,0);
(%o3)                           1
@end group
@group
(%i4) bit_lsh(1,1);
(%o4)                           2
@end group
@group
(%i5) bit_lsh(1,i);
(%o5)                     bit_lsh(1, i)
@end group
@group
(%i6) bit_lsh(-3,1);
(%o6)                          - 6
@end group
@group
(%i7) bit_lsh(-2,1);
(%o7)                          - 4
@end group
@end example
@opencatbox
@category{Number theory}
@category{Binary operations}
@category{Package bitwise}
@closecatbox
@end deffn


@anchor{bit_rsh}
@deffn {Function} bit_rsh (@var{int}, @var{nBits})

This function shifts all bits of the signed integer @code{int} to the right by
@code{nBits} bits. The width of the integer is reduced by @code{nBits} for
this process.

@c ===beg===
@c load("bitwise")$
@c bit_rsh(0,1);
@c bit_rsh(2,0);
@c bit_rsh(2,1);
@c bit_rsh(2,2);
@c bit_rsh(-3,1);
@c bit_rsh(-2,1);
@c bit_rsh(-2,2);
@c ===end===
@example
(%i1) load("bitwise")$
@group
(%i2) bit_rsh(0,1);
(%o2)                           0
@end group
@group
(%i3) bit_rsh(2,0);
(%o3)                           2
@end group
@group
(%i4) bit_rsh(2,1);
(%o4)                           1
@end group
@group
(%i5) bit_rsh(2,2);
(%o5)                           0
@end group
@group
(%i6) bit_rsh(-3,1);
(%o6)                          - 2
@end group
@group
(%i7) bit_rsh(-2,1);
(%o7)                          - 1
@end group
@group
(%i8) bit_rsh(-2,2);
(%o8)                          - 1
@end group
@end example
@opencatbox
@category{Number theory}
@category{Binary operations}
@category{Package bitwise}
@closecatbox
@end deffn

@anchor{bit_length}
@deffn {Function} bit_length (@var{int})

determines how many bits a variable needs to be long in order to store the
number @code{int}. This function only operates on positive numbers.

@c ===beg===
@c load("bitwise")$
@c bit_length(0);
@c bit_length(1);
@c bit_length(7);
@c bit_length(8);
@c ===end===
@example
(%i1) load("bitwise")$
@group
(%i2) bit_length(0);
(%o2)                           0
@end group
@group
(%i3) bit_length(1);
(%o3)                           1
@end group
@group
(%i4) bit_length(7);
(%o4)                           3
@end group
@group
(%i5) bit_length(8);
(%o5)                           4
@end group
@end example
@opencatbox
@category{Number theory}
@category{Binary operations}
@category{Package bitwise}
@closecatbox
@end deffn

@anchor{bit_onep}
@deffn {Function} bit_onep (@var{int}, @var{nBit})

determines if bits @code{nBit} is set in the signed integer @code{int}.

@c ===beg===
@c load("bitwise")$
@c bit_onep(85,0);
@c bit_onep(85,1);
@c bit_onep(85,2);
@c bit_onep(85,3);
@c bit_onep(85,100);
@c bit_onep(i,100);
@c ===end===
@example
(%i1) load("bitwise")$
@group
(%i2) bit_onep(85,0);
(%o2)                         true
@end group
@group
(%i3) bit_onep(85,1);
(%o3)                         false
@end group
@group
(%i4) bit_onep(85,2);
(%o4)                         true
@end group
@group
(%i5) bit_onep(85,3);
(%o5)                         false
@end group
@group
(%i6) bit_onep(85,100);
(%o6)                         false
@end group
@group
(%i7) bit_onep(i,100);
(%o7)                   bit_onep(i, 100)
@end group
@end example

For signed numbers the sign bit is interpreted to be more than @code{nBit} to the
left of the leftmost bit of @code{int} that reads @code{1}.
@c ===beg===
@c load("bitwise")$
@c bit_onep(-2,0);
@c bit_onep(-2,1);
@c bit_onep(-2,2);
@c bit_onep(-2,3);
@c bit_onep(-2,4);
@c ===end===
@example
(%i1) load("bitwise")$
@group
(%i2) bit_onep(-2,0);
(%o2)                         false
@end group
@group
(%i3) bit_onep(-2,1);
(%o3)                         true
@end group
@group
(%i4) bit_onep(-2,2);
(%o4)                         true
@end group
@group
(%i5) bit_onep(-2,3);
(%o5)                         true
@end group
@group
(%i6) bit_onep(-2,4);
(%o6)                         true
@end group
@end example


If it is known if the number to be tested is even this information
is taken into consideration by the function.
@c ===beg===
@c load("bitwise")$
@c declare(e,even,o,odd);
@c bit_onep(e,0);
@c bit_onep(o,0);
@c ===end===
@example
(%i1) load("bitwise")$
@group
(%i2) declare(e,even,o,odd);
(%o2)                         done
@end group
@group
(%i3) bit_onep(e,0);
(%o3)                         false
@end group
@group
(%i4) bit_onep(o,0);
(%o4)                         true
@end group
@end example

@opencatbox
@category{Number theory}
@category{Binary operations}
@category{Package bitwise}
@closecatbox
@end deffn
