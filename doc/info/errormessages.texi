@menu
* Error messages::
* Warning messages::
@end menu

This chapter provides detailed information about the meaning of some error messages
or on how to recover from errors.
@node Error messages, Warning messages,,Error and warning messages
@section Error messages
@menu
* No such list element::
* argument must be a non-atomic expression::
* cannot assign to function::
* 0 to a negative exponent::
* Comma is not a prefix operator::
* Illegal use of delimiter::
* loadfile failed to load::
* makelist second argument must evaluate to a number::
* Only symbols can be bound::
* Operators of arguments must all be the same::
* out of memory::
* part fell off the end::
* undefined variable during plotting::
* VTK is not installed::
@end menu


@node No such list element, argument must be a non-atomic expression, Error messages, Error messages
@subsection apply: no such "list" element
One common cause for this error message is that square brackets operator
(@code{[ ]}) was used trying to access a list element that whose element
number was @code{< 1} or @code{> length(list)}.
@opencatbox
@category{Error messages}
@closecatbox


@node argument must be a non-atomic expression, cannot assign to function, No such list element, Error messages
@subsection argument must be a non-atomic expression
This normally means that a list, a set or something else that consists of more than one
element was expected. One possible cause for this error message is a construct of the 
following type:
@c ===beg===
@c l:[1,2,3];
@c append(l,4);
@c ===end===
@example
@group
(%i1) l:[1,2,3];
(%o1)                       [1, 2, 3]
@end group
@group
(%i2) append(l,4);
append: argument must be a non-atomic expression; found 4
 -- an error. To debug this try: debugmode(true);
@end group
@end example
The correct way to append variables or numbers to a list is to wrap them in a
single-element list first:
@c ===beg===
@c l:[1,2,3];
@c append(l,[4]);
@c ===end===
@example
@group
(%i1) l:[1,2,3];
(%o1)                       [1, 2, 3]
@end group
@group
(%i2) append(l,[4]);
(%o2)                     [1, 2, 3, 4]
@end group
@end example

@opencatbox
@category{Error messages}
@closecatbox


@node cannot assign to function, 0 to a negative exponent, argument must be a non-atomic expression, Error messages
@subsection assignment: cannot assign to <function name>
Maxima supports several assignment operators. When trying to define a function
@code{:=} has to be used.
@opencatbox
@category{Error messages}
@closecatbox


@node 0 to a negative exponent, Comma is not a prefix operator, cannot assign to function, Error messages
@subsection expt: undefined:  0 to a negative exponent.
This message notifies about a classical division by zero error.
@opencatbox
@category{Error messages}
@closecatbox


@node Comma is not a prefix operator, Illegal use of delimiter, 0 to a negative exponent, Error messages
@subsection incorrect syntax: , is not a prefix operator
This might be caused by a command starting with a comma (@code{,}) or by one comma
being directly followed by another one..
@opencatbox
@category{Error messages}
@closecatbox


@node Illegal use of delimiter, loadfile failed to load, Comma is not a prefix operator, Error messages
@subsection incorrect syntax: Illegal use of delimiter )
Common reasons for this error appearing are a closing parenthesis without an
opening one or a closing parenthesis directly preceded by a comma.
@opencatbox
@category{Error messages}
@closecatbox


@node loadfile failed to load, makelist second argument must evaluate to a number, Illegal use of delimiter, Error messages
@subsection loadfile: failed to load <filename>
This error message normally indicates that the file exists, but can not be read.
If the file is present and readable there is another possible for this error
message: Maxima can compile packages to native binary files in order to make them
run faster. If after compiling the file something in the system has changed in a
way that makes it incompatible with the binary the binary the file cannot be
loaded any more. Maxima normally puts binary files it creates from its own packages
in a folder named @code{binary} within the folder whose name it is printed after
typing:
@c ===beg===
@c maxima_userdir;
@c ===end===
@example
@group
(%i1) maxima_userdir;
(%o1)                 /home/gunter/.maxima
@end group
@end example
If this directory is missing maxima will recreate it again as soon as it has to
compile a package.
@opencatbox
@category{Error messages}
@closecatbox


@node makelist second argument must evaluate to a number, Only symbols can be bound, loadfile failed to load, Error messages
@subsection makelist: second argument must evaluate to a number
@code{makelist} expects the second argument to be the name of the variable whose value is to
be stepped. This time instead of the name of a still-undefined variable maxima has found
something else, possibly a list or the name of a list.

@opencatbox
@category{Error messages}
@closecatbox


@node Only symbols can be bound, Operators of arguments must all be the same, makelist second argument must evaluate to a number, Error messages
@subsection Only symbols can be bound
The most probable cause for this error is that there was an attempt to either
use a number or a variable whose numerical value is known as a loop
counter.
@opencatbox
@category{Error messages}
@closecatbox


@node Operators of arguments must all be the same, out of memory, Only symbols can be bound, Error messages
@subsection operators of arguments must all be the same
One possible reason for this error message to appear is a try to use @mref{append} in order
to add an equation to a list:
@c ===beg===
@c l:[a=1,b=2,c=3];
@c append(l,d=5);
@c ===end===
@example
@group
(%i1) l:[a=1,b=2,c=3];
(%o1)                 [a = 1, b = 2, c = 3]
@end group
@group
(%i2) append(l,d=5);
append: operators of arguments must all be the same.
 -- an error. To debug this try: debugmode(true);
@end group
@end example
In order to add an equation to a list it has to be wrapped in a
single-element list first:
@c ===beg===
@c l:[a=1,b=2,c=3];
@c append(l,[d=5]);
@c ===end===
@example
@group
(%i1) l:[a=1,b=2,c=3];
(%o1)                 [a = 1, b = 2, c = 3]
@end group
@group
(%i2) append(l,[d=5]);
(%o2)             [a = 1, b = 2, c = 3, d = 5]
@end group
@end example


@opencatbox
@category{Error messages}
@closecatbox


@node out of memory, part fell off the end, Operators of arguments must all be the same, Error messages
@subsection Out of memory
Lisp typically handles several types of memory containing at least one stack
and a heap that contains user objects. To avoid running out of memory several
approaches might be useful:
@itemize @bullet
@item If possible, the best solution normally is to use an algorithm that is
      more memory-efficient.
@item Compiling a function might drastically reduce the amount of memory it
      needs.
@item Arrays of a fixed type might be more memory-efficient than lists.
@item If maxima is run by sbcl sbcl's memory limit might be set to a value
      that is too low to solve the current problem. In this case the
      command-line option @code{--dynamic-space-size <n>} allows to tell
      sbcl to reserve @code{n} megabytes for the heap. It is to note, though,
      that sbcl has to handle several distinct types of memory and therefore
      might be able to only reserve about half of the available physical
      memory. Also note that 32-bit processes might only be able to access
      2GB of physical memory.
@end itemize
@opencatbox
@category{Error messages}
@closecatbox


@node part fell off the end, undefined variable during plotting, out of memory, Error messages
@subsection part: fell off the end
@code{part()} was used to access the @code{n}th item in something that has less than
@code{n} items.
@opencatbox
@category{Error messages}
@closecatbox


@node undefined variable during plotting, VTK is not installed, part fell off the end, Error messages
@subsection undefined variable (draw or plot)
A function could not be plotted since it still contained a variable maxima
doesn't know the value of.

In order to find out which variable this could be it is sometimes helpful to
temporarily replace the name of the drawing command (@code{draw2d}, @code{plot2d}
or similar) by a random name (for example @code{ddraw2d}) that doesn't coincide
with the name of an existing function to make maxima print out what parameters
the drawing command sees.

@example
(%i1) load("draw")$
(%i2) f(x):=sin(omega*t);
(%o2) f(x) := sin(omega t)
(%i3) draw2d(
        explicit(
          f(x),
          x,1,10
        )
      );
draw2d (explicit): non defined variable
 -- an error. To debug this try: debugmode(true);
(%i4) ddraw2d(
        explicit(
          f(x),
          x,1,10
        )
      );
(%o4) ddraw2d(explicit(sin(omega t), x, 1, 10))
@end example

@opencatbox
@category{Error messages}
@closecatbox


@node VTK is not installed,  , undefined variable during plotting, Error messages
@subsection VTK is not installed, which is required for Scene

This might either mean that VTK is actually not installed - or cannot be found by
maxima - or that maxima has no write access to the directory whose name is output
if the following maxima command is entered:
@c ===beg===
@c maxima_tempdir;
@c ===end===
@example
@group
(%i1) maxima_tempdir;
(%o1)                         /tmp
@end group
@end example

@opencatbox
@category{Error messages}
@closecatbox


@node Warning messages, ,Error messages,Error and warning messages
@section Warning messages
@menu
* undefined variable during translation::
* replaced x by y::
@end menu

@node undefined variable during translation, replaced x by y, Warning messages, Warning messages
@subsection Encountered undefined variable <x> in translation
A function was compiled but the type of the variable @code{x} was not known.
This means that the compiled command contains additional code that makes it
retain all the flexibility maxima provides in respect to this variable.
If @code{x} isn't meant as a variable name but just a named option to a
command prepending the named option by a single quote (@code{'}) should
resolve this issue.
@opencatbox
@category{Error messages}
@closecatbox


@node replaced x by y,  , undefined variable during translation, Warning messages
@subsection Rat: replaced <x> by <y> = <z>
Floating-point numbers provide a maximum number of digits that is typically high,
but still limited. Good examples that this limitation might be too low
even for harmless-looking examples include 
@uref{https://en.wikipedia.org/wiki/Wilkinson%27s_polynomial, Wilkinson's Polynomial},
The Rump polynomial and the fact that an exact 1/10 cannot be expressed as a binary
floating-point number. 
In places where the floating-point error might add up or hinder terms from
cancelling each other out maxima therefore by default replaces them with exact
fractions.
See also @code{ratprint}, @code{ratepsilon}, @code{bftorat}, @code{fpprintprec}
and @code{rationalize}.
@opencatbox
@category{Warning messages}
@closecatbox
