@menu
* Introduction to alt-display::
* Functions and Variables for alt-display::
@end menu

@node Introduction to alt-display, Functions and Variables for alt-display, alt-display-pkg, alt-display-pkg
@section Introduction to alt-display

The @emph{alt-display} package provides a means to change the way that
Maxima displays its output. The @var{*alt-display1d*} and
@var{*alt-display2d*} Lisp hooks were introduced to Maxima in 2002, but
were not easily accessible from the Maxima REPL until the introduction
of this package.

The package provides a general purpose function to define alternative
display functions, and a separate function to set the display
function. The package also provides customized display functions to
produce output in @TeX{}, Texinfo, XML and all three output formats
within Texinfo.

Here is a sample session:

@c (linenum:1,display2d:true,reset_displays());
@example
(%i1) load("alt-display.mac")$
(%i2) set_alt_display(2,tex_display)$

(%i3) x/(x^2+y^2) = 1;
\mbox@{\tt\red(@{\it \%o_3@}) \black@}$$@{@{x@}\over@{y^2+x^2@}@}=1$$

(%i4) set_alt_display(2,mathml_display)$

(%i5) x/(x^2+y^2) = 1;
<math xmlns="http://www.w3.org/1998/Math/MathML"> <mi>mlabel</mi> 
<mfenced separators=""><msub><mi>%o</mi> <mn>5</mn></msub> 
<mo>,</mo><mfrac><mrow><mi>x</mi> </mrow> <mrow><msup><mrow>
<mi>y</mi> </mrow> <mn>2</mn> </msup> <mo>+</mo> <msup><mrow>
<mi>x</mi> </mrow> <mn>2</mn> </msup> </mrow></mfrac> <mo>=</mo> 
<mn>1</mn> </mfenced> </math>

(%i6) set_alt_display(2,multi_display_for_texinfo)$

(%i7) x/(x^2+y^2) = 1;

@@iftex
@@tex
\mbox@{\tt\red(@{\it \%o_7@}) \black@}$$@{@{x@}\over@{y^2+x^2@}@}=1$$
@@end tex
@@end iftex
@@ifhtml
@@html

<math xmlns="http://www.w3.org/1998/Math/MathML"> <mi>mlabel</mi> 
<mfenced separators=""><msub><mi>%o</mi> <mn>7</mn></msub> 
<mo>,</mo><mfrac><mrow><mi>x</mi> </mrow> <mrow><msup><mrow>
<mi>y</mi> </mrow> <mn>2</mn> </msup> <mo>+</mo> <msup><mrow>
<mi>x</mi> </mrow> <mn>2</mn> </msup> </mrow></mfrac> <mo>=</mo> 
<mn>1</mn> </mfenced> </math>
@@end html
@@end ifhtml
@@ifinfo
@@example
(%o7) x/(y^2+x^2) = 1
@@end example
@@end ifinfo
@end example

If the alternative display function causes an error, the error is
trapped and the display function is reset to the default display. In the
following example, the @code{error} function is set to display the
output. This throws an error, which is handled by resetting the
2d-display to the default.

@example
(%i8) set_alt_display(2,?error)$

(%i9) x;

Error in *alt-display2d*.
Messge: Condition designator ((MLABEL) $%O9 $X) is not of type (OR SYMBOL STRING
                                                             FUNCTION).
*alt-display2d* reset to nil.
 -- an error. To debug this try: debugmode(true);

(%i10) x;
(%o10)                                 x
@end example

@node Functions and Variables for alt-display,  , Introduction to alt-display, alt-display-pkg
@section Functions and Variables for alt-display

@deffn {Function} define_alt_display (@var{function}(@var{input}), @var{expr})
This function is similar to @code{define}: it evaluates its arguments
and expands into a function definition. The @var{function} is a
function of a single input @var{input}. For convenience, a substitution
is applied to @var{expr} after evaluation, to provide easy access to
Lisp variable names.

Set a time-stamp on each prompt:
@example
(%i1) load("alt-display.mac")$

(%i2) display2d:false$

(%i3) define_alt_display(time_stamp(x),
                   block([alt_display1d:false,alt_display2d:false],
                         prompt_prefix:printf(false,"~a~%",timedate()),
                         displa(x)));

(%o3) time_stamp(x):=block([\*alt\-display1d\*:false,
                            \*alt\-display2d\*:false],
                 \*prompt\-prefix\*:printf(false,"~a~%",timedate()),displa(x))
(%i4) set_alt_display(1,time_stamp);

(%o4) done
2017-11-27 16:15:58-06:00
(%i5) 
@end example

The input line @code{%i3} defines @code{time_stamp} using
@code{define_alt_display}. The output line @code{%o3} shows that the
Maxima variable names @code{alt_display1d}, @code{alt_display2d} and
@code{prompt_prefix} have been replaced by their Lisp translations, as
has @code{displa} been replaced by @code{?displa} (the display
function).

The display variables @code{alt_display1d} and @code{alt_display2d} are
both bound to @code{false} in the body of @code{time_stamp} to prevent
an infinite recursion in @code{displa}.

@opencatbox
@category{Package alt-display}
@closecatbox

@end deffn

@anchor{info_display}
@deffn {Function} info_display (@var{form})
This is an alias for the default 1-d display function. It may be used as
an alternative 1-d or 2-d display function.

@example
(%i1) load("alt-display.mac")$

(%i2) set_alt_display(2,info_display);

(%o2) done
(%i3) x/y;

(%o3) x/y
@end example

@opencatbox
@category{Package alt-display}
@closecatbox

@end deffn

@anchor{mathml_display}
@deffn {Function} mathml_display (@var{form})
Produces MathML output.

@example
(%i1) load("alt-display.mac")$

(%i2) set_alt_display(2,mathml_display);
<math xmlns="http://www.w3.org/1998/Math/MathML"> <mi>mlabel</mi> 
 <mfenced separators=""><msub><mi>%o</mi> <mn>2</mn></msub> 
 <mo>,</mo><mi>done</mi> </mfenced> </math>
@end example

@opencatbox
@category{Package alt-display}
@closecatbox

@end deffn

@anchor{tex_display}
@deffn {Function} tex_display (@var{form})
Produces TeX output.

@example
(%i2) set_alt_display(2,tex_display);
\mbox@{\tt\red(@{\it \%o_2@}) \black@}$$\mathbf@{done@}$$
(%i3) x/(x^2+y^2);
\mbox@{\tt\red(@{\it \%o_3@}) \black@}$$@{@{x@}\over@{y^2+x^2@}@}$$
@end example

@opencatbox
@category{Package alt-display}
@closecatbox

@end deffn

@deffn {Function} multi_display_for_texinfo (@var{form})
Produces Texinfo output using all three display functions.

@example
(%i2) set_alt_display(2,multi_display_for_texinfo)$

(%i3) x/(x^2+y^2);

@@iftex
@@tex
\mbox@{\tt\red(@{\it \%o_3@}) \black@}$$@{@{x@}\over@{y^2+x^2@}@}$$
@@end tex
@@end iftex
@@ifhtml
@@html

   <math xmlns="http://www.w3.org/1998/Math/MathML"> <mi>mlabel</mi> 
   <mfenced separators=""><msub><mi>%o</mi> <mn>3</mn></msub> 
   <mo>,</mo><mfrac><mrow><mi>x</mi> </mrow> <mrow><msup><mrow>
   <mi>y</mi> </mrow> <mn>2</mn> </msup> <mo>+</mo> <msup><mrow>
   <mi>x</mi> </mrow> <mn>2</mn> </msup> </mrow></mfrac> </mfenced> </math>
@@end html
@@end ifhtml
@@ifinfo
@@example
(%o3) x/(y^2+x^2)
@@end example
@@end ifinfo
@end example

@opencatbox
@category{Package alt-display}
@closecatbox

@end deffn

@deffn {Functions} reset_displays ()
Resets the prompt prefix and suffix to the empty string, and sets both
1-d and 2-d display functions to the default.

@opencatbox
@category{Package alt-display}
@closecatbox

@end deffn

@anchor{set_alt_display}
@deffn {Function} set_alt_display (@var{num}, @var{display-function})
The input @var{num} is the display to set; it may be either 1 or 2. The
second input @var{display-function} is the display function to use. The
display function may be either a Maxima function or a @code{lambda}
expression.

Here is an example where the display function is a @code{lambda}
expression; it just displays the result as @TeX{}.
@example
(%i1) load("alt-display.mac")$

(%i2) set_alt_display(2, lambda([form], tex(?caddr(form))))$

(%i3) integrate(exp(-t^2),t,0,inf);
$$@{@{\sqrt@{\pi@}@}\over@{2@}@}$$
@end example

A user-defined display function should take care that it @emph{prints}
its output. A display function that returns a string will appear to
display nothing, nor cause any errors.

@opencatbox
@category{Package alt-display}
@closecatbox

@end deffn

@anchor{set_prompt}
@deffn {Function} set_prompt (@var{fix}, @var{expr})
Set the prompt prefix or suffix to @var{expr}. The input @var{fix} must
evaluate to one of @code{prefix}, @code{suffix}, @code{general},
@code{prolog} or @code{epilog}. The input @var{expr} must evaluate to
either a string or @code{false}; if @code{false}, the @var{fix} is reset
to the default value.

@example
(%i1) load("alt-display.mac")$
(%i2) set_prompt('prefix,printf(false,"It is now: ~a~%",timedate()))$

It is now: 2014-01-07 15:23:23-05:00
(%i3) 
@end example

The following example shows the effect of each option, except
@code{prolog}. Note that the @code{epilog} prompt is printed as Maxima
closes down. The @code{general} is printed between the end of input and
the output, unless the input line ends in @code{$}.

Here is an example to show where the prompt strings are placed.

@example
(%i1) load("alt-display.mac")$

(%i2) set_prompt(prefix,"<<prefix>> ",suffix,"<<suffix>> ",general,
           printf(false,"<<general>>~%"),epilog,printf(false,"<<epilog>>~%"));

(%o2)                                done
<<prefix>> (%i3) <<suffix>> x/y;
<<general>>
                                       x
(%o3)                                  -
                                       y
<<prefix>> (%i4) <<suffix>> quit();
<<general>>
<<epilog>>
@end example

Here is an example that shows how to colorize the input and output when
Maxima is running in a terminal or terminal emulator like
Emacs@footnote{Readers using the @code{info} reader in @code{Emacs} will
see the actual prompt strings; other readers will see the colorized
output}.

@ifinfo
@c  (%i2) set_prompt('prefix,"[1;31m",'suffix,"[0;32m",'general,"[1;34m",'epilog,"[00;m")$
@example
(%i1) load("alt-display.mac")$

(%i2) cs(s) := printf(false,"~c[~am",ascii(27),s)$

(%i3) set_prompt(prefix,cs("1;31"),suffix,cs("0;32"),general,cs("1;34"),epilog,cs("00;"));
(%o3)                                done
[1;31m(%i4)[0;32m integrate(exp(-x^2),x,0,inf);[1;34m
                                   sqrt(%pi)
(%o4)                              ---------
                                       2
[1;31m(%i5)[00;m
@end example
@end ifinfo
@ifnotinfo
@figure{color_terminal}
@end ifnotinfo

Each prompt string starts with the ASCII escape character (27) followed
by an open square bracket (91); each string ends with a lower-case m
(109). The webpages
@url{http://misc.flogisoft.com/bash/tip_colors_and_formatting} and
@url{http://www.tldp.org/HOWTO/Bash-Prompt-HOWTO/x329.html} provide
information on how to use control strings to set the terminal colors.

@opencatbox
@category{Package alt-display}
@closecatbox

@end deffn

@c Local Variables: 
@c mode: texinfo
@c TeX-master: "include-maxima"
@c End: 
