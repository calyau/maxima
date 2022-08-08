m4_dnl Maxima has an # operator, so disable comment delimiters completely so 
m4_dnl we don't accidentally comment out text
m4_changecom()m4_dnl
m4_dnl Change the quote characters to something that isn't likely to
m4_dnl show up in the manual.
m4_changequote(`<<<', `>>>')m4_dnl
m4_dnl
m4_dnl For writing formulas suitable for various output formats.  For
m4_dnl simplicity two or three arguments are required:
m4_dnl
m4_dnl 1:  HTML output with MathJAX enabled
m4_dnl 2:  HTML output without MathJax. Also used for info
m4_dnl 3:  If given, this is for TeX output.  If not, then use arg 1.
m4_dnl
m4_dnl For easiest use, it's best to quote each arg, so use
m4_dnl m4_displaymath(<<<arg1>>>, <<<arg2>>>, <<<arg3>>>)
m4_dnl
m4_dnl m4_displaymath is used for displayed math equations.
m4_dnl
m4_dnl Be careful not to leave extra blank lines; TeX may complain about the
m4_dnl blank lines.
m4_define(<<<m4_displaymath>>>, 
<<<
@ifhtml
@displaymath
$1
@end displaymath
@end ifhtml
@ifinfo
$2
@end ifinfo
@tex
$$m4_ifelse(<<<$#>>>, <<<3>>>, <<<<<<$3>>>>>>, <<<$1>>>)$$
@end tex
>>>)m4_dnl
m4_dnl
m4_dnl m4_math(jax, info) or m4_math(jax, info, tex)
m4_dnl Like m4_displaymath, but this is meant for inline math equations.
m4_define(<<<m4_math>>>, 
<<<
@ifhtml
@math{$1}
@end ifhtml
@ifinfo
@math{$2}
@end ifinfo
@tex
$m4_ifelse(<<<$#>>>, <<<3>>>, <<<<<<$3>>>>>>, <<<$1>>>)$
@end tex
>>>)m4_dnl
m4_dnl
m4_dnl m4_mathdot(jax, info) or m4_mathdot(jax, info, tex)
m4_dnl
m4_dnl Like m4_math, but automatically add a dot at the end of the
m4_dnl equation to end a sentence.  If you place a dot after m4_math(), there's
m4_dnl an extra space in the output that looks weird.  This is analogous to mrefdot.
m4_define(<<<m4_mathdot>>>,
<<<m4_ifelse(<<<$#>>>, <<<3>>>,
m4_math(<<<<<<$1.>>>>>>, <<<<<<$2.>>>>>>, <<<<<<$3.>>>>>>),
m4_math(<<<<<<$1.>>>>>>, <<<<<<$2.>>>>>>))>>>)m4_dnl
m4_dnl
m4_dnl Like m4_mathdot, but adds a comma at the end instead of a dot.
m4_dnl Using m4_math() followed by a comma leaves an extra space that looks
m4_dnl weird.  This is analogous to mrefcomma.
m4_define(<<<m4_mathcomma>>>,
<<<m4_ifelse(<<<$#>>>, <<<3>>>,
m4_math(<<<<<<$1,>>>>>>, <<<<<<$2,>>>>>>, <<<<<<$3,>>>>>>),
m4_math(<<<<<<$1,>>>>>>, <<<<<<$2,>>>>>>))>>>)m4_dnl
