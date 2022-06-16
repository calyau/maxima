m4_dnl Maxima has an # operator, so disable comment delimiters completely so 
m4_dnl we don't accidentally comment out text
m4_changecom()
m4_dnl Change the quote characters to something that isn't likely to
m4_dnl show up in the manual.
m4_changequote(`<<<', `>>>')
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
<<<@ifhtml
@html
@displaymath
$1
@end displaymath
@end html
@end ifhtml
@ifinfo
$2
@end ifinfo
@tex
$$m4_ifelse(<<<$#>>>, <<<3>>>, <<<<<<$3>>>>>>, <<<$1>>>)$$
@end tex
>>>)

m4_dnl Like m4_displaymath, but this is meant for inline math equations.
m4_define(<<<m4_math>>>, 
<<<@ifhtml
@html
@math{$1}
@end html
@end ifhtml
@ifinfo
$2
@end ifinfo
@tex
$m4_ifelse(<<<$#>>>, <<<3>>>, <<<<<<$3>>>>>>, <<<$1>>>)$
@end tex
>>>)
