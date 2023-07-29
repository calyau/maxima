# raddenest
A Maxima package for denesting radical expressions

Most of the code in this Maxima package is a rather direct port of Sympy's
denesting code:

http://docs.sympy.org/1.0/_modules/sympy/simplify/sqrtdenest.html

The code is licensed under the GPL-compatible "3-clause BSD license".

More information can be found in the comments in `raddenest.mac`.

## Setup
Copy the three files to `~/.maxima` (or alternatively some other directory that is searched by Maxima. See `file_search_maxima` for more info.)

The package can then be loaded using `load(raddenest)`.
To run the testsuite or try the provided demos you may have to point `file_search_demo` and `file_search_tests` to `~/.maxima`:

```
file_search_demo: append(["~/.maxima/$$$.dem"],file_search_demo);
demo(raddenest);
file_search_tests: append(["~/.maxima/$$$.mac"],file_search_tests);
testsuite_files:["rtest_raddenest.mac"];
run_testsuite(display_all=false, tests=["rtest_raddenest.mac"]);
```
