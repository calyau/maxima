@menu
* Introduction to operatingsystem::
* Directory operations::
* File operations::
* Environment operations::
@end menu

@node Introduction to operatingsystem, Directory operations, operatingsystem-pkg, operatingsystem-pkg
@section Introduction to operatingsystem

Package @code{operatingsystem} contains functions for operatingsystem-tasks, like file system operations.


@node Directory operations, File operations, Introduction to operatingsystem, operatingsystem-pkg
@section Directory operations


@anchor{chdir}
@deffn {Function} chdir (@var{dir})
Change to directory @var{dir}
@end deffn

@anchor{mkdir}
@deffn {Function} mkdir (@var{dir})
Create directory @var{dir}
@end deffn

@anchor{rmdir}
@deffn {Function} rmdir (@var{dir})
remove directory @var{dir}
@end deffn

@anchor{getcurrentdirectory}
@deffn {Function} getcurrentdirectory ()
returns the current working directory.

See also @mrefdot{directory}

@end deffn

Examples:

@example
(%i1) load("operatingsystem")$
(%i2) mkdir("testdirectory")$
(%i3) chdir("testdirectory")$
(%i4) chdir("..")$
(%i5) rmdir("testdirectory")$
@end example

@node File operations, Environment operations, Directory operations, operatingsystem-pkg
@section File operations


@anchor{copy_file}
@deffn {Function} copy_file (@var{file1}, @var{file2})
copies file @var{file1} to @var{file2}
@end deffn

@anchor{rename_file}
@deffn {Function} rename_file (@var{file1}, @var{file2})
renames file @var{file1} to @var{file2}
@end deffn

@anchor{delete_file}
@deffn {Function} delete_file (@var{file1})
deletes file @var{file1}
@end deffn


@node Environment operations, , File operations, operatingsystem-pkg
@section Environment operations


@anchor{getenv}
@deffn {Function} getenv (@var{env})
Get the value of the environmentvariable @var{env}

Example:

@example
(%i1) load("operatingsystem")$
(%i2) getenv("PATH");
(%o2) /usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
@end example

@end deffn
