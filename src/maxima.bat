@echo off

set arg0=%0
set arg1=%1
set arg2=%2
set arg3=%3
set arg4=%4
set arg5=%5
set arg6=%6
set arg7=%7
set arg8=%8
set arg9=%9

set lisp=clisp
set version=5.9.0.1cvs
set verbose=false

:startparseargs
if "%1" == "-l" goto foundlisp
if "%1" == "--lisp" goto foundlisp
if "%1" == "-u" goto foundversion
if "%1" == "--use-version" goto foundversion
if "%1" == "-v" goto foundverbose
if "%1" == "--verbose" goto foundverbose

:continueparseargs
shift
if not "%1" == "" goto startparseargs
goto endparseargs

:foundlisp
set lisp=%2
shift
goto continueparseargs

:foundversion
set version=%2
shift
goto continueparseargs

:foundverbose
set verbose=true
goto continueparseargs

:endparseargs

set clisp=d:\msys\1.0\home\amundson\bin\clisp-2.31\bin\clisp
set mib=d:\msys\1.0\home\amundson\maxinstall\lib\maxima\5.9.0.1cvs\binary-clisp\maxima.mem

if "%verbose%" == "true" @echo on
if "%lisp%" == "gcl" goto dogcl
if "%lisp%" == "clisp" goto doclisp

@echo "maxima error: lisp %lisp% not known."
goto end

:dogcl
echo "Gcl? you lose."
goto end

:doclisp
%clisp% -q -M "%mib%" "" -- "%arg1%" "%arg2%" "%arg3%" "%arg4%" "%arg5%" "%arg6%" "%arg7%" "%arg8%" "%arg9%"
goto end

:end



