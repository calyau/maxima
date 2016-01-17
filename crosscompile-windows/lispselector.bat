@echo off
rem startscript for Lisp configuration tool

rem the get directory, where the this file is installed (including final "\")
set xmpath=%~dp0

"%xmpath%wish86.exe" "%xmpath%lispselector.tcl" %*

