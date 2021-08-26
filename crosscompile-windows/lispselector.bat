@echo off
rem SPDX-License-Identifier: GPL-2.0-or-later
rem startscript for Lisp configuration tool

rem the get directory, where the this file is installed (including final "\")
set xmpath=%~dp0

start "" "%xmpath%wish86s.exe" "%xmpath%lispselector.tcl" %*

