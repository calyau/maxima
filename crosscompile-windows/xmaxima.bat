@echo off
rem SPDX-License-Identifier: GPL-2.0-or-later
rem startscript for xmaxima

rem get directory, where xmaxima.bat is installed (including final "\")
set xmpath=%~dp0
rem change "\" to "/" in that path. Works too and causes less troubles.
set "xmpath=%xmpath:\=/%"

set "MAXIMA_PREFIX=%xmpath%/.."

start "Xmaxima" /b "%xmpath%wish86s.exe" "%xmpath%xmaxima" %*

