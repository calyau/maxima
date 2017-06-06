@echo off
rem startscript for xmaxima

rem get directory, where xmaxima.bat is installed (including final "\")
set xmpath=%~dp0
rem change "\" to "/" in that path. Works too and causes less troubles.
set "xmpath=%xmpath:\=/%"

set "MAXIMA_PREFIX=%xmpath%/.."

start "Xmaxima" /b "%xmpath%wish86.exe" "%xmpath%xmaxima" %*

