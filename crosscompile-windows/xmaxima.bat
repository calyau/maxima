@echo off
rem startscript for xmaxima

rem get directory, where xmaxima.bat is installed (including final "\")
set xmpath=%~dp0


set MAXIMA_PREFIX=%xmpath%/..

start "Xmaxima" /b "%xmpath%wish86.exe" "%xmpath%\xmaxima" %*

