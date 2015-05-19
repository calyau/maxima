@echo off
rem startscript for xmaxima

rem get directory, where xmaxima.bat is installed (including final "\")
set xmpath=%~dp0

"%xmpath%wish86.exe" "%xmpath%\xmaxima" %*

