rem startscript for wxmaxima
rem workaround (dont know how to solve it better) for file associations
rem get path to wxmaxima-assoc.bat directory
set wxpath=%~dp0

start /b "%wxpath%\wxmaxima.exe" /o %*
