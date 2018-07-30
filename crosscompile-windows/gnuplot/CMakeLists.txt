# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

set(GNUPLOTVERSION     5.2.4)

string(REPLACE "." "" GNUPLOTVERSIONWITHOUTDOTS "${GNUPLOTVERSION}")

if(BUILD_64BIT)
    set(GNUPLOT_MD5   "80bf6103769f6058ead28bbf366ac37e")
    set(GNUPLOT_URL   "http://sourceforge.net/projects/gnuplot/files/gnuplot/${GNUPLOTVERSION}/gp${GNUPLOTVERSIONWITHOUTDOTS}-win64-mingw_2.7z")
else()
    set(GNUPLOT_MD5   "4d48bb873c5366029caafc4f80539d69")
    set(GNUPLOT_URL   "http://sourceforge.net/projects/gnuplot/files/gnuplot/${GNUPLOTVERSION}/gp${GNUPLOTVERSIONWITHOUTDOTS}-win32-mingw_2.7z")
endif()

# Gnuplot (already a binary package just extract it...)
externalproject_add(gnuplot
  URL "${GNUPLOT_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${GNUPLOT_MD5}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)
install(DIRECTORY ${CMAKE_BINARY_DIR}/gnuplot/gnuplot-prefix/src/gnuplot/ DESTINATION gnuplot COMPONENT Gnuplot)
