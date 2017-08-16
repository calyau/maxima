# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

set(GNUPLOTVERSION     5.0.7)

string(REPLACE "." "" GNUPLOTVERSIONWITHOUTDOTS "${GNUPLOTVERSION}")

if(BUILD_64BIT)
    set(GNUPLOT_MD5   "2c6975f1d81a589c353904cb59722ed0")
    set(GNUPLOT_URL   "http://sourceforge.net/projects/gnuplot/files/gnuplot/${GNUPLOTVERSION}/gp${GNUPLOTVERSIONWITHOUTDOTS}-win64-mingw.zip")
else()
    set(GNUPLOT_MD5   "d97fd066923890dd59172e93d24d2534")
    set(GNUPLOT_URL   "http://sourceforge.net/projects/gnuplot/files/gnuplot/${GNUPLOTVERSION}/gp${GNUPLOTVERSIONWITHOUTDOTS}-win32-mingw.zip")
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
install(DIRECTORY ${CMAKE_BINARY_DIR}/gnuplot/gnuplot-prefix/src/gnuplot/ DESTINATION gnuplot)
