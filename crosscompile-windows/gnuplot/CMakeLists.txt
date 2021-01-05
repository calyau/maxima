# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.


string(REPLACE "." "" GNUPLOTVERSIONWITHOUTDOTS "${GNUPLOTVERSION}")

if(BUILD_64BIT)
    set(GNUPLOTVERSION "5.4.1")
    string(REPLACE "." "" GNUPLOTVERSIONWITHOUTDOTS "${GNUPLOTVERSION}")
    set(GNUPLOT_MD5 "8eab94420b3325b7d78cf8044a5fd3d6")
    set(GNUPLOT_URL "https://sourceforge.net/projects/gnuplot/files/gnuplot/${GNUPLOTVERSION}/gp${GNUPLOTVERSIONWITHOUTDOTS}-win64-mingw.7z")
else()
    set(GNUPLOTVERSION "5.2.7")
    string(REPLACE "." "" GNUPLOTVERSIONWITHOUTDOTS "${GNUPLOTVERSION}")
    set(GNUPLOT_MD5 "228d25b13e4ce7358a99edc7efa574be")
    set(GNUPLOT_URL "https://sourceforge.net/projects/gnuplot/files/gnuplot/${GNUPLOTVERSION}/gp${GNUPLOTVERSIONWITHOUTDOTS}-win32-mingw.7z")
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
