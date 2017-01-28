# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

set(TCLVERSION  "8.6.6")
set(TKVERSION   "8.6.6")

set(TCL_MD5         "5193aea8107839a79df8ac709552ecb7")
set(TK_MD5          "dd7dbb3a6523c42d05f6ab6e86096e99")

set(TCL_URL       "http://prdownloads.sourceforge.net/tcl/tcl${TCLVERSION}-src.tar.gz")
set(TK_URL        "http://prdownloads.sourceforge.net/tcl/tk${TKVERSION}-src.tar.gz")

if(BUILD_64BIT)
    set(CONFIGURE64BIT "--enable-64bit")
else()
    set(CONFIGURE64BIT "")
endif()


# build tcl/tk (required for xmaxima and the Lisp selection tool)
externalproject_add(tcl
  URL "${TCL_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${TCL_MD5}
  CONFIGURE_COMMAND ${CMAKE_BINARY_DIR}/tcltk/tcl-prefix/src/tcl/win/configure --host=${HOST} ${CONFIGURE64BIT} --prefix=C:/maxima-${MAXIMAVERSION}
  BUILD_COMMAND $(MAKE)
        COMMAND $(MAKE) install
)
install(DIRECTORY ${CMAKE_BINARY_DIR}/tcltk/tcl-prefix/src/tcl-build/C:/maxima-${MAXIMAVERSION}/ DESTINATION .)

externalproject_add(tk
  URL "${TK_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${TK_MD5}
  DEPENDS tcl
  CONFIGURE_COMMAND ${CMAKE_BINARY_DIR}/tcltk/tk-prefix/src/tk/win/configure --host=${HOST} ${CONFIGURE64BIT} --with-tcl=${CMAKE_BINARY_DIR}/tcltk/tcl-prefix/src/tcl-build/C:/maxima-${MAXIMAVERSION}/lib --prefix=C:/maxima-${MAXIMAVERSION}
  BUILD_COMMAND $(MAKE)
        COMMAND $(MAKE) install
)
install(DIRECTORY ${CMAKE_BINARY_DIR}/tcltk/tk-prefix/src/tk-build/C:/maxima-${MAXIMAVERSION}/ DESTINATION .)
