# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

set(TCLVERSION  "8.6.9")
set(TKVERSION   "8.6.9.1")

set(TCL_MD5     "aa0a121d95a0e7b73a036f26028538d4")
set(TK_MD5      "9efe3976468352dc894dae0c4e785a8e")

set(TCL_URL     "http://prdownloads.sourceforge.net/tcl/tcl${TCLVERSION}-src.tar.gz")
set(TK_URL      "http://prdownloads.sourceforge.net/tcl/tk${TKVERSION}-src.tar.gz")

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
  CONFIGURE_COMMAND ${CMAKE_BINARY_DIR}/tcltk/tcl-prefix/src/tcl/win/configure --build=${BUILDHOST} --host=${HOST} ${CONFIGURE64BIT} --prefix=${WINDOWS_DRIVELETTER}:/maxima-${MAXIMAVERSION}
  BUILD_COMMAND $(MAKE)
        COMMAND $(MAKE) install
)
install(DIRECTORY ${CMAKE_BINARY_DIR}/tcltk/tcl-prefix/src/tcl-build/${WINDOWS_DRIVELETTER}:/maxima-${MAXIMAVERSION}/ DESTINATION . COMPONENT TclTk)

externalproject_add(tk
  URL "${TK_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${TK_MD5}
  DEPENDS tcl
  CONFIGURE_COMMAND ${CMAKE_BINARY_DIR}/tcltk/tk-prefix/src/tk/win/configure --build=${BUILDHOST} --host=${HOST} ${CONFIGURE64BIT} --with-tcl=${CMAKE_BINARY_DIR}/tcltk/tcl-prefix/src/tcl-build/${WINDOWS_DRIVELETTER}:/maxima-${MAXIMAVERSION}/lib --prefix=${WINDOWS_DRIVELETTER}:/maxima-${MAXIMAVERSION}
  BUILD_COMMAND $(MAKE)
        COMMAND $(MAKE) install
)
install(DIRECTORY ${CMAKE_BINARY_DIR}/tcltk/tk-prefix/src/tk-build/${WINDOWS_DRIVELETTER}:/maxima-${MAXIMAVERSION}/ DESTINATION . COMPONENT TclTk)
