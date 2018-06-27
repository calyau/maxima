# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

if(BUILD_64BIT)
    set(SBCLVERSION "1.4.2")
    set(SBCL_MD5           "b581c4461be2a9839f4305f500ab16e6")
    set(SBCL_INSTALLERNAME "sbcl-${SBCLVERSION}-x86-64-windows-binary.msi")
else()
    set(SBCLVERSION "1.4.2")
    set(SBCL_MD5           "43db66f7cd9ee2d0ae791939c0769f1b")
    set(SBCL_INSTALLERNAME "sbcl-${SBCLVERSION}-x86-windows-binary.msi")
endif()

set(SBCL_URL      "http://prdownloads.sourceforge.net/sbcl/${SBCL_INSTALLERNAME}")

externalproject_add(sbcl
  URL "${SBCL_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${SBCL_MD5}
  DOWNLOAD_NO_EXTRACT 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND cd ${CMAKE_BINARY_DIR}/sbcl && ${CMAKE_SOURCE_DIR}/sbcl.sh
  INSTALL_COMMAND ""
)

ExternalProject_Add_Step(sbcl extract
  COMMENT "Performing extraction step for 'SBCL'"
  COMMAND mkdir -p ${CMAKE_BINARY_DIR}/sbcl && cd ${CMAKE_BINARY_DIR}/sbcl && ${SEVENZIP_EXE} x -y ${CMAKE_SOURCE_DIR}/downloads/${SBCL_INSTALLERNAME}
  DEPENDEES download
  DEPENDERS patch
)

install(FILES ${CMAKE_BINARY_DIR}/sbcl/sbcl.exe ${CMAKE_BINARY_DIR}/sbcl/sbcl.core DESTINATION bin COMPONENT SBCL)
install(DIRECTORY ${CMAKE_BINARY_DIR}/sbcl/contrib DESTINATION bin COMPONENT SBCL)

