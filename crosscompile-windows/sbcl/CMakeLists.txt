# CMake file for crosscompiling Maxima/wxMaxima for Windows
# SPDX-License-Identifier: GPL-2.0-or-later
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

if(BUILD_64BIT)
    set(SBCLVERSION "2.3.2")
    set(SBCL_MD5 "bb3e6e16b1c793813b348464a56239f3")
    set(SBCL_INSTALLERNAME "sbcl-${SBCLVERSION}-x86-64-windows-binary.msi")
else()
    set(SBCLVERSION "2.3.2")
    set(SBCL_MD5 "d909b01d9f520df639dbd49709735ee6")
    set(SBCL_INSTALLERNAME "sbcl-${SBCLVERSION}-x86-windows-binary.msi")
endif()

set(SBCL_URL "https://prdownloads.sourceforge.net/sbcl/${SBCL_INSTALLERNAME}")

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

