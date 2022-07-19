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

set(TEXINFOVERSION "6.8")
set(TEXINFO_MD5 "a91b404e30561a5df803e6eb3a53be71")
set(TEXINFO_URL "https://ftp.gnu.org/gnu/texinfo/texinfo-${TEXINFOVERSION}.tar.xz")

# Build wxWidgets static library (required for wxMaxima)
# We link statically, so the installation of the library is not
# necessary, we only need the locale files. But fortunately
# there is a make target, which does install the locale files only.
externalproject_add(texinfo
  URL "${TEXINFO_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${TEXINFO_MD5}
  CONFIGURE_COMMAND ${CMAKE_BINARY_DIR}/texinfo/texinfo-prefix/src/texinfo/configure --prefix=${CMAKE_BINARY_DIR}/texinfo-installroot
  BUILD_COMMAND $(MAKE)
  INSTALL_COMMAND $(MAKE) install
)

# Install wxWidgets locale files only.
#INSTALL(DIRECTORY "${CMAKE_BINARY_DIR}/wxwidgets-installroot/usr/local/share/" DESTINATION share/ COMPONENT wxMaxima)
