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

set(WXWIDGETSVERSION "3.2.2.1")
set(WXWIDGETS_MD5 "45bd5f56a06e7c4ca7caf6c0b4d5d506")
set(WXWIDGETS_URL "https://github.com/wxWidgets/wxWidgets/releases/download/v${WXWIDGETSVERSION}/wxWidgets-${WXWIDGETSVERSION}.tar.bz2")

# Build wxWidgets static library (required for wxMaxima)
# We link statically, so the installation of the library is not
# necessary, we only need the locale files. But fortunately
# there is a make target, which does install the locale files only.
externalproject_add(wxwidgets
  URL "${WXWIDGETS_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${WXWIDGETS_MD5}
  CONFIGURE_COMMAND ${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets/configure --disable-shared --disable-compat28 --with-zlib=builtin --with-libjpeg=builtin --without-libtiff --with-libpng=builtin --with-expat=builtin --build=${BUILDHOST} --host=${HOST} --enable-accessibility
  BUILD_COMMAND $(MAKE)
  INSTALL_COMMAND $(MAKE) locale_install "DESTDIR=${CMAKE_BINARY_DIR}/wxwidgets-installroot"
)

# Install wxWidgets locale files only.
install(DIRECTORY "${CMAKE_BINARY_DIR}/wxwidgets-installroot/usr/local/share/" DESTINATION share/ COMPONENT wxMaxima)
