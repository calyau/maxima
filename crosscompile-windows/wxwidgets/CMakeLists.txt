# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

set(WXWIDGETSVERSION   "3.1.0")

set(WXWIDGETS_MD5   "e20c14bb9bf5d4ec0979a3cd7510dece")

set(WXWIDGETS_URL "https://github.com/wxWidgets/wxWidgets/releases/download/v${WXWIDGETSVERSION}/wxWidgets-${WXWIDGETSVERSION}.tar.bz2")

configure_file("${CMAKE_SOURCE_DIR}/wxwidgets/wxwidgets-install-translations-for-wxmaxima.sh.tmpl" "${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-install-translations-for-wxmaxima.sh")

# Build wxWidgets static library (required for wxMaxima)
externalproject_add(wxwidgets
  URL "${WXWIDGETS_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${WXWIDGETS_MD5}
  CONFIGURE_COMMAND ${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets/configure --disable-shared --disable-compat28 --with-zlib=builtin --with-libjpeg=builtin --without-libtiff --with-libpng=builtin --with-expat=builtin --build=${BUILDHOST} --host=${HOST} --enable-accessibility
  BUILD_COMMAND $(MAKE)
        COMMAND ${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-install-translations-for-wxmaxima.sh
  INSTALL_COMMAND ""
)
