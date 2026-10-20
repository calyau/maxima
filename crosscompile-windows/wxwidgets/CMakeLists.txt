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

set(WXWIDGETSVERSION "3.3.4")
set(WXWIDGETS_MD5 "2594b43a82431cd2e9efcd17e10ee2af")
set(WXWIDGETS_URL "https://github.com/wxWidgets/wxWidgets/releases/download/v${WXWIDGETSVERSION}/wxWidgets-${WXWIDGETSVERSION}.tar.bz2")

# WxWidgets Webview/Edge needs this package

# MSWEBVIEWVERSION / MSWEBVIEW_MD5 is set in the directory above.

externalproject_add(wxwidgets
  URL "${WXWIDGETS_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${WXWIDGETS_MD5}
  DEPENDS mswebview
  CMAKE_ARGS -DCMAKE_SYSTEM_NAME=${CMAKE_SYSTEM_NAME} -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER} -DCMAKE_RC_COMPILER=${CMAKE_RC_COMPILER} -DwxUSE_LUNASVG=builtin -DwxUSE_WEBVIEW_EDGE=YES -DwxBUILD_LOCALES=ON
  PATCH_COMMAND cmake -E make_directory ${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets/3rdparty/webview2
        COMMAND cmake -E chdir ${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets/3rdparty/webview2 cmake -E tar x ${CMAKE_SOURCE_DIR}/downloads/microsoft.web.webview2.${MSWEBVIEWVERSION}.nupkg
  BUILD_COMMAND $(MAKE)
        COMMAND $(MAKE) locale
        COMMAND $(MAKE) install DESTDIR=${CMAKE_BINARY_DIR}/wxwidgets-installroot/
  INSTALL_COMMAND ""
)


# Install wxWidgets locale files.
install(DIRECTORY "${CMAKE_BINARY_DIR}/wxwidgets-installroot/usr/local/share/locale" DESTINATION share COMPONENT wxMaxima FILES_MATCHING PATTERN "*.mo")

# Install wxWidgets DLLs.
install(DIRECTORY "${CMAKE_BINARY_DIR}/wxwidgets-installroot/usr/local/bin/" DESTINATION bin COMPONENT wxMaxima FILES_MATCHING PATTERN "*.dll")
