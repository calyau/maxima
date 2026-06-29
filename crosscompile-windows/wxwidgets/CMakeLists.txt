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

set(WXWIDGETSVERSION "3.3.2")
set(WXWIDGETS_MD5 "361dd5312350f49c41829f62083fdc1e")
set(WXWIDGETS_URL "https://github.com/wxWidgets/wxWidgets/releases/download/v${WXWIDGETSVERSION}/wxWidgets-${WXWIDGETSVERSION}.tar.bz2")

# WxWidgets Webview/Edge needs this package
file(DOWNLOAD https://www.nuget.org/api/v2/package/Microsoft.Web.WebView2/1.0.4022.49 ${CMAKE_SOURCE_DIR}/downloads/microsoft.web.webview2.1.0.4022.49.nupkg EXPECTED_HASH MD5=c6afa3240de0b466b6bb652a3fd1ae66 SHOW_PROGRESS)

externalproject_add(wxwidgets
  URL "${WXWIDGETS_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${WXWIDGETS_MD5}
  CMAKE_ARGS -DCMAKE_SYSTEM_NAME=${CMAKE_SYSTEM_NAME} -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER} -DCMAKE_RC_COMPILER=${CMAKE_RC_COMPILER} -DwxUSE_LUNASVG=builtin -DwxUSE_WEBVIEW_EDGE=YES
  PATCH_COMMAND cmake -E make_directory ${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets/3rdparty/webview2
        COMMAND cmake -E chdir ${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets/3rdparty/webview2 cmake -E tar x ${CMAKE_SOURCE_DIR}/downloads/microsoft.web.webview2.1.0.4022.49.nupkg

  BUILD_COMMAND $(MAKE)
        COMMAND $(MAKE) locale
   INSTALL_COMMAND ""
)


# Install wxWidgets locale files.
install(DIRECTORY "${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets/locale/" DESTINATION share/ COMPONENT wxMaxima FILES_MATCHING PATTERN "*.mo")

# Install DLLs
file(GLOB WXWIDGETS_DLLS "${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets-build/lib/*.dll")
install(FILES ${WXWIDGETS_DLLS} DESTINATION bin/ COMPONENT wxMaxima)
