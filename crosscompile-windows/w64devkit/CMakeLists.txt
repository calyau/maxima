# SPDX-License-Identifier: GPL-2.0-or-later

set(W64DEVKIT_MD5 "97563256fea44229e5bcfa3a73443b5b")
set(W64DEVKIT_URL "https://github.com/skeeto/w64devkit/releases/download/v2.8.0/w64devkit-x64-2.8.0.7z.exe")

externalproject_add(w64devkit
  URL "${W64DEVKIT_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${W64DEVKIT_MD5}
  DOWNLOAD_NO_EXTRACT 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND "" # ${CMAKE_COMMAND} -E copy_directory "${CMAKE_BINARY_DIR}/w64devkit/w64devkit-prefix/src/w64devkit" "${CMAKE_BINARY_DIR}/mingw"
  INSTALL_COMMAND ""
)

ExternalProject_Add_Step(w64devkit extract
  COMMENT "Performing extraction step for 'W64DEVKIT'"
  COMMAND mkdir -p ${CMAKE_BINARY_DIR}/w64devkit && cd ${CMAKE_BINARY_DIR}/w64devkit && ${SEVENZIP_EXE} x -y ${CMAKE_SOURCE_DIR}/downloads/w64devkit-x64-2.8.0.7z.exe
  DEPENDEES download
  DEPENDERS patch
)

install(DIRECTORY ${CMAKE_BINARY_DIR}/w64devkit/w64devkit/ DESTINATION w64devkit COMPONENT ECL)
