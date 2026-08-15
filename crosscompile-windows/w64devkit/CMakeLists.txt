# SPDX-License-Identifier: GPL-2.0-or-later

set(W64DEVKIT_VERSION "2.9.1")
set(W64DEVKIT_MD5 "0669c822860a57dfb473daa3e7f910d9")
set(W64DEVKIT_URL "https://github.com/skeeto/w64devkit/releases/download/v${W64DEVKIT_VERSION}/w64devkit-x64-${W64DEVKIT_VERSION}.7z.exe")

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
  COMMAND mkdir -p ${CMAKE_BINARY_DIR}/w64devkit && cd ${CMAKE_BINARY_DIR}/w64devkit && ${SEVENZIP_EXE} x -y ${CMAKE_SOURCE_DIR}/downloads/w64devkit-x64-${W64DEVKIT_VERSION}.7z.exe
  DEPENDEES download
  DEPENDERS patch
)

install(DIRECTORY ${CMAKE_BINARY_DIR}/w64devkit/w64devkit/ DESTINATION w64devkit COMPONENT ECL)
