# SPDX-License-Identifier: GPL-2.0-or-later
set(CCL_VERSION 1.12.2)
set(CCL_MD5 "b5b327242b73b50cda40fddbb8cdd321")
set(CCL_URL "https://github.com/Clozure/ccl/releases/download/v${CCL_VERSION}/ccl-${CCL_VERSION}-windowsx86.zip")

externalproject_add(ccl
  URL "${CCL_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${CCL_MD5}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)
install(DIRECTORY ${CMAKE_BINARY_DIR}/ccl/ccl-prefix/src/ccl/ DESTINATION bin COMPONENT CCL64)
