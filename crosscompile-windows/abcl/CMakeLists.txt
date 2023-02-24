# SPDX-License-Identifier: GPL-2.0-or-later
set(ABCL_VERSION 1.9.1)
set(ABCL_MD5 "f230ca769c97420e500b6d5915263e1d")
set(ABCL_URL "https://abcl.org/releases/${ABCL_VERSION}/abcl-bin-${ABCL_VERSION}.tar.gz")

externalproject_add(abcl
  URL "${ABCL_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${ABCL_MD5}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)
