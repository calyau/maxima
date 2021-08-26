# SPDX-License-Identifier: GPL-2.0-or-later
set(ABCL_VERSION 1.8.0)
set(ABCL_MD5 "4e5c6234b21097cdcaccd126c640f7f9")
set(ABCL_URL "https://abcl.org/releases/${ABCL_VERSION}/abcl-bin-${ABCL_VERSION}.tar.gz")

externalproject_add(abcl
  URL "${ABCL_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${ABCL_MD5}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)
