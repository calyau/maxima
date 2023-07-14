# SPDX-License-Identifier: GPL-2.0-or-later
set(ABCL_VERSION 1.9.2)
set(ABCL_MD5 "326aeac87eaebb03591c74a0f54c30a3")
set(ABCL_URL "https://abcl.org/releases/${ABCL_VERSION}/abcl-bin-${ABCL_VERSION}.tar.gz")

externalproject_add(abcl
  URL "${ABCL_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${ABCL_MD5}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)
