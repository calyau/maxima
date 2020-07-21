set(ABCL_VERSION 1.7.1)
set(ABCL_MD5 "3af3d048a77a93564bc338947f40a31c")
set(ABCL_URL "https://abcl.org/releases/${ABCL_VERSION}/abcl-bin-${ABCL_VERSION}.tar.gz")

externalproject_add(abcl
  URL "${ABCL_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${ABCL_MD5}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)
