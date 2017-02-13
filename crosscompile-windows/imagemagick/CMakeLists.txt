# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

set(IMAGEMAGICKVERSION "7.0.4-8-portable-Q16")

if(BUILD_64BIT)
    set(IMAGEMAGICK_MD5 "e3468b8dfb71b5ee8c67390ccd402817")
    set(IMAGEMAGICK_URL "https://www.imagemagick.org/download/binaries/ImageMagick-${IMAGEMAGICKVERSION}-x64.zip")
else()
    set(IMAGEMAGICK_MD5 "ee4edf63d9dcdbd5a8906329043da816")
    set(IMAGEMAGICK_URL "https://www.imagemagick.org/download/binaries/ImageMagick-${IMAGEMAGICKVERSION}-x86.zip")
endif()

# Imagemagick "convert" is needed for wxMaxima (already a binary package just extract it...)
externalproject_add(imagemagick
  URL "${IMAGEMAGICK_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${IMAGEMAGICK_MD5}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)
install(FILES ${CMAKE_BINARY_DIR}/imagemagick/imagemagick-prefix/src/imagemagick/convert.exe DESTINATION wxMaxima)
