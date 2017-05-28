# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

set(IMAGEMAGICKVERSION "7.0.5-8-portable-Q16")

# Hash values for current releases are available from: https://www.imagemagick.org/download/binaries/digest.rdf
if(BUILD_64BIT)
    set(IMAGEMAGICK_SHA256 "d78eee82d2cd029c56b9469d7d53d2aa2ba4fbe37d72bdea74315c8629182f46")
    set(IMAGEMAGICK_URL    "https://www.imagemagick.org/download/binaries/ImageMagick-${IMAGEMAGICKVERSION}-x64.zip")
else()
    set(IMAGEMAGICK_SHA256 "028922789742b9da262fab85423bdd662a2d9f535a10b37c0a353872c9d24d53")
    set(IMAGEMAGICK_URL    "https://www.imagemagick.org/download/binaries/ImageMagick-${IMAGEMAGICKVERSION}-x86.zip")
endif()

# Imagemagick "convert" is needed for wxMaxima (already a binary package just extract it...)
externalproject_add(imagemagick
    URL "${IMAGEMAGICK_URL}"
    DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
    URL_HASH SHA256=${IMAGEMAGICK_SHA256}
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
)
install(FILES ${CMAKE_BINARY_DIR}/imagemagick/imagemagick-prefix/src/imagemagick/convert.exe DESTINATION wxMaxima)

