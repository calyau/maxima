# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

set(IMAGEMAGICKVERSION "7.0.5-10-portable-Q16")

# Hash values for current releases are available from: https://www.imagemagick.org/download/binaries/digest.rdf
if(BUILD_64BIT)
    set(IMAGEMAGICK_SHA256 "82f2c7e46c25ccbf0d62d3c0bc2fd395dc9a3d8e908c9cf6e24c26123299c955")
    set(IMAGEMAGICK_URL    "https://www.imagemagick.org/download/binaries/ImageMagick-${IMAGEMAGICKVERSION}-x64.zip")
else()
    set(IMAGEMAGICK_SHA256 "3f2cd04a2db94686fcbd4ee3dd8c340a3a2aa5772c1f2fab8170cd84d16e1b27")
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

