# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

set(IMAGEMAGICKVERSION "7.0.6-10-portable-Q16")

# Hash values for current releases are available from: https://www.imagemagick.org/download/binaries/digest.rdf
if(BUILD_64BIT)
    set(IMAGEMAGICK_SHA256 "6bc7cdb016fb0795325b5a6c6e3d40d54bc46480465b290fb23cb4b7cda51aaa")
    set(IMAGEMAGICK_URL    "https://www.imagemagick.org/download/binaries/ImageMagick-${IMAGEMAGICKVERSION}-x64.zip")
else()
    set(IMAGEMAGICK_SHA256 "6838568074db6ab2e6ca4acff6d2978bd77a84a4f7ee262a049bb98641ce582e")
    set(IMAGEMAGICK_URL    "https://www.imagemagick.org/download/binaries/ImageMagick-${IMAGEMAGICKVERSION}-x86.zip")
endif()

# the current git version of wxMaxima no longer needs Imagemagick's "convert"
if(NOT USE_WXMAXIMA_GIT)
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
endif()

