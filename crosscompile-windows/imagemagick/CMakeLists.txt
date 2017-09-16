# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

set(IMAGEMAGICKVERSION "7.0.7-2-portable-Q16")

# Hash values for current releases are available from: https://www.imagemagick.org/download/binaries/digest.rdf
if(BUILD_64BIT)
    set(IMAGEMAGICK_SHA256 "8318b1edc89ef695470e8ce2e841337ecf3a2016309310464796fb7159b092f4")
    set(IMAGEMAGICK_URL    "https://www.imagemagick.org/download/binaries/ImageMagick-${IMAGEMAGICKVERSION}-x64.zip")
else()
    set(IMAGEMAGICK_SHA256 "606a70407b93a20b8db460613387b18c68134bebbea2d42fa5200d5f7b5a925d")
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

