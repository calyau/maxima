# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

set(WXMAXIMAVERSION "16.12.0")

set(WXMAXIMA_MD5    "324e91f865996597e0f18a2231a94261")

set(WXMAXIMA_URL  "http://sourceforge.net/projects/wxmaxima/files/wxMaxima/${WXMAXIMAVERSION}/wxmaxima-${WXMAXIMAVERSION}.tar.gz")

set(WXMAXIMA_GIT "https://github.com/andrejv/wxmaxima.git")

if(USE_WXMAXIMA_GIT)
    externalproject_add(wxmaxima-git
        GIT_REPOSITORY ${WXMAXIMA_GIT}
        DEPENDS wxwidgets
        PATCH_COMMAND  ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets/wxwin.m4 ${CMAKE_BINARY_DIR}/wxmaxima/wxmaxima-git-prefix/src/wxmaxima-git/acinclude.m4
              COMMAND ${CMAKE_BINARY_DIR}/wxmaxima/wxmaxima-git-prefix/src/wxmaxima-git/bootstrap
        CONFIGURE_COMMAND  ${CMAKE_BINARY_DIR}/wxmaxima/wxmaxima-git-prefix/src/wxmaxima-git/configure --host=${HOST} --with-wx-config=${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets-build/wx-config --prefix=C:/maxima-${MAXIMAVERSION}
        BUILD_COMMAND $(MAKE)
              COMMAND $(MAKE) allmo
              COMMAND $(MAKE) wxMaxima.win
       INSTALL_COMMAND ""
    )
    install(DIRECTORY ${CMAKE_BINARY_DIR}/wxmaxima-git/wxmaxima-git-prefix/src/wxmaxima-git-build/wxMaxima/ DESTINATION wxMaxima)
else()
    externalproject_add(wxmaxima
        URL "${WXMAXIMA_URL}"
        DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
        URL_MD5 ${WXMAXIMA_MD5}
        DEPENDS wxwidgets
        CONFIGURE_COMMAND ${CMAKE_BINARY_DIR}/wxmaxima/wxmaxima-prefix/src/wxmaxima/configure --host=${HOST} --with-wx-config=${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets-build/wx-config --prefix=C:/maxima-${MAXIMAVERSION}
        BUILD_COMMAND $(MAKE)
              COMMAND $(MAKE) allmo
              COMMAND $(MAKE) wxMaxima.win
        INSTALL_COMMAND ""
    )
    install(DIRECTORY ${CMAKE_BINARY_DIR}/wxmaxima/wxmaxima-prefix/src/wxmaxima-build/wxMaxima/ DESTINATION wxMaxima)
endif()
