# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

set(WXMAXIMAVERSION "17.10.0")

set(WXMAXIMA_MD5    "496f9cfc3f5880e45d0c9acbb9f17069")

set(WXMAXIMA_URL    "https://github.com/andrejv/wxmaxima/archive/Version-${WXMAXIMAVERSION}.tar.gz")

set(WXMAXIMA_GIT    "https://github.com/andrejv/wxmaxima.git")

if(USE_WXMAXIMA_GIT)
    find_package(Git REQUIRED)
    message(STATUS "Cloning or updating local wxmaxima git repository.")
    execute_process(COMMAND mkdir -p ${CMAKE_SOURCE_DIR}/downloads/wxmaxima-git
		    COMMAND sh -c "test -d wxmaxima-git/.git || \"${GIT_EXECUTABLE}\" clone \"${WXMAXIMA_GIT}\" wxmaxima-git"
		    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/downloads)

    execute_process(COMMAND sh -c "test -d .git && \"${GIT_EXECUTABLE}\" pull"
		    WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/downloads/wxmaxima-git)

    externalproject_add(wxmaxima-git
        GIT_REPOSITORY ${CMAKE_SOURCE_DIR}/downloads/wxmaxima-git
        DEPENDS wxwidgets
        PATCH_COMMAND  ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets/wxwin.m4 ${CMAKE_BINARY_DIR}/wxmaxima/wxmaxima-git-prefix/src/wxmaxima-git/acinclude.m4
        # search wx-config here - do not use the host wx-config
        CMAKE_CACHE_ARGS "-DCMAKE_PROGRAM_PATH:PATH=${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets-build"
        CMAKE_ARGS -DCMAKE_SYSTEM_NAME=${CMAKE_SYSTEM_NAME} -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER} -DCMAKE_RC_COMPILER=${CMAKE_RC_COMPILER} -DCMAKE_INSTALL_PREFIX=C:/maxima-${MAXIMAVERSION} -DCMAKE_BUILD_TYPE=DEBUG
        BUILD_COMMAND $(MAKE)
    )
    install(DIRECTORY ${CMAKE_BINARY_DIR}/wxmaxima/wxmaxima-git-prefix/src/wxmaxima-git-build/C\:/maxima-${MAXIMAVERSION}/ DESTINATION .)
else()
    externalproject_add(wxmaxima
        URL "${WXMAXIMA_URL}"
        DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
        DOWNLOAD_NAME wxmaxima-${WXMAXIMAVERSION}.tar.gz
        URL_MD5 ${WXMAXIMA_MD5}
        DEPENDS wxwidgets
	PATCH_COMMAND  ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets/wxwin.m4 ${CMAKE_BINARY_DIR}/wxmaxima/wxmaxima-prefix/src/wxmaxima/acinclude.m4
        # search wx-config here - do not use the host wx-config
        CMAKE_CACHE_ARGS "-DCMAKE_PROGRAM_PATH:PATH=${CMAKE_BINARY_DIR}/wxwidgets/wxwidgets-prefix/src/wxwidgets-build"
        CMAKE_ARGS -DCMAKE_SYSTEM_NAME=${CMAKE_SYSTEM_NAME} -DCMAKE_C_COMPILER=${CMAKE_C_COMPILER} -DCMAKE_CXX_COMPILER=${CMAKE_CXX_COMPILER} -DCMAKE_RC_COMPILER=${CMAKE_RC_COMPILER} -DCMAKE_INSTALL_PREFIX=C:/maxima-${MAXIMAVERSION}
        BUILD_COMMAND $(MAKE)
    )
    install(DIRECTORY ${CMAKE_BINARY_DIR}/wxmaxima/wxmaxima-prefix/src/wxmaxima-build/C\:/maxima-${MAXIMAVERSION}/ DESTINATION .)
endif()

# install wxWidgets locale files
install(DIRECTORY ${CMAKE_BINARY_DIR}/wxMaxima-translations/wxMaxima DESTINATION .)
