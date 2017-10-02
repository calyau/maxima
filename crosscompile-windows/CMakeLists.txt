# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

set(CMAKE_SYSTEM_NAME Windows)


option(BUILD_64BIT "Build a 64 bit installer." NO)

if(BUILD_64BIT)
    set(HOST x86_64-w64-mingw32)
    set(INSTALLERSUFFIX "win64")
else()
    set(HOST i686-w64-mingw32)
    set(INSTALLERSUFFIX "win32")
endif()
    
set(CMAKE_C_COMPILER   ${HOST}-gcc)
set(CMAKE_CXX_COMPILER ${HOST}-g++)
set(CMAKE_RC_COMPILER  ${HOST}-windres)
project(maxima)
cmake_minimum_required(VERSION 3.6)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/)


# What is the build system?
execute_process(COMMAND gcc -dumpmachine OUTPUT_VARIABLE BUILDHOST OUTPUT_STRIP_TRAILING_WHITESPACE)
message(STATUS "Build system: ${BUILDHOST}")

# Output the host system, for which we are compiling
message(STATUS "Host system: ${HOST}")


file(READ ${CMAKE_ROOT}/Modules/NSIS.template.in MYNSISTEMPLATE)
string(REPLACE "RequestExecutionLevel admin" "RequestExecutionLevel user" 
        MYNSISTEMPLATE "${MYNSISTEMPLATE}")
file(WRITE ${CMAKE_BINARY_DIR}/NSIS.template.in "${MYNSISTEMPLATE}")
message (STATUS "Copied and patched ${CMAKE_ROOT}/Modules/NSIS.template.in to ${CMAKE_BINARY_DIR}/NSIS.template.in")

option(USE_WXMAXIMA_GIT "Use wxMaxima development snapshot." NO)
option(USE_VTK "Use VTK." YES)
# the following option is used for nightly builds of the installer.
# it builds the current Maxima/wxMaxima Git versions and also
# sets the installation directory to C:\maxima-current
option(BUILD_CURRENT "Build a installer for current Maxima/wxMaxima." NO)



# If you want to use a updated version of a program,
# update the version number and the checksum.
# If no further patches are needed, you should get a
# updated setup-file automatically.

# current program versions
set(CLISPVERSION       2.49)


# MD5 checksums for the current program versions.
set(CLISP_MD5       "8d58d64f83dd7ad01c054aecc16c1f52")
set(TEXFONTS_MD5    "8101a6feca2c43a6b9218195df9e7c35")

if(BUILD_CURRENT)
    set(USE_WXMAXIMA_GIT YES)
    set(MAXIMAVERSION "current")
else()
    # Get Maxima version from configure.ac
    file(STRINGS "${CMAKE_SOURCE_DIR}/../configure.ac" CONFIGUREAC REGEX "AC_INIT\\(.*\\)")
    string(REGEX REPLACE "AC_INIT\\(\\[maxima\\], \\[(.*)\\]\\)" "\\1" MAXIMAVERSION ${CONFIGUREAC})
    message(STATUS "Maxima version (from configure.ac): ${MAXIMAVERSION}")
endif()



# download URLs
set(CLISP_URL     "http://downloads.sourceforge.net/project/clisp/clisp/${CLISPVERSION}/clisp-${CLISPVERSION}-win32-mingw-big.zip")

# No license at the homepage, but the TeX fonts are public domain according to:
# http://archive.debian.net/changelogs/pool/main/t/ttf-jsmath/ttf-jsmath_0.01-2/copyright
# https://www.mail-archive.com/sage-devel@googlegroups.com/msg09750.html
# https://www.mail-archive.com/sage-devel@googlegroups.com/msg09748.html
set(TEXFONTS_URL  "http://www.math.union.edu/~dpvc/jsMath/download/TeX-fonts-25.zip")



# Where are libgcc and libstdc++?
if(BUILD_64BIT)
    execute_process(COMMAND "${CMAKE_CXX_COMPILER}" -print-file-name=libgcc_s_seh-1.dll OUTPUT_VARIABLE MINGW_LIBGCC    OUTPUT_STRIP_TRAILING_WHITESPACE)
else()
    execute_process(COMMAND "${CMAKE_CXX_COMPILER}" -print-file-name=libgcc_s_sjlj-1.dll OUTPUT_VARIABLE MINGW_LIBGCC    OUTPUT_STRIP_TRAILING_WHITESPACE)
endif()

execute_process(COMMAND "${CMAKE_CXX_COMPILER}" -print-file-name=libstdc++-6.dll     OUTPUT_VARIABLE MINGW_LIBSTDCPP OUTPUT_STRIP_TRAILING_WHITESPACE)

message(STATUS "Found libstdc++ at ${MINGW_LIBSTDCPP}")

message(STATUS "Found libgcc at ${MINGW_LIBGCC}")

add_subdirectory("wine/")

find_program(SEVENZIP_EXE NAMES 7z)

if(SEVENZIP_EXE)
    message(STATUS "Found 7z at ${SEVENZIP_EXE}")
else()
    message(FATAL_ERROR "7z not found. It is required for crosscompiling Maxima.")
endif()

include(ExternalProject)



# Clisp (already a binary package just extract it...)
#
# What is the difference between clisp-2.49-win32-mingw-big.zip and clisp-2.49-win32-mingw-small.zip?
# What should be used for running Maxima?
externalproject_add(clisp
  URL "${CLISP_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${CLISP_MD5}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)
install(DIRECTORY ${CMAKE_BINARY_DIR}/clisp-prefix/src/clisp/ DESTINATION clisp-${CLISPVERSION})

add_subdirectory("sbcl/")

add_subdirectory("gnuplot/")

add_subdirectory("tcltk/")

add_subdirectory("wxwidgets/")

add_subdirectory("wxmaxima/")

add_subdirectory("vtk/")

# TeX Fonts (a binary package just extract it...)
externalproject_add(texfonts
  URL "${TEXFONTS_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${TEXFONTS_MD5}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)
install(DIRECTORY ${CMAKE_BINARY_DIR}/texfonts-prefix/src/texfonts/ DESTINATION wxMaxima/fonts)


set(CLISPROOT "${CMAKE_BINARY_DIR}/clisp-prefix/src/clisp")
configure_file("${CMAKE_SOURCE_DIR}/wine-clisp.sh.tmpl" "${CMAKE_BINARY_DIR}/wine-clisp.sh")
configure_file("${CMAKE_SOURCE_DIR}/wine-lisp.sh.tmpl" "${CMAKE_BINARY_DIR}/wine-lisp.sh")
configure_file("${CMAKE_SOURCE_DIR}/wine-sbcl.sh.tmpl" "${CMAKE_BINARY_DIR}/wine-sbcl.sh")


externalproject_add(maxima
  # first copy the maxima source to a build directory, since out-of-source-builds do not work
  DOWNLOAD_COMMAND rsync -a --exclude=crosscompile-windows/build/ "${CMAKE_SOURCE_DIR}/../"  "${CMAKE_BINARY_DIR}/maxima-prefix/src/maxima"
  DEPENDS clisp sbcl tcl tk
  BUILD_IN_SOURCE 1
  PATCH_COMMAND sh -c "test \"${BUILD_64BIT}\" != \"OFF\" && patch --binary -p1 < ${CMAKE_SOURCE_DIR}/maxima.bat.in-patch || exit 0"
  CONFIGURE_COMMAND sh -c "test -x ./bootstrap && ./bootstrap || return 0"
            COMMAND ./configure --host=${HOST} --enable-clisp --with-clisp=${CMAKE_BINARY_DIR}/wine-clisp.sh --with-clisp-runtime=${CMAKE_BINARY_DIR}/wine-lisp.sh --enable-sbcl --with-sbcl=${CMAKE_BINARY_DIR}/wine-sbcl.sh --prefix=C:/maxima-${MAXIMAVERSION}
  # clisp-depends.mk is generated with "\" as path seperator, not "/" - correct that...
  BUILD_COMMAND LANG=C $(MAKE) -C src clisp-depends.mk
        COMMAND sed -i s+\\\\+/+g src/clisp-depends.mk
        COMMAND LANG=C $(MAKE)
        COMMAND LANG=C $(MAKE) pdf
  INSTALL_COMMAND LANG=C $(MAKE) install DESTDIR=${CMAKE_BINARY_DIR}/maxima-installroot/
)

# crosscompiling winkill with autoconf does not work, compile it using CMake.
add_library(winkill_lib SHARED ${CMAKE_SOURCE_DIR}/../interfaces/xmaxima/win32/winkill_lib.c)
add_executable(winkill ${CMAKE_SOURCE_DIR}/../interfaces/xmaxima/win32/winkill.c)
set_target_properties(winkill_lib PROPERTIES PREFIX "") # remove 'lib' prefix from libwinkill_lib.dll
target_link_libraries(winkill winkill_lib)
install(TARGETS winkill winkill_lib RUNTIME DESTINATION bin LIBRARY DESTINATION bin)
install(FILES "${CMAKE_SOURCE_DIR}/../interfaces/xmaxima/win32/win_signals.lisp" DESTINATION bin/)

install(DIRECTORY ${CMAKE_BINARY_DIR}/maxima-installroot/C:/maxima-${MAXIMAVERSION}/ DESTINATION .)
install(FILES "${CMAKE_SOURCE_DIR}/xmaxima.bat" DESTINATION bin/)
install(FILES "${CMAKE_BINARY_DIR}/maxima-prefix/src/maxima/doc/info/maxima.pdf" DESTINATION share/doc/)
install(FILES "${CMAKE_SOURCE_DIR}/lispselector.tcl" DESTINATION bin/)
install(FILES "${CMAKE_SOURCE_DIR}/lispselector.bat" DESTINATION bin/)



# Include DLLs from the current Mingw environment.
install(FILES "${MINGW_LIBSTDCPP}" "${MINGW_LIBGCC}" DESTINATION wxMaxima/)
install(FILES "${MINGW_LIBSTDCPP}" "${MINGW_LIBGCC}" DESTINATION bin/)

install(FILES "licenses.html" DESTINATION share/doc/)


set(CPACK_GENERATOR "NSIS;ZIP")
set(CPACK_PACKAGE_VERSION "${MAXIMAVERSION}")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Maxima")
set(CPACK_PACKAGE_VENDOR "Maxima Team")
set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_SOURCE_DIR}/../README")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_SOURCE_DIR}/../COPYING")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "maxima-${CPACK_PACKAGE_VERSION}")
set(CPACK_NSIS_INSTALL_ROOT "C:")
set(CPACK_NSIS_ENABLE_UNINSTALL_BEFORE_INSTALL ON)
set(CPACK_PACKAGE_ICON "${CMAKE_SOURCE_DIR}/maxima-icon.bmp")
set(CPACK_NSIS_MUI_ICON "${CMAKE_SOURCE_DIR}/../interfaces/xmaxima\\\\maxima-icon.ico")
set(CPACK_NSIS_MUI_UNIICON "${CPACK_NSIS_MUI_ICON}")
set(CPACK_PACKAGE_EXECUTABLES "..\\\\wxMaxima\\\\wxmaxima" "wxMaxima (GUI for Maxima)")
set(CPACK_NSIS_URL_INFO_ABOUT "http://maxima.sourceforge.net")
set(CPACK_NSIS_MENU_LINKS "bin/maxima.bat" "Maxima (command line)" "bin/xmaxima.bat" "XMaxima (simple GUI)" "http://maxima.sourceforge.net" "About Maxima" "https://andrejv.github.io/wxmaxima/" "About wxMaxima" "share/doc/maxima.pdf" "Maxima documentation"  "share/doc/licenses.html" "Maxima and included software licenses" "bin/lispselector.bat" "Configure default LISP for Maxima")
set(CPACK_PACKAGE_FILE_NAME "maxima-clisp-sbcl-${CPACK_PACKAGE_VERSION}-${INSTALLERSUFFIX}")
set(CPACK_NSIS_COMPRESSOR "/SOLID lzma")

# File associations:
set(CPACK_NSIS_DEFINES "!include ${CMAKE_SOURCE_DIR}\\\\FileAssociation.nsh")
set(CPACK_NSIS_EXTRA_INSTALL_COMMANDS "
\\\${registerExtension} \\\"\\\$INSTDIR\\\\wxMaxima\\\\wxmaxima.exe\\\" \\\".wxm\\\" \\\"wxMaxima Document\\\"
\\\${registerExtension} \\\"\\\$INSTDIR\\\\wxMaxima\\\\wxmaxima.exe\\\" \\\".wxmx\\\" \\\"wxMaxima XML Document\\\"
\\\${registerExtension} \\\"\\\$INSTDIR\\\\wxMaxima\\\\wxmaxima.exe\\\" \\\".mac\\\" \\\"Maxima Batch file\\\"
WriteRegStr HKCR \\\".wxmx\\\\ShellNew\\\" \\\"NullFile\\\" \\\"\\\"
")
set(CPACK_NSIS_EXTRA_UNINSTALL_COMMANDS "
\\\${unregisterExtension} \\\".wxm\\\" \\\"wxMaxima Document\\\"
\\\${unregisterExtension} \\\".wxmx\\\" \\\"wxMaxima XML Document\\\"
\\\${unregisterExtension} \\\".mac\\\" \\\"Maxima Batch File\\\"
DeleteRegKey HKCR \\\".wxmx\\\\ShellNew\\\"
")


SET(CPACK_NSIS_INSTALLER_MUI_ICON_CODE "
  !define MUI_WELCOMEFINISHPAGE_BITMAP \\\"${CMAKE_SOURCE_DIR}/maxima-installerimage.bmp\\\"") 
include(CPack)
