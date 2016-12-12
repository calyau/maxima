# CMake file for crosscompiling Maxima/wxMaxima for Windows
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

set(CMAKE_SYSTEM_NAME Windows)

set(HOST i686-w64-mingw32)
set(CMAKE_C_COMPILER   ${HOST}-gcc)
set(CMAKE_CXX_COMPILER ${HOST}-g++)
set(CMAKE_RC_COMPILER  ${HOST}-windres)
project(maxima)
cmake_minimum_required(VERSION 3.6)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR}/ ${CMAKE_BINARY_DIR}/)


file(READ ${CMAKE_ROOT}/Modules/NSIS.template.in MYNSISTEMPLATE)
string(REPLACE "RequestExecutionLevel admin" "RequestExecutionLevel user" 
        MYNSISTEMPLATE "${MYNSISTEMPLATE}")
string(REPLACE "!insertmacro MUI_PAGE_DIRECTORY" "; page directory disabled: !insertmacro MUI_PAGE_DIRECTORY" 
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
set(CLISPVERSION     2.49)
set(WXMAXIMAVERSION  16.12.0)
set(WXWIDGETSVERSION 3.1.0)
set(GNUPLOTVERSION   5.0.4)
set(TCLVERSION       8.6.6)
set(TKVERSION        8.6.6)
set(VTKVERSION       6.3.0)
set(SBCLVERSION      1.3.12)


# MD5 checksums for the current program versions.
set(CLISP_MD5     "8d58d64f83dd7ad01c054aecc16c1f52")
set(WXMAXIMA_MD5  "324e91f865996597e0f18a2231a94261")
set(WXWIDGETS_MD5 "e20c14bb9bf5d4ec0979a3cd7510dece")
set(GNUPLOT_MD5   "ff740f99cac26479f48d7d7eb0f32284")
set(TCL_MD5       "5193aea8107839a79df8ac709552ecb7")
set(TK_MD5        "dd7dbb3a6523c42d05f6ab6e86096e99")
set(TEXFONTS_MD5  "8101a6feca2c43a6b9218195df9e7c35")
set(VTK_MD5       "8d8b0878be81a0ab471e929f828dfe9a")
set(SBCL_MD5      "918fb23e00645e56c8687df4e06c30ae")

if(BUILD_CURRENT)
    set(USE_WXMAXIMA_GIT YES)
    set(MAXIMAVERSION "current")
else()
    # Get Maxima version from configure.ac
    file(STRINGS "${CMAKE_SOURCE_DIR}/../configure.ac" CONFIGUREAC REGEX "AC_INIT\\(.*\\)")
    string(REGEX REPLACE "AC_INIT\\(\\[maxima\\], \\[(.*)\\]\\)" "\\1" MAXIMAVERSION ${CONFIGUREAC})
    message(STATUS "Maxima version (from configure.ac): ${MAXIMAVERSION}")
endif()



string(REPLACE "." "" GNUPLOTVERSIONWITHOUTDOTS "${GNUPLOTVERSION}")

# download URLs
set(CLISP_URL     "http://downloads.sourceforge.net/project/clisp/clisp/${CLISPVERSION}/clisp-${CLISPVERSION}-win32-mingw-big.zip")
set(WXMAXIMA_URL  "http://sourceforge.net/projects/wxmaxima/files/wxMaxima/${WXMAXIMAVERSION}/wxmaxima-${WXMAXIMAVERSION}.tar.gz")
set(WXWIDGETS_URL "https://github.com/wxWidgets/wxWidgets/releases/download/v${WXWIDGETSVERSION}/wxWidgets-${WXWIDGETSVERSION}.tar.bz2")
set(GNUPLOT_URL   "http://sourceforge.net/projects/gnuplot/files/gnuplot/${GNUPLOTVERSION}/gp${GNUPLOTVERSIONWITHOUTDOTS}-win32-mingw.zip")
set(TCL_URL       "http://prdownloads.sourceforge.net/tcl/tcl${TCLVERSION}-src.tar.gz")
set(TK_URL        "http://prdownloads.sourceforge.net/tcl/tk${TKVERSION}-src.tar.gz")
set(VTK_URL       "http://www.vtk.org/files/release/6.3/vtkpython-${VTKVERSION}-Windows-32bit.exe")
set(SBCL_URL      "http://prdownloads.sourceforge.net/sbcl/sbcl-${SBCLVERSION}-x86-windows-binary.msi")

# No license at the homepage, but the TeX fonts are public domain according to:
# http://archive.debian.net/changelogs/pool/main/t/ttf-jsmath/ttf-jsmath_0.01-2/copyright
# https://www.mail-archive.com/sage-devel@googlegroups.com/msg09750.html
# https://www.mail-archive.com/sage-devel@googlegroups.com/msg09748.html
set(TEXFONTS_URL  "http://www.math.union.edu/~dpvc/jsMath/download/TeX-fonts-25.zip")

set(WXMAXIMA_GIT "https://github.com/andrejv/wxmaxima.git")


# Where are libgcc_s_sjlj-1.dll and libstdc++-6.dll?
#
execute_process(COMMAND "${CMAKE_CXX_COMPILER}" -print-file-name=libgcc_s_sjlj-1.dll OUTPUT_VARIABLE MINGW_LIBGCC    OUTPUT_STRIP_TRAILING_WHITESPACE)
execute_process(COMMAND "${CMAKE_CXX_COMPILER}" -print-file-name=libstdc++-6.dll     OUTPUT_VARIABLE MINGW_LIBSTDCPP OUTPUT_STRIP_TRAILING_WHITESPACE)

message(STATUS "Found libstdc++-6.dll at ${MINGW_LIBSTDCPP}")
message(STATUS "Found libgcc_s_sjlj-1.dll at ${MINGW_LIBGCC}")


find_program(WINE_EXE NAMES wine)

if(WINE_EXE)
    message(STATUS "Found wine at ${WINE_EXE}")
else()
    message(FATAL_ERROR "Wine not found. It is required for crosscompiling Maxima.")
endif()

find_program(SEVENZIP_EXE NAMES 7z)

if(SEVENZIP_EXE)
    message(STATUS "Found 7z at ${SEVENZIP_EXE}")
else()
    message(FATAL_ERROR "7z not found. It is required for crosscompiling Maxima.")
endif()

include(ExternalProject)

# Build wxWidgets static library (required for wxMaxima)
# --host=${HOST} should suffice, but then wxwidgets creates libs with wrong names, so add --build=${HOST}
# (that should mean, the build system is Windows - wrong. Perhaps a bug in the wxwidgets build system.)
externalproject_add(wxWidgets
  URL "${WXWIDGETS_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${WXWIDGETS_MD5}
  CONFIGURE_COMMAND ${CMAKE_BINARY_DIR}/wxWidgets-prefix/src/wxWidgets/configure --disable-shared --disable-compat28 --with-zlib=builtin --with-libjpeg=builtin --without-libtiff --with-libpng=builtin --with-expat=builtin --host=${HOST} --build=${HOST}
  BUILD_COMMAND $(MAKE)
  INSTALL_COMMAND ""
)

# build tcl/tk (required for xmaxima and the Lisp selection tool)
externalproject_add(tcl
  URL "${TCL_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${TCL_MD5}
  CONFIGURE_COMMAND ${CMAKE_BINARY_DIR}/tcl-prefix/src/tcl/win/configure --host=${HOST} --prefix=C:/maxima-${MAXIMAVERSION}
  BUILD_COMMAND $(MAKE)
        COMMAND $(MAKE) install
)
install(DIRECTORY ${CMAKE_BINARY_DIR}/tcl-prefix/src/tcl-build/C:/maxima-${MAXIMAVERSION}/ DESTINATION .)

externalproject_add(tk
  URL "${TK_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${TK_MD5}
  DEPENDS tcl
  CONFIGURE_COMMAND ${CMAKE_BINARY_DIR}/tk-prefix/src/tk/win/configure --host=${HOST} --with-tcl=${CMAKE_BINARY_DIR}/tcl-prefix/src/tcl-build/C:/maxima-${MAXIMAVERSION}/lib --prefix=C:/maxima-${MAXIMAVERSION}
  BUILD_COMMAND $(MAKE)
        COMMAND $(MAKE) install
)
install(DIRECTORY ${CMAKE_BINARY_DIR}/tk-prefix/src/tk-build/C:/maxima-${MAXIMAVERSION}/ DESTINATION .)

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

externalproject_add(sbcl
  URL "${SBCL_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${SBCL_MD5}
  DOWNLOAD_NO_EXTRACT 1
  CONFIGURE_COMMAND ""
  BUILD_COMMAND cd ${CMAKE_BINARY_DIR}/sbcl && ${CMAKE_SOURCE_DIR}/sbcl.sh
  INSTALL_COMMAND ""
)
ExternalProject_Add_Step(sbcl extract
  COMMENT "Performing extraction step for 'SBCL'"
  COMMAND mkdir -p ${CMAKE_BINARY_DIR}/sbcl && cd ${CMAKE_BINARY_DIR}/sbcl && ${SEVENZIP_EXE} x -y ${CMAKE_SOURCE_DIR}/downloads/sbcl-${SBCLVERSION}-x86-windows-binary.msi
  DEPENDEES download
  DEPENDERS patch
)

install(DIRECTORY ${CMAKE_BINARY_DIR}/sbcl/ DESTINATION bin)

# Gnuplot (already a binary package just extract it...)
externalproject_add(gnuplot
  URL "${GNUPLOT_URL}"
  DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
  URL_MD5 ${GNUPLOT_MD5}
  CONFIGURE_COMMAND ""
  BUILD_COMMAND ""
  INSTALL_COMMAND ""
)
install(DIRECTORY ${CMAKE_BINARY_DIR}/gnuplot-prefix/src/gnuplot/ DESTINATION gnuplot)

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


configure_file("${CMAKE_SOURCE_DIR}/setup-imaxima-imath.el-patch.tmpl" "${CMAKE_BINARY_DIR}/setup-imaxima-imath.el-patch" NEWLINE_STYLE WIN32)

externalproject_add(maxima
  # first copy the maxima source to a build directory, since out-of-source-builds do not work
  DOWNLOAD_COMMAND rsync -a --exclude=crosscompile-windows/build/ "${CMAKE_SOURCE_DIR}/../"  "${CMAKE_BINARY_DIR}/maxima-prefix/src/maxima"
  DEPENDS clisp sbcl tcl tk
  BUILD_IN_SOURCE 1
  PATCH_COMMAND patch --binary -p0 < ${CMAKE_BINARY_DIR}/setup-imaxima-imath.el-patch
  CONFIGURE_COMMAND ./bootstrap
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

if(USE_WXMAXIMA_GIT)
    externalproject_add(wxMaxima-git
        GIT_REPOSITORY ${WXMAXIMA_GIT}
        DEPENDS wxWidgets
        PATCH_COMMAND  ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/wxWidgets-prefix/src/wxWidgets/wxwin.m4 ${CMAKE_BINARY_DIR}/wxMaxima-git-prefix/src/wxMaxima-git/acinclude.m4
              COMMAND ${CMAKE_BINARY_DIR}/wxMaxima-git-prefix/src/wxMaxima-git/bootstrap
        CONFIGURE_COMMAND  ${CMAKE_BINARY_DIR}/wxMaxima-git-prefix/src/wxMaxima-git/configure --host=${HOST} --with-wx-config=${CMAKE_BINARY_DIR}/wxWidgets-prefix/src/wxWidgets-build/wx-config --prefix=C:/maxima-${MAXIMAVERSION}
        BUILD_COMMAND $(MAKE)
              COMMAND $(MAKE) allmo
              COMMAND $(MAKE) wxMaxima.win
       INSTALL_COMMAND ""
    )
    install(DIRECTORY ${CMAKE_BINARY_DIR}/wxMaxima-git-prefix/src/wxMaxima-git-build/wxMaxima/ DESTINATION wxMaxima)
else()
    externalproject_add(wxMaxima
        URL "${WXMAXIMA_URL}"
        DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
        URL_MD5 ${WXMAXIMA_MD5}
        DEPENDS wxWidgets
        CONFIGURE_COMMAND ${CMAKE_BINARY_DIR}/wxMaxima-prefix/src/wxMaxima/configure --host=${HOST} --with-wx-config=${CMAKE_BINARY_DIR}/wxWidgets-prefix/src/wxWidgets-build/wx-config --prefix=C:/maxima-${MAXIMAVERSION}
        BUILD_COMMAND $(MAKE)
              COMMAND $(MAKE) allmo
              COMMAND $(MAKE) wxMaxima.win
        INSTALL_COMMAND ""
    )
    install(DIRECTORY ${CMAKE_BINARY_DIR}/wxMaxima-prefix/src/wxMaxima-build/wxMaxima/ DESTINATION wxMaxima)
endif()


if(USE_VTK)
    externalproject_add(vtk
        URL "${VTK_URL}"
        DOWNLOAD_DIR ${CMAKE_SOURCE_DIR}/downloads
        DOWNLOAD_NO_EXTRACT 1
        URL_MD5 ${VTK_MD5}
        CONFIGURE_COMMAND ""
        BUILD_COMMAND ""
        INSTALL_COMMAND ""
    )
    ExternalProject_Add_Step(vtk extract
        COMMENT "Performing extraction step for 'VTK'"
        COMMAND mkdir -p ${CMAKE_BINARY_DIR}/vtk && cd ${CMAKE_BINARY_DIR}/vtk && ${SEVENZIP_EXE} x -y ${CMAKE_SOURCE_DIR}/downloads/vtkpython-${VTKVERSION}-Windows-32bit.exe
        DEPENDEES download
        DEPENDERS patch
    )
    install(DIRECTORY ${CMAKE_BINARY_DIR}/vtk/\$_OUTDIR/bin DESTINATION .)
endif()

# Include DLLs from the current Mingw environment.
install(FILES "${MINGW_LIBSTDCPP}" "${MINGW_LIBGCC}" DESTINATION wxMaxima/)
install(FILES "${MINGW_LIBSTDCPP}" "${MINGW_LIBGCC}" DESTINATION bin/)

install(FILES "licenses.html" DESTINATION share/doc/)


set(CPACK_GENERATOR "NSIS")
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
set(CPACK_PACKAGE_FILE_NAME "maxima-clisp-sbcl-${CPACK_PACKAGE_VERSION}")
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


include(CPack)
