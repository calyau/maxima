# The make target "abcl" compiles Maxima native
# (Java is platform independent) and copies the resulting
# binary-abcl directory to the installer.

find_package(Git)
# Find the path, where the binary-abcl files should be installed, 
# that will either be $PREFIX/lib/maxima/${MAXIMA_VERSION} (if building from a release)
# or include the current "git describe --dirty" output, e.g.
# "lib/maxima/branch_5_42_base_121_g6d10b3b_dirty/"
#
# The CMAKE_SOURCE_DIR is MAXIMA-SOURCE-PATH/crosscompile-windows, 
# so check for "${CMAKE_SOURCE_DIR}/../.git", not "${CMAKE_SOURCE_DIR}/.git"
if(GIT_FOUND AND EXISTS "${CMAKE_SOURCE_DIR}/../.git")
    execute_process(COMMAND ${GIT_EXECUTABLE} describe --dirty
                    WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}"
                    OUTPUT_VARIABLE MAXIMA_VERSION_PATH
                    OUTPUT_STRIP_TRAILING_WHITESPACE)
else()
    set(MAXIMA_VERSION_PATH "${MAXIMA_VERSION}")
endif()

externalproject_add(abcl
  # first copy the Maxima source to a build directory
  DOWNLOAD_COMMAND rsync -a --exclude=crosscompile-windows/build/ "${CMAKE_SOURCE_DIR}/../"  "${CMAKE_BINARY_DIR}/abcl/abcl-prefix/src/abcl"
  BUILD_IN_SOURCE 1
  CONFIGURE_COMMAND sh -c "test -x ./bootstrap && ./bootstrap || return 0"
            COMMAND ./configure --enable-abcl "--with-abcl-jar=${WITH_ABCL_JAR}"
  BUILD_COMMAND LANG=C $(MAKE)
  INSTALL_COMMAND ""
)

install(DIRECTORY "${CMAKE_BINARY_DIR}/abcl/abcl-prefix/src/abcl/src/binary-abcl"
        DESTINATION "lib/maxima/${MAXIMA_VERSION_PATH}/" COMPONENT ABCL)