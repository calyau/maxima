# CMake file for crosscompiling Maxima/wxMaxima for Windows
# SPDX-License-Identifier: GPL-2.0-or-later
# Copyright (C) by Wolfgang Dautermann
# License GPLv2+: GNU GPL version 2 or later <http://gnu.org/licenses/gpl.html>
# This is free software: you are free to change and redistribute it.
# There is NO WARRANTY, to the extent permitted by law.

find_program(WINE_EXE NAMES wine64)

if(WINE_EXE)
    message(STATUS "Found wine at ${WINE_EXE}")
else()
    message(FATAL_ERROR "Wine not found. It is required for crosscompiling Maxima.")
endif()
