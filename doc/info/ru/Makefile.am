MAKEINFOFLAGS = --enable-encoding -I . -I .. -I ../.. -I $(srcdir)/..

TEXINFO_TEX = ../texinfo.tex

lang = ru
langsdir = /ru

# The Russian manual is currently distributed as source but not built.
# It includes untranslated English sections whose dependency graph no
# longer matches the Russian Texinfo include stack.
#include ../common-lang.mk
EXTRA_DIST = $(wildcard *.texi)

maxima_TEXINFOS = $(wildcard *.texi)
EXTRA_DIST += include-maxima.texi.in
