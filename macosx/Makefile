APPDIR = $(HOME)/Desktop
VERSION = 5.24.0
LISP_PROGRAM = /usr/local/bin/sbcl
LISP_NAME = sbcl

MAXIMA_APP = $(APPDIR)/Maxima.app
APPRESOURCES = $(MAXIMA_APP)/Contents/Resources
PREFIX = $(APPRESOURCES)/maxima
BINDIR = $(PREFIX)/bin 

all:
	./configure --with-$(LISP_NAME) --prefix=$(PREFIX)
	make
	platypus -F -y -a Maxima                      \
                 -c macosx/script                     \
                 -p /bin/sh                           \
                 -o None                              \
                 -i interfaces/xmaxima/maxima-new.png \
                 -V $(VERSION)                        \
                 -u "Maxima Development Team"         \
                 -I net.sourceforge.maxima            \
                 $(MAXIMA_APP)
	make install
	install -c macosx/maxima.sh $(APPRESOURCES)
	install -c $(LISP_PROGRAM) $(BINDIR)
