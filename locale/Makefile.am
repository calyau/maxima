TRANSLATIONS =

localedir = $(datadir)/locale

install-data-local: install-maxima-catalogs

install-maxima-catalogs:
	for i in $(TRANSLATIONS) ; do \
		$(mkinstalldirs) $(DESTDIR)$(localedir)/$$i/LC_MESSAGES ; \
		$(INSTALL_DATA) $$i.mo $(DESTDIR)$(localedir)/$$i/LC_MESSAGES/maxima.mo ; \
	done

# the programs we use (TODO: use configure to detect them)
MSGFMT=msgfmt --verbose
MSGMERGE=msgmerge
XGETTEXT=xgettext
XARGS=xargs

XGETTEXT_ARGS=-s -c -j --language=Lisp

%.mo: %.po
	$(MSGFMT) -o $@ $<

%.po: maxima.pot
	touch $@
	$(MSGMERGE) $@ maxima.pot > $@.new && mv $@.new $@;

maxima.pot:
	touch $@
	find ../src -name "*.lisp" | $(XARGS) $(XGETTEXT) $(XGETTEXT_ARGS) -o maxima.pot

allpo: maxima.pot
	@for t in $(TRANSLATIONS); do $(MAKE) $$t.po; done

allmo: force-update allpo
	@for t in $(TRANSLATIONS); do $(MAKE) $$t.mo; done

force-update:
	$(RM) maxima.pot

all: allmo
