include $(top_srcdir)/common.mk
genericdir =$(sharedir)

sharefiles = $(shell cd $(srcdir);find * -type f | \
grep -E -v '\.ERR$$' | \
grep -E -v 'CVS|Makefile(\.in|\.am)?|^\./Makefile|\.gitignore|\.gitattributes' | \
grep -E -v '/fortran/' | \
grep -E -v 'colnew/ex./' | \
grep -E -v 'lbfgs/.*\.f' | \
grep -E -v '/\.'\# | \
grep -E -v '~|'\# | \
sed -e 's%\./%%' | \
sort -f -d | \
sed '$$s/\\$$//' )


genericdirDATA = \
$(sharefiles)

EXTRA_DIST = $(genericdirDATA)

SUBDIRS = contrib logic draw nelder_mead raddenest
