include $(top_srcdir)/common.mk
genericdir =$(sharedir)

sharefiles = $(shell cd $(srcdir);find * -type f | \
egrep -v '\.ERR$$' | \
egrep -v 'CVS|Makefile(\.in|\.am)?|^\./Makefile|\.gitignore|\.gitattributes' | \
egrep -v '/fortran/' | \
egrep -v 'colnew/ex./' | \
egrep -v 'lbfgs/.*\.f' | \
egrep -v '/\.\#' | \
egrep -v '~|\#' | \
sed -e 's%\./%%' | \
sort -f -d | \
sed '$$s/\\$$//' )


genericdirDATA = \
$(sharefiles)

EXTRA_DIST = $(genericdirDATA)

SUBDIRS = contrib logic draw
