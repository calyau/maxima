SUBDIRS = src lisp-utils tests doc interfaces share demo plotting
EXTRA_DIST = common.mk maxima-local xmaxima-local README.lisps

# Build the Maxima rpms. Optionally, use
#     make rpm SUDO=sudo
# to use sudo to wrap the commands. The user must have sudo permissions
# for rpm and cp.
rpm:
	$(SUDO) cp $(distdir).tar.gz /usr/src/redhat/SOURCES && \
	$(SUDO) rpm -ba maxima.spec