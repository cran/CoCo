
all:
	(cd src ; make )
#	(cd mips ; make )

SUBDIRS = src mips Extensions
subdir = lsp

distclean:
	@(for d in $(SUBDIRS); do rsd="$${d} $${rsd}"; done; \
		for d in $${rsd}; do (cd $${d} && $(MAKE) $@); done)
	@rm -rf Affald *~ ? bin/?
