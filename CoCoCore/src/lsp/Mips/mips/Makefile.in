
XLISPSTAT = xlispstat

###############################################################################

.SUFFIXES: .lsp .fsl

.lsp.fsl:
	cp $< fsltmp.lsp
	echo '(compile-file "fsltmp.lsp" :output-file "fsltmp.fsl") (exit)' \
	  | ${XLISPSTAT}
	mv fsltmp.fsl $@
	rm fsltmp.lsp

FSLFILES = ./mips.fsl ./mipstest.fsl ./mipsblocks.fsl

all: ${FSLFILES} mips.wks

mips.wks: ${FSLFILES}
	rm -f mips.wks
	echo '(save-workspace "mips") (exit)' | ${XLISPSTAT} ../loadmips

###############################################################################

subdir = lsp/mips

clean:
	@echo "cleaning in $(subdir)"
	@-rm -f *~ *.fsl *.wks

distclean: clean
	@-rm -rf Affald ? Work

