
XLISPSTAT = xlispstat

XLISPSTAT = xlispstat

###############################################################################

.SUFFIXES: .lsp .fsl

.lsp.fsl:
	cp $< fsltmp.lsp
	echo '(compile-file "fsltmp.lsp" :output-file "fsltmp.fsl") (exit)' \
	  | ${XLISPSTAT}
	mv fsltmp.fsl $@
	rm fsltmp.lsp

FSLFILES = ./cocoinit.fsl ./cocoapix.fsl \
	./cocofunc.fsl ./cocometh.fsl \
	./independence.fsl ./draggraph.fsl \
	./vertices.fsl ./edges.fsl tests.fsl ./blocks.fsl \
	./assocdiag.fsl ./cocograph.fsl \
	./cocotest.fsl ./cocodyns.fsl \
	./plotcontrols.fsl ./toggle.fsl ./cococontrols.fsl \
	./cocomanager.fsl ./resampling.fsl \
	./cococg.fsl ./cgtest.fsl ./cgblocks.fsl \
	./split.fsl ./graphtotex.fsl ./cocomenu.fsl \
#	./cocokey.fsl ./cocodialogs.fsl ./cocooptions.fsl

all: coco.wks cococg.wks
	${MAKE} coco.wks
	${MAKE} cococg.wks

coco.wks: ${FSLFILES} ./cocokey.lsp ./cocodialogs.lsp ./cocooptions.lsp
	rm -f coco.wks
	echo '(save-workspace "coco") (exit)' | ${XLISPSTAT} ../loadcoco

cococg.wks: ${FSLFILES}
	rm -f cococg.wks
	echo '(save-workspace "cococg") (exit)' | ${XLISPSTAT} ../loadcococg

###############################################################################

subdir = lsp/src

clean:
	@echo "cleaning in $(subdir)"
	@-rm -f *~ *.fsl *.wks

distclean: clean
	@-rm -rf bin Affald ? Work Dialogs Options
