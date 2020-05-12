P = /usr/amickish/doc
VPATH = $(P)

# Break the circularity of dependence between the manuals and the
# pagenumbers file by not making the pagenumbers depend on the Hints

OUTPUT_MINUS_HINTS = overview/overview.ps apps/apps.ps tour/tour.ps \
        tutorial/tutorial.ps \
	kr/kr-manual.ps opal/opal-manual.ps inter/inter-manual.ps \
	aggregadgets/aggregadgets-manual.ps gadgets/gadgets-manual.ps \
	debug/debug-manual.ps demos/demoguide.ps \
	sampleprog/sampleprog.ps gilt/gilt-manual.ps c32/c32-manual.ps\
        lapidary/lapidary-manual.ps

OUTPUT = $(OUTPUT_MINUS_HINTS) hints/hints.ps 


manuals: $(OUTPUT)

all: $(OUTPUT) scripts/pagenumbers-done index/index.ps refman/refman.ps

pagenumbers.mss: # scripts/pagenumbers-done

scripts/pagenumbers-done: # $(OUTPUT_MINUS_HINTS)
	# scripts/create-pages $(OUTPUT)
	# cat /dev/null > scripts/pagenumbers-done

refman/refman.ps: refman/refman.mss garnet.lib pagenumbers.mss
	scribe refman/refman
	mv refman.ps refman

overview/overview.ps: overview/overview.mss garnet.lib pagenumbers.mss
	- mv overview/overview.aux overview.aux
	scribe overview/overview -v
	mv overview.* overview
	spell overview/overview.lex > overview/overview.mis

apps/apps.ps: apps/apps.mss garnet.lib pagenumbers.mss
	scribe apps/apps
	mv apps.ps apps

tour/tour.ps: tour/tour.mss garnet.lib pagenumbers.mss
	- mv tour/tour.aux tour.aux
	scribe tour/tour
	mv tour.* tour

tutorial/tutorial.ps: tutorial/tutorial.mss garnet.lib pagenumbers.mss
	- mv tutorial/tutorial.aux tutorial.aux
	scribe tutorial/tutorial
	mv tutorial.* tutorial

kr/kr-manual.ps: kr/kr-manual.mss garnet.lib pagenumbers.mss
	- mv kr/kr-manual.aux kr-manual.aux
	scribe kr/kr-manual
	mv kr-manual.* kr

opal/opal-manual.ps: opal/opal-manual.mss garnet.lib pagenumbers.mss
	- mv opal/opal-manual.aux opal-manual.aux
	scribe opal/opal-manual
	mv opal-manual.* opal

inter/inter-manual.ps: inter/inter-manual.mss garnet.lib pagenumbers.mss
	- mv inter/inter-manual.aux inter-manual.aux
	scribe inter/inter-manual
	mv inter-manual.* inter

aggregadgets/aggregadgets-manual.ps: aggregadgets/aggregadgets-manual.mss \
  garnet.lib pagenumbers.mss
	- mv aggregadgets/aggregadgets-manual.aux aggregadgets-manual.aux
	scribe aggregadgets/aggregadgets-manual
	mv aggregadgets-manual.* aggregadgets

gadgets/gadgets-manual.ps: gadgets/gadgets-manual.mss garnet.lib pagenumbers.mss
	- mv gadgets/gadgets-manual.aux gadgets-manual.aux
	scribe gadgets/gadgets-manual
	mv gadgets-manual.* gadgets

debug/debug-manual.ps: debug/debug-manual.mss garnet.lib pagenumbers.mss
	- mv debug/debug-manual.aux debug-manual.aux
	scribe debug/debug-manual
	mv debug-manual.* debug

demos/demoguide.ps: demos/demoguide.mss garnet.lib pagenumbers.mss
	- mv demos/demoguide.aux demoguide.aux
	scribe demos/demoguide
	mv demoguide.* demos

sampleprog/sampleprog.ps: sampleprog/sampleprog.mss garnet.lib pagenumbers.mss
	- mv sampleprog/sampleprog.aux sampleprog.aux
	scribe sampleprog/sampleprog
	mv sampleprog.* sampleprog

gilt/gilt-manual.ps: gilt/gilt-manual.mss garnet.lib pagenumbers.mss
	- mv gilt/gilt-manual.aux gilt-manual.aux
	scribe gilt/gilt-manual
	mv gilt-manual.* gilt

c32/c32-manual.ps: c32/c32-manual.mss garnet.lib pagenumbers.mss
	- mv c32/c32-manual.aux c32-manual.aux
	scribe c32/c32-manual
	mv c32-manual.* c32

lapidary/lapidary-manual.ps: lapidary/lapidary-manual.mss garnet.lib pagenumbers.mss
	- mv lapidary/lapidary-manual.aux lapidary-manual.aux
	scribe lapidary/lapidary-manual
	mv lapidary-manual.* lapidary

hints/hints.ps: hints/hints.mss garnet.lib pagenumbers.mss
	scribe hints/hints
	mv hints.* hints

index/index.ps: $(OUTPUT)
	@scripts/get-indices $(OUTPUT)
	scribe index/index
	awk -f scripts/make-index.awk index.ps > index/index.ps
	rm index.*

clean:
	/bin/rm -f aggregadgets/aggregadgets-manual.ps
	/bin/rm -f debug/debug-manual.ps
	/bin/rm -f demos/demoguide.ps
	/bin/rm -f gadgets/gadgets-manual.ps
	/bin/rm -f gilt/gilt-manual.ps
	/bin/rm -f c32/c32-manual.ps
	/bin/rm -f inter/inter-manual.ps
	/bin/rm -f kr/kr-manual.ps
	/bin/rm -f opal/opal-manual.ps
	/bin/rm -f overview/overview.ps
	/bin/rm -f apps/apps.ps
	/bin/rm -f refman.ps
	/bin/rm -f refman/refman.ps
	/bin/rm -f sampleprog/sampleprog.ps
	/bin/rm -f tour/tour.ps
	/bin/rm -f tutorial/tutorial.ps
	/bin/rm -f lapidary/lapidary-manual.ps
	/bin/rm -f hints/hints.ps
