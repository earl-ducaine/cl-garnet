.PHONY: clean

all: html pdf info

clean:
	rm -rf include
	rm -f  *.pdf *.html *.info
	rm -f *.aux *.cp *.fn *.fns *.ky *.log *.pg *.toc *.tp *.tps *.vr
	rm -f include-stamp

clx-truetype.texinfo:


include-stamp: ../*.lisp ../*.asd
	sbcl \
	--eval '(let ((asdf:*central-registry* (cons #p"../" (cons #p"/home/michael/webspace/sb-texinfo/" asdf:*central-registry*)))) (require :sb-texinfo) (require :clx-truetype))' \
	--eval '(sb-texinfo:generate-includes "include/" (list (find-package :clx-truetype)) :base-package :clx-truetype)' \
	--eval '(quit)'
	touch include-stamp

%.html: clx-truetype.texinfo style.css include-stamp
	makeinfo --html --no-split --css-include=style.css $<

%.pdf: clx-truetype.texinfo include-stamp
	texi2dvi -p $<

%.info: clx-truetype.texinfo include-stamp
	makeinfo --no-split $<

html: clx-truetype.html

pdf: clx-truetype.pdf

info: clx-truetype.info
