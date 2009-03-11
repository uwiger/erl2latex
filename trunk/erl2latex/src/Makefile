## The MIT License
##
## Copyright (c) 2008 Ulf Wiger <ulf@wiger.net>,
##                    John Hughes <john.hughes@quviq.com>
##
## Permission is hereby granted, free of charge, to any person obtaining a
## copy of this software and associated documentation files (the "Software"),
## to deal in the Software without restriction, including without limitation
## the rights to use, copy, modify, merge, publish, distribute, sublicense,
## and/or sell copies of the Software, and to permit persons to whom the
## Software is furnished to do so, subject to the following conditions:
##
## The above copyright notice and this permission notice shall be included in
## all copies or substantial portions of the Software.
##
## THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
## IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
## FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
## THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
## LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
## FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
## DEALINGS IN THE SOFTWARE.

# FLAGS = -keep_temp

EBINDIR = ../ebin
DOCDIR = ../doc
EOBJEXT = beam

MKPDF = ${PWD}/make_pdf ${FLAGS} -o ../doc
ERLC = erlc -W -o ${EBINDIR} +debug_info

ESRC := erl2latex.erl \
	erl2pdf.erl


EOBJS = $(ESRC:%.erl=$(EBINDIR)/%.$(EOBJEXT))
DOCS = $(ESRC:%.erl=$(DOCDIR)/%.pdf)

all: src doc

src: $(EOBJS)
doc: $(DOCS) \
	make_pdf.pdf \
	make_tex.pdf

pdf:	${MODS:%=%.pdf} \
	make_pdf.pdf \
	make_tex.pdf

clean:
	rm *~ \#*\# $(EBINDIR)/*.beam $(DOCDIR)/*.pdf $(DOCDIR)/*.latex


#.SUFFIXES: .erl .beam .tex .pdf

$(EBINDIR)/%.$(EOBJEXT): %.erl
	$(ERLC) $<

$(DOCDIR)/make_pdf.pdf: make_pdf
	$(MKPDF) make_pdf

$(DOCDIR)/make_latex.pdf: make_latex
	$(MKPDF) make_latex

$(DOCDIR)/%.pdf: %.erl
	$(MKPDF) $<

.erl.beam:
	$(ERLC) $<
#	erlc -W -o ../ebin $<

.erl.tex:
	${PWD}/make_tex -o ../doc $<

.erl.pdf:
	${MKPDF} $<

MODS :=	erl2latex \
	erl2pdf


#tex:	${MODS:%=%.tex} \
#	make_tex




make_pdf.pdf: make_pdf erl2pdf.erl erl2latex.erl
	${MKPDF} make_pdf
make_tex.pdf: make_tex erl2pdf.erl erl2latex.erl
	${MKPDF} make_tex

