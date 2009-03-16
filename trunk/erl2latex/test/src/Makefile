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

.SUFFIXES: .erl .beam .tex .pdf .escript

vpath %.beam ../../ebin
vpath %.pdf ../doc

SRC = ../../src
EBIN == ../../ebin

MKPDF = $(SRC)/make_pdf.escript ${FLAGS} -o ../doc
ERLC = erlc -W -o ../ebin +debug_info

epdfs = t1.pdf

spdfs = 

pdfs = $(epdfs) $(spdfs)

beams = t1.beam

all : $(beams) $(epdfs) $(spdfs)

clean :
	rm -f *~ #*# 
	rm -f ../ebin/*.beam
	rm -f ../doc/*.pdf ../doc/*.tex

$(beams) : %.beam : %.erl
	$(ERLC) $<

$(epdfs) : %.pdf : %.erl
	$(MKPDF) $<

$(spdfs) : %.pdf : %.escript
	$(MKPDF) $<

$(pdfs) : $(SRC)/erl2latex.erl $(SRC)/erl2pdf.erl