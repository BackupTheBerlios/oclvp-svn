## Makefile.am - Use automake to create Makefile.in
#
# This file is part of oclvp
#
# Written and Copyright (c) 2005 by Marcel Kyas
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation; either version 2 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
# 02111-1307, USA.

MOSTLYCLEANFILES	= *~ *.aux *.log
CLEANFILES		= *.cmi *.cmo *.cmx *.cma *.cmxa *.o *.tex \
			  dllxml.so xml.a libxml.a xml-stamp
DISTCLEANFILES		= oclvp.dump oclvp.pdf META
MAINTAINERCLEANFILES	= depend Makefile.in Version.ml

EXTRA_DIST		= depend META.in

BUILT_SOURCES		= META Version.ml

OCAMLPATH		= .
export OCAMLPATH

noinst_LIBRARIES	= libxml_stubs.a

libxml_stubs_a_SOURCES	= xmlr_stubs.c xmlw_stubs.c XmlReader.ml XmlWriter.ml
AM_CPPFLAGS		= $(XML_CPPFLAGS) -I`ocamlc -where`
AM_CFLAGS		= -Wall -Wno-unused -Werror


OCAMLCOPTS		= -I . -I ${srcdir} -linkpkg -package . -dllpath `pwd`
OCAMLOPTOPTS		= -I . -I ${srcdir} -linkpkg -package .

bin_PROGRAMS		= oclvp oclvp.opt

oclvp_SOURCES		= Version.ml ClassDiagram.ml ObjectDiagram.ml \
			  StateMachine.ml OCL.mli OCL.ml SUML.ml XMI.ml \
			  PVSBackEnd.ml Driver.ml

oclvp_opt_SOURCES	= $(oclvp_SOURCES)

all-local: oclvp

oclvp: $(patsubst %.ml, %.cmo, $(patsubst %.mli, %.cmi, $(oclvp_SOURCES)))
	$(OCAMLFIND) ocamlc $(OCAMLCOPTS) $(filter %.cmo, $^) -o $@

oclvp.opt: $(patsubst %.ml, %.cmx, $(patsubst %.mli, %.cmi, $(oclvp_SOURCES)))
	$(OCAMLFIND) ocamlopt $(OCAMLOPTOPTS) $(filter %.cmx, $^) -o $@

.PRECIOUS: *.cmo *.cmi *.cmx *.cma *.cmxa xml-stamp
.ml.cmo:
	$(OCAMLFIND) ocamlc $(OCAMLCOPTS) -c $< -o $@

.mli.cmi:
	$(OCAMLFIND) ocamlc $(OCAMLCOPTS) -c $< -o $@

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(OCAMLOPTOPTS) -c $< -o $@

Version.ml: ${top_srcdir}/configure.ac
	@rm -f Version.ml
	@echo "let oclvp_version = \"${VERSION}\";;" > Version.ml
	@echo "let oclvp_package = \"${PACKAGE}\";;" >> Version.ml
	@echo Wrote $@.

dllxml.so libxml.a xml.cma xml.cmxa: xml-stamp

xml-stamp: $(filter %.cmo, $(patsubst %.ml, %.cmo, $(libxml_stubs_a_SOURCES)))\
	$(filter %.cmx, $(patsubst %.ml, %.cmx, $(libxml_stubs_a_SOURCES))) \
	$(filter %.o, $(patsubst %.c, %.o, $(libxml_stubs_a_SOURCES)))
	$(OCAMLMKLIB) -o xml $^ $(XML_LIBS)

META: $(srcdir)/META.in $(top_srcdir)/configure.ac
	rm -f META && sed -e s/@VERSION\@/$(VERSION)/g < $< > $@

depend: $(oclvp_SOURCES) $(filter $(libxml_stubs_a_SOURCES), %.ml)
	$(OCAMLFIND) ocamldep $^ > $@

.PHONY: ocamldoc
ocamldoc: oclvp.dump oclvp.pdf

oclvp.dump: $(oclvp_SOURCES)
	test -d html || mkdir html
	$(OCAMLDOC) -html -d html -dump $@ -hide XmlReader,XmlWriter -all-params $^

%.pdf: %.tex
	pdflatex $<
	pdflatex $<

oclvp.tex: $(oclvp_SOURCES)
	$(OCAMLWEB) -o $@ $^

distclean-local:
	test ! -d html || rm -rf html

install-hook:
	$(OCAMLFIND) install xml META XmlReader.ml XmlWriter.ml \
		Xml.cma Xml.cmxa libxml.a dllxml.so

oclvp: xml.cma
oclvp.opt: xml.cmxa

XmlReader.cmi XmlWriter.cmi: META

-include ${srcdir}/depend
