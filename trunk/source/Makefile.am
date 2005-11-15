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
CLEANFILES		= *.cmi *.cmo *.cmx *.o *.tex
DISTCLEANFILES		= oclvp.pdf
MAINTAINERCLEANFILES	= depend Makefile.in Version.ml

EXTRA_DIST		= depend

OCAMLCOPTS		= -g -I . -I ${srcdir} -package xmlr -linkpkg \
			  -dllpath-all
OCAMLOPTOPTS		= -I . -I ${srcdir} -package xmlr -linkpkg

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

.PRECIOUS: *.cmo *.cmi *.cmx
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

depend: $(oclvp_SOURCES)
	$(OCAMLFIND) ocamldep $(oclvp_SOURCES) > $@

.PHONY: ocamldoc
ocamldoc: oclvp.dump oclvp.pdf

oclvp.dump: $(oclvp_SOURCES)
	test -d html || mkdir html
	$(OCAMLDOC) -html -d html -dump $@ -hide Xmlr -all-params $^

%.pdf: %.tex
	pdflatex $<
	pdflatex $<

oclvp.tex: $(oclvp_SOURCES)
	$(OCAMLWEB) -o $@ $^

distclean-local:
	test ! -d html || rm -rf html
	rm -f oclvp.dump

-include ${srcdir}/depend
