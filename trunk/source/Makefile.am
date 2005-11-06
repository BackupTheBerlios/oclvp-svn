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

MOSTLYCLEANFILES	= *~
CLEANFILES		= *.cmi *.cmo *.cmx *.o
DISTCLEANFILES		= depends Version.ml
MAINTAINERCLEANFILES	= Makefile.in

OCAMLCOPTS		= -package xmlr -linkpkg -dllpath-all
OCAMLOPTOPTS		= -package xmlr -linkpkg

bin_PROGRAMS	= oclvp oclvp.opt

BUILT_SOURCES		= Version.ml

oclvp_SOURCES		= Version.ml ClassDiagram.ml ObjectDiagram.ml \
			  StateMachine.ml OCL.ml SUML.ml XMI.ml PVSBackEnd.ml \
			  Driver.ml

oclvp_opt_SOURCES	= $(oclvp_SOURCES)

all-local: oclvp

oclvp: $(oclvp_SOURCES:.ml=.cmo)
	$(OCAMLFIND) ocamlc $(OCAMLCOPTS) $^ -o $@

oclvp.opt: $(oclvp_opt_SOURCES:.ml=.cmx)
	$(OCAMLFIND) ocamlopt $(OCAMLOPTOPTS) $^ -o $@

.PRECIOUS: *.cmo *.cmi *.cmx
%.cmo %.cmi: %.ml
	$(OCAMLFIND) ocamlc $(OCAMLCOPTS) -c $<

%.cmx %.cmxi: %.ml
	$(OCAMLFIND) ocamlopt $(OCAMLOPTOPTS) -c $<

Version.ml: Makefile
	@rm -f Version.ml
	@echo "let oclvp_version = \"${VERSION}\";;" > Version.ml
	@echo "let oclvp_package = \"${PACKAGE}\";;" >> Version.ml
	@echo Wrote $@.

depends: $(oclvp_SOURCES) Makefile
	$(OCAMLFIND) ocamldep $(oclvp_SOURCES) > $@

-include depends
