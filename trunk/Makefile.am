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

SUBDIRS			= info share source tests
EXTRA_DIST		= oclvp.spec

MOSTLYCLEANFILES	=
CLEANFILES		=
DISTCLEANFILES		=
MAINTAINERCLEANFILES	= aclocal.m4 configure install-sh \
			  Makefile.in missing oclvp.spec

oclvp.spec: $(srcdir)/oclvp.spec.in $(srcdir)/configure.ac
	sed -e s/@VERSION\@/$(VERSION)/g \
	    -e s/@PKGCONFIG_VERSION\@/$(PKGCONFIG_VERSION)/g < $< > $@
