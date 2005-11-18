/*
 * xslt_stubs.c -- OCaml bindings for libxslt.
 *
 * This file is part of oclvp
 *
 * Written and Copyright (c) 2005 by Marcel Kyas <mkyas@users.berlios.de>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 */

#include "xml_helpers.h"

#include <libxslt/xsltconfig.h>
#include <libxslt/xslt.h>
#include <libxslt/xsltInternals.h>
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>
#include <libxslt/extensions.h>

CAMLprim value
xml_transform(value stylesheet, value source, value target)
{
	CAMLparam3(stylesheet, source, target);
	xsltStylesheetPtr style;
	xmlDocPtr xstyle, doc, result;
	xsltTransformContextPtr ctxt;

	xmlLineNumbersDefault(0);
	xmlKeepBlanksDefault(0);
	xmlSubstituteEntitiesDefault(1);
	xmlLoadExtDtdDefaultValue = 0;

	xsltRegisterTestModule();

	xmlDefaultSAXHandlerInit();
	xmlDefaultSAXHandler.cdataBlock = NULL;

	/* Load the style sheet */

	xstyle = xmlParseFile(String_val(stylesheet));
	if (xstyle == NULL) {
		caml_failwith("xmlParseFile(stylesheet)");
	}

	style = xsltParseStylesheetDoc(xstyle);
	if (style == NULL || style->errors != 0) {
		if (style != NULL)
			xsltFreeStylesheet(style);
		else
			xmlFreeDoc(xstyle);
		caml_failwith("xsltParseStylesheetDoc");
	}

	/* Load the document. */
	doc = xmlParseFile(String_val(source));
	if (doc == NULL) {
		xsltFreeStylesheet(style);
		caml_failwith("xmlParseFile(source)");
	}

	ctxt = xsltNewTransformContext(style, doc);
	if (ctxt == NULL) {
		xsltFreeStylesheet(style);
		xmlFreeDoc(doc);
		caml_failwith("xmlNewTransformContext");
	}
	result = xsltApplyStylesheetUser(style, doc, NULL, NULL, NULL, ctxt);
	xmlFreeDoc(doc);
	if (ctxt->state == XSLT_STATE_ERROR) {
		xsltFreeTransformContext(ctxt);
		xsltFreeStylesheet(style);
		if (result != NULL)
			xmlFreeDoc(result);
		caml_failwith("Transformation had errors");
	} else if (ctxt->state == XSLT_STATE_STOPPED) {
		xsltFreeTransformContext(ctxt);
		xsltFreeStylesheet(style);
		if (result != NULL)
			xmlFreeDoc(result);
		caml_failwith("Transformation stopped.");
	} else if (result->URL == NULL ) {
		result->URL = xmlStrdup(doc->URL);
	}
	xsltFreeTransformContext(ctxt);
	xsltFreeStylesheet(style);

	/* Save the document */
	do {
		int old = xmlIndentTreeOutput;

		xmlIndentTreeOutput = 1;
		xmlSaveFormatFile(String_val(target), result, 1);
		xmlIndentTreeOutput = old;
	} while (0);

	xmlFreeDoc(result);

	CAMLreturn(Val_unit);
}
