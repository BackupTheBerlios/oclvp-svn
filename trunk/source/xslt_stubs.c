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


#define XsltStylesheet_val(v) (*(xsltStylesheetPtr*) Data_custom_val(v))

static void
xslt_stylesheet_finalize(value v)
{
        xsltFreeStylesheet(XsltStylesheet_val(v));
}





static struct custom_operations xslt_stylesheet_custom_operations = {
        .identifier = "de.berlios.oclvp.xsltstylesheet.1",
        .finalize = xslt_stylesheet_finalize,
        .compare = custom_compare_default,
        .hash = custom_hash_default,
        .serialize = custom_serialize_default,
        .deserialize = custom_deserialize_default
};





static value
xslt_stylesheet_new(xsltStylesheetPtr arg)
{
        CAMLparam0();
        CAMLlocal1(res);
        res = caml_alloc_custom(&xslt_stylesheet_custom_operations, 4, 0, 1);
        Field(res, 1) = (value) arg;
        CAMLreturn(res);
}




static void
xslt_transform_ctxt_finalize(value v)
{
        xsltFreeTransformContext(XsltTransformCtxt_val(v));
}





static struct custom_operations xslt_transform_ctxt_custom_operations = {
        .identifier = "de.berlios.oclvp.xslttransformcontext.1",
        .finalize = xslt_transform_ctxt_finalize,
        .compare = custom_compare_default,
        .hash = custom_hash_default,
        .serialize = custom_serialize_default,
        .deserialize = custom_deserialize_default
};





static value
xslt_transform_ctxt_new(xsltTransformContextPtr arg)
{
        CAMLparam0();
        CAMLlocal1(res);
        res = caml_alloc_custom(&xslt_transform_ctxt_custom_operations, 4, 0,
				1);
        Field(res, 1) = (value) arg;
        CAMLreturn(res);
}



CAMLprim value
xslt_parse_stylesheet_doc(value doc)
{
	CAMLparam1(doc);
	xsltStylesheetPtr style;

	style = xsltParseStylesheetDoc(XmlDoc_val(doc));
	if (style == NULL || style->errors != 0) {
		if (style != NULL)
			xsltFreeStylesheet(style);
		caml_failwith("xsltParseStylesheetDoc");
	}
	CAMLreturn (xslt_stylesheet_new(style));
}



CAMLprim value
xslt_transform(value stylesheet, value doc, value target)
{
	CAMLparam3(stylesheet, doc, target);
	xsltTransformContextPtr ctxt;
	xmlDocPtr result;

	ctxt = xsltNewTransformContext(XsltStylesheet_val(stylesheet),
				       XmlDoc_val(doc));
	if (ctxt == NULL) {
		caml_failwith("xmlNewTransformContext");
	}
	result = xsltApplyStylesheetUser(XsltStylesheet_val(stylesheet),
					 XmlDoc_val(doc),
					 NULL, NULL, NULL, ctxt);
	if (ctxt->state == XSLT_STATE_ERROR) {
		caml_failwith("Transformation had errors");
	} else if (ctxt->state == XSLT_STATE_STOPPED) {
		caml_failwith("Transformation stopped.");
	} else if (XmlDoc_val(result)->URL == NULL ) {
		XmlDoc_val(result)->URL = xmlStrdup(XmlDoc_val(doc)->URL);
	}
	CAMLreturn(xml_doc_new(result));
}
