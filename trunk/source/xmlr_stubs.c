/* ocaml-xmlr - OCaml bindings for libxml's xmlreader.
 * Copyright (C) 2004  Evan Martin <martine@danga.com>
 */

/*
 * xmlr_stubs.c -- OCaml bindings for libxml's XmlReader.
 *
 * This file has been modified and adapted for use in oclvp
 * by Marcel Kyas <mkyas@users.berlios.de> on November 17, 2005.
 *
 * Contrary to the other files of oclvp, the following license applies to
 * the content of this file and XmlReader.ml.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS OF THE
 * SOFTWARE.
 *
 * Except as contained in this notice, the names of the authors shall not
 * be used in advertising or otherwise to promote the sale, use or other
 * dealings in this Software without prior written authorisation of the
 * authors.
 */

#include <libxml/xmlreader.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>





#define XmlReader_val(v) (*(xmlTextReaderPtr*)Data_custom_val(v))





static void xml_reader_finalize(value v) {
	xmlTextReaderClose(XmlReader_val(v));
	/* XXX: api docs say this can fail. */
}





static struct custom_operations xmlr_custom_operations = {
	.identifier  = "de.berlios.oclvp.xmlreader.1",
	.finalize    = xml_reader_finalize,
	.compare     = custom_compare_default,
	.hash        = custom_hash_default,
	.serialize   = custom_serialize_default,
	.deserialize = custom_deserialize_default,
};





static int
load_opts(value vopts) {
	CAMLparam1(vopts);
	CAMLlocal1(vlist);
	int opts = 0;
	
	if (!Is_long(vopts)) /* None */
		for (vlist = Field(vopts, 0); Is_block(vlist); vlist = Field(vlist, 1))
			opts |= 1 << Int_val(Field(vlist, 0));

	CAMLreturn(Val_int(opts));
}





static value
xml_reader_new(xmlTextReaderPtr reader) {
	CAMLparam0();
	CAMLlocal1(vreader);
	vreader = alloc_custom(&xmlr_custom_operations, 4, 0, 1);
	Field(vreader, 1) = (value)reader;
	CAMLreturn(vreader);
}





static char*
string_opt(value opt) {
	CAMLparam1(opt);
	if (Is_long(opt))    /* None */
		CAMLreturn(NULL);
	else                 /* Some x */
		CAMLreturn(String_val(Field(opt, 0)));
}





CAMLprim value
xml_reader_from_filename(value vencoding, value vopts, value filename) {
	CAMLparam3(vencoding, vopts, filename);
	xmlTextReaderPtr reader;
	
	reader = xmlReaderForFile(String_val(filename), string_opt(vencoding), load_opts(vopts));

	CAMLreturn(xml_reader_new(reader));
}





CAMLprim value
xml_reader_from_string(value baseurl, value encoding, value opts, value str) {
	CAMLparam4(baseurl, encoding, opts, str);
	xmlTextReaderPtr reader;
	
	/* XXX we probably need to hold a reference to the string? */
	reader = xmlReaderForMemory(String_val(str), string_length(str),
			string_opt(baseurl), string_opt(encoding), load_opts(opts));

	CAMLreturn(xml_reader_new(reader));
}





CAMLprim value
xml_reader_read(value reader) {
	CAMLparam1(reader);
	int ret;
	
	ret = xmlTextReaderRead(XmlReader_val(reader));
	if (ret < 0)
		failwith("xmlTextReaderRead");
	CAMLreturn(Val_int(ret));
}





/*** current node ***/

CAMLprim value
xml_reader_nodetype(value reader) {
	CAMLparam1(reader);
	int ret;
	
	ret = xmlTextReaderNodeType(XmlReader_val(reader));
	if (ret < 0)
		failwith("xmlTextReaderNodeType");
	CAMLreturn(Val_int(ret));
}





CAMLprim value
xml_reader_name(value reader) {
	CAMLparam1(reader);
	const xmlChar *ret;
	
	ret = xmlTextReaderConstName(XmlReader_val(reader));
	CAMLreturn(copy_string((char*) ret));
}





CAMLprim value
xml_reader_has_value(value reader) {
	CAMLparam1(reader);
	int ret;
	
	ret = xmlTextReaderHasValue(XmlReader_val(reader));
	if (ret < 0)
		failwith("xmlTextReaderHasValue");
	CAMLreturn(Val_int(ret));
}





CAMLprim value
xml_reader_value(value reader) {
	CAMLparam1(reader);
	const xmlChar *ret;
	
	ret = xmlTextReaderConstValue(XmlReader_val(reader));
	if (ret == NULL)
		failwith("xmlTextReaderConstValue");
	CAMLreturn(copy_string((char*) ret));
}





CAMLprim value
xml_reader_base_uri(value reader) {
	CAMLparam1(reader);
	const xmlChar *ret;
	
	ret = xmlTextReaderBaseUri(XmlReader_val(reader));
	if (ret == NULL)
		failwith("xmlTextReaderBaseUri");
	CAMLreturn(copy_string((char*) ret));
}





CAMLprim value
xml_reader_local_name(value reader) {
	CAMLparam1(reader);
	const xmlChar *ret;
	
	ret = xmlTextReaderConstLocalName(XmlReader_val(reader));
	if (ret == NULL)
		failwith("xmlTextReaderConstLocalName");
	CAMLreturn(copy_string((char*) ret));
}





CAMLprim value
xml_reader_namespace_uri(value reader) {
	CAMLparam1(reader);
	const xmlChar *ret;
	
	ret = xmlTextReaderConstNamespaceUri(XmlReader_val(reader));
	if (ret == NULL)
		failwith("xmlTextReaderConstNamespaceUri");
	CAMLreturn(copy_string((char*) ret));
}





CAMLprim value
xml_reader_prefix(value reader) {
	CAMLparam1(reader);
	const xmlChar *ret;
	
	ret = xmlTextReaderConstPrefix(XmlReader_val(reader));
	if (ret == NULL)
		failwith("xmlTextReaderConstPrefix");
	CAMLreturn(copy_string((char*) ret));
}





CAMLprim value
xml_reader_is_empty_element(value reader) {
	CAMLparam1(reader);
	int ret;
	
	ret = xmlTextReaderIsEmptyElement(XmlReader_val(reader));
	if (ret < 0)
		failwith("xmlTextReaderIsEmptyElement");
	CAMLreturn(Val_int(ret));
}





CAMLprim value
xml_reader_depth(value reader) {
	CAMLparam1(reader);
	int ret;
	
	ret = xmlTextReaderDepth(XmlReader_val(reader));
	if (ret < 0)
		failwith("xmlTextReaderDepth");
	CAMLreturn(Val_int(ret));
}




#if 0
/* XXX doesn't work--unimplemented in libxml itself */
CAMLprim value
xml_reader_inner_xml(value reader) {
	CAMLparam1(reader);
	CAMLlocal1(cret);
	xmlChar* ret;
	
	ret = xmlTextReaderReadInnerXml(XmlReader_val(reader));
	if (ret == NULL)
		failwith("xmlTextReaderReadInnerXml");
	cret = copy_string(ret);
	xmlFree(ret);
	CAMLreturn(cret);
}





CAMLprim value
xml_reader_outer_xml(value reader) {
	CAMLparam1(reader);
	CAMLlocal1(cret);
	xmlChar* ret;
	
	ret = xmlTextReaderReadOuterXml(XmlReader_val(reader));
	if (ret == NULL)
		failwith("xmlTextReaderReadOuterXml");
	cret = copy_string(ret);
	xmlFree(ret);
	CAMLreturn(cret);
}
#endif





CAMLprim value
xml_reader_has_attributes(value reader) {
	CAMLparam1(reader);
	int ret;
	
	ret = xmlTextReaderHasAttributes(XmlReader_val(reader));
	if (ret < 0)
		failwith("xmlTextReaderHasAttributes");
	CAMLreturn(Val_int(ret));
}





CAMLprim value
xml_reader_attribute_count(value reader) {
	CAMLparam1(reader);
	int ret;
	
	ret = xmlTextReaderAttributeCount(XmlReader_val(reader));
	if (ret < 0)
		failwith("xmlTextReaderAttributeCount");
	CAMLreturn(Val_int(ret));
}





CAMLprim value
xml_reader_get_attribute(value reader, value name)
{
	CAMLparam2(reader, name);
	CAMLlocal1(cret);
	xmlChar *ret;
	
	ret = xmlTextReaderGetAttribute(XmlReader_val(reader),
					BAD_CAST String_val(name));
	if (ret == NULL)
		raise_not_found();
	cret = copy_string((char*) ret);
	xmlFree(ret);
	CAMLreturn(cret);
}





CAMLprim value
xml_reader_get_attribute_no(value reader, value no)
{
	CAMLparam2(reader, no);
	CAMLlocal1(cret);
	xmlChar *ret;
	
	ret = xmlTextReaderGetAttributeNo(XmlReader_val(reader), Int_val(no));
	if (ret == NULL)
		raise_not_found();
	cret = copy_string((char*) ret);
	xmlFree(ret);
	CAMLreturn(cret);
}





CAMLprim value
xml_reader_get_attribute_ns(value reader, value name, value ns)
{
	CAMLparam3(reader, name, ns);
	CAMLlocal1(cret);
	xmlChar *ret;
	
	ret = xmlTextReaderGetAttributeNs(XmlReader_val(reader),
					  BAD_CAST String_val(name),
					  BAD_CAST String_val(ns));
	if (ret == NULL)
		raise_not_found();
	cret = copy_string((char*) ret);
	xmlFree(ret);
	CAMLreturn(cret);
}
