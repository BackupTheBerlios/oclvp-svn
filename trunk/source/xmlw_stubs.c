/*
 * xmlw_stubs.c -- OCaml bindings for libxml's xmlwriter.
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

#include <libxml/xmlwriter.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/custom.h>





/* Cast a custom block to a xmlTextWriter pointer. */
#define XmlWriter_val(v) (*((xmlTextWriterPtr*)Data_custom_val(v)))





static void xml_writer_finalize(value v)
{
	xmlFreeTextWriter(XmlWriter_val(v));
	/* XXX: This may fail, but we ignore this. */
}





static struct custom_operations xmlw_custom_operations = {
	.identifier = "de.berlios.oclvp.xmlwriter.1",
	.finalize = xml_writer_finalize,
	.compare = custom_compare_default,
	.hash = custom_hash_default,
	.serialize = custom_serialize_default,
	.deserialize = custom_deserialize_default
};





static value
xml_writer_wrap(xmlTextWriterPtr writer)
{
	CAMLparam0();
	CAMLlocal1(vwriter);
	vwriter = alloc_custom(&xmlw_custom_operations, 4, 0, 1);
	Field(vwriter, 1) = (value)writer;
	CAMLreturn(vwriter);
}





CAMLprim value
xml_writer_to_file(value baseuri, value compression)
{
	CAMLparam2(baseuri, compression);
	xmlTextWriterPtr writer;

	writer = xmlNewTextWriterFilename(String_val(baseuri),
				          Int_val(compression));
	CAMLreturn (xml_writer_wrap(writer));
}




CAMLprim value
xml_writer_end_attribute(value writer)
{
	CAMLparam1(writer);
	int ret;

	ret = xmlTextWriterEndAttribute(XmlWriter_val(writer));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_end_cdata(value writer)
{
	CAMLparam1(writer);
	int ret;

	ret = xmlTextWriterEndCDATA(XmlWriter_val(writer));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_end_comment(value writer)
{
	CAMLparam1(writer);
	int ret;

	ret = xmlTextWriterEndComment(XmlWriter_val(writer));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_end_dtd(value writer)
{
	CAMLparam1(writer);
	int ret;

	ret = xmlTextWriterEndDTD(XmlWriter_val(writer));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_end_dtd_attlist(value writer)
{
	CAMLparam1(writer);
	int ret;

	ret = xmlTextWriterEndDTDAttlist(XmlWriter_val(writer));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_end_dtd_element(value writer)
{
	CAMLparam1(writer);
	int ret;

	ret = xmlTextWriterEndDTDElement(XmlWriter_val(writer));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_end_dtd_entity(value writer)
{
	CAMLparam1(writer);
	int ret;

	ret = xmlTextWriterEndDTDEntity(XmlWriter_val(writer));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_end_document(value writer)
{
	CAMLparam1(writer);
	int ret;

	ret = xmlTextWriterEndDocument(XmlWriter_val(writer));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_end_element(value writer)
{
	CAMLparam1(writer);
	int ret;

	ret = xmlTextWriterEndElement(XmlWriter_val(writer));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_end_pi(value writer)
{
	CAMLparam1(writer);
	int ret;

	ret = xmlTextWriterEndPI(XmlWriter_val(writer));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_flush(value writer)
{
	CAMLparam1(writer);
	int ret;

	ret = xmlTextWriterFlush(XmlWriter_val(writer));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_set_indent(value writer, value indent)
{
	CAMLparam2(writer, indent);
	int ret;

	ret = xmlTextWriterSetIndent(XmlWriter_val(writer),
				     Bool_val(indent));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_start_attribute(value writer, value name)
{
	int ret;
	CAMLparam2(writer, name);

	ret = xmlTextWriterStartAttribute(XmlWriter_val(writer),
					  BAD_CAST String_val(name));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_start_attribute_namespace(value writer, value prefix, value name,
			             value nsuri)
{
	CAMLparam4(writer, prefix, name, nsuri);
	int ret;

	ret = xmlTextWriterStartAttributeNS(XmlWriter_val(writer),
					    BAD_CAST String_val(prefix),
					    BAD_CAST String_val(name),
					    BAD_CAST String_val(nsuri));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_start_cdata(value writer)
{
	CAMLparam1(writer);
	int ret;

	ret = xmlTextWriterStartCDATA(XmlWriter_val(writer));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_start_comment(value writer)
{
	CAMLparam1(writer);
	int ret;

	ret = xmlTextWriterStartComment(XmlWriter_val(writer));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_start_dtd(value writer, value name, value pubid, value sysid)
{
	CAMLparam4(writer, name, pubid, sysid);
	int ret;

	ret = xmlTextWriterStartDTD(XmlWriter_val(writer),
				    BAD_CAST String_val(name),
				    BAD_CAST String_val(pubid),
				    BAD_CAST String_val(sysid));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_start_dtd_attlist(value writer, value name)
{
	CAMLparam2(writer, name);
	int ret;

	ret = xmlTextWriterStartDTDAttlist(XmlWriter_val(writer),
					   BAD_CAST String_val(name));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_start_dtd_element(value writer, value name)
{
	CAMLparam2(writer, name);
	int ret;

	ret = xmlTextWriterStartDTDElement(XmlWriter_val(writer),
					   BAD_CAST String_val(name));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_start_dtd_entity(value writer, value pe, value name)
{
	CAMLparam3(writer, pe, name);
	int ret;

	ret = xmlTextWriterStartDTDEntity(XmlWriter_val(writer),
					  Bool_val(pe),
					  BAD_CAST String_val(name));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_start_document(value writer, value version, value encoding,
			  value standalone)
{
	CAMLparam4(writer, version, encoding, standalone);
	int ret;

	ret = xmlTextWriterStartDocument(XmlWriter_val(writer),
					 String_val(version),
					 String_val(encoding),
					 String_val(standalone));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_start_element(value writer, value name)
{
	CAMLparam2(writer, name);
	int ret;

	ret = xmlTextWriterStartElement(XmlWriter_val(writer),
					BAD_CAST String_val(name));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_start_element_namespace(value writer, value prefix, value name,
				   value nsuri)
{
	CAMLparam4(writer, prefix, name, nsuri);
	int ret;

	ret = xmlTextWriterStartElementNS(XmlWriter_val(writer),
					  BAD_CAST String_val(prefix),
					  BAD_CAST String_val(name),
					  BAD_CAST String_val(nsuri));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_start_processing_instruction(value writer, value target)
{
	CAMLparam2(writer, target);
	int ret;

	ret = xmlTextWriterStartPI(XmlWriter_val(writer),
				   BAD_CAST String_val(target));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_write_attribute(value writer, value name, value content)
{
	CAMLparam3(writer, name, content);
	int ret;

	ret = xmlTextWriterWriteAttribute(XmlWriter_val(writer),
					  BAD_CAST String_val(name),
					  BAD_CAST String_val(content));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_write_attribute_namespace(value writer, value prefix, value name,
				     value nsuri, value content)
{
	CAMLparam5(writer, prefix, name, nsuri, content);
	int ret;

	ret = xmlTextWriterWriteAttributeNS(XmlWriter_val(writer),
					    BAD_CAST String_val(prefix),
					    BAD_CAST String_val(name),
					    BAD_CAST String_val(nsuri),
					    BAD_CAST String_val(content));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_write_base64(value writer, value data, value start, value len)
{
	CAMLparam4(writer, data, start, len);
	int ret;

	ret = xmlTextWriterWriteBase64(XmlWriter_val(writer),
				       String_val(data),
				       Int_val(start),
				       Int_val(len));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_write_binhex(value writer, value data, value start, value len)
{
	CAMLparam4(writer, data, start, len);
	int ret;

	ret = xmlTextWriterWriteBinHex(XmlWriter_val(writer),
				       String_val(data),
				       Int_val(start),
				       Int_val(len));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_write_cdata(value writer, value content)
{
	CAMLparam2(writer, content);
	int ret;

	ret = xmlTextWriterWriteCDATA(XmlWriter_val(writer),
				      BAD_CAST String_val(content));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_write_comment(value writer, value content)
{
	CAMLparam2(writer, content);
	int ret;

	ret = xmlTextWriterWriteComment(XmlWriter_val(writer),
				        BAD_CAST String_val(content));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_write_dtd(value writer, value name, value pubid, value sysid,
		     value subset)
{
	CAMLparam5(writer, name, pubid, sysid, subset);
	int ret;

	ret = xmlTextWriterWriteDTD(XmlWriter_val(writer),
				    BAD_CAST String_val(name),
				    BAD_CAST String_val(pubid),
				    BAD_CAST String_val(sysid),
				    BAD_CAST String_val(subset));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_write_dtd_attlist(value writer, value name, value content)
{
	CAMLparam3(writer, name, content);
	int ret;

	ret = xmlTextWriterWriteDTDAttlist(XmlWriter_val(writer),
					   BAD_CAST String_val(name),
					   BAD_CAST String_val(content));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_write_dtd_element(value writer, value name, value content)
{
	CAMLparam3(writer, name, content);
	int ret;

	ret = xmlTextWriterWriteDTDElement(XmlWriter_val(writer),
					   BAD_CAST String_val(name),
					   BAD_CAST String_val(content));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_write_dtd_external_entity(value writer, value pe, value name,
				     value pubid, value sysid, value ndatid)
{
	CAMLparam5(writer, pe, name, pubid, sysid);
	CAMLxparam1(ndatid);
	int ret;

	ret = xmlTextWriterWriteDTDExternalEntity(XmlWriter_val(writer),
						  Bool_val(pe),
						  BAD_CAST String_val(name),
						  BAD_CAST String_val(pubid),
						  BAD_CAST String_val(sysid),
						  BAD_CAST String_val(ndatid));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_write_dtd_external_entity_contents(value writer, value pubid,
					      value sysid, value ndataid)
{
	CAMLparam4(writer, pubid, sysid, ndataid);
	int ret;

	ret = xmlTextWriterWriteDTDExternalEntityContents(
		XmlWriter_val(writer),
		BAD_CAST String_val(pubid),
		BAD_CAST String_val(sysid),
		BAD_CAST String_val(ndataid));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_write_dtd_internal_entity(value writer, value pe, value name,
				     value content)
{
	CAMLparam4(writer, pe, name, content);
	int ret;

	ret = xmlTextWriterWriteDTDInternalEntity(XmlWriter_val(writer),
						  Bool_val(pe),
						  BAD_CAST String_val(name),
						 BAD_CAST String_val(content));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_write_dtd_notation(value writer, value name, value pubid,
			      value sysid)
{
	CAMLparam4(writer, name, pubid, sysid);
	int ret;

	ret = xmlTextWriterWriteDTDNotation(XmlWriter_val(writer),
					    BAD_CAST String_val(name),
					    BAD_CAST String_val(pubid),
					    BAD_CAST String_val(sysid));
	CAMLreturn(Val_unit);
}




CAMLprim value
xml_writer_write_element(value writer, value name, value content)
{
	CAMLparam3(writer, name, content);
	int ret;

	ret = xmlTextWriterWriteElement(XmlWriter_val(writer),
					BAD_CAST String_val(name),
					BAD_CAST String_val(content));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_write_element_namespace(value writer, value prefix, value name,
				   value nsuri, value content)
{
	CAMLparam5(writer, prefix, name, nsuri, content);
	int ret;

	ret = xmlTextWriterWriteElementNS(XmlWriter_val(writer),
					  BAD_CAST String_val(prefix),
					  BAD_CAST String_val(name),
					  BAD_CAST String_val(nsuri),
					  BAD_CAST String_val(content));
	CAMLreturn(Val_unit);
}




/* We do not bind all the format-string based versions. */





CAMLprim value
xml_writer_write_processing_instruction(value writer, value target,
					value content)
{
	CAMLparam3(writer, target, content);
	int ret;

	ret = xmlTextWriterWritePI(XmlWriter_val(writer),
				   BAD_CAST String_val(target),
				   BAD_CAST String_val(content));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_write_raw(value writer, value content)
{
	CAMLparam2(writer, content);
	int ret;

	ret = xmlTextWriterWriteRaw(XmlWriter_val(writer),
				    BAD_CAST String_val(content));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_write_raw_len(value writer, value content, value len)
{
	CAMLparam3(writer, content, len);
	int ret;

	ret = xmlTextWriterWriteRawLen(XmlWriter_val(writer),
				       BAD_CAST String_val(content),
				       Int_val(len));
	CAMLreturn(Val_unit);
}





CAMLprim value
xml_writer_write_string(value writer, value content)
{
	CAMLparam2(writer, content);
	int ret;

	ret = xmlTextWriterWriteString(XmlWriter_val(writer),
				       BAD_CAST String_val(content));
	CAMLreturn(Val_unit);
}






/* skip the va list versions of the functions. */
